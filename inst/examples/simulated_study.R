# Source after loading superdf. Potential outcomes stay outside the released data.
simulate_population <- function(seed = 812L) {
  set.seed(seed)
  school_effect <- rnorm(80, sd = 4)
  population <- data.frame(
    id = seq_len(320),
    school = rep(seq_len(80), each = 4),
    district = rep(1:2, each = 160)
  )
  population$baseline <- 50 +
    5 * population$district +
    school_effect[population$school] +
    rnorm(320, sd = 2)
  population$y0 <- population$baseline + rnorm(320, sd = 2)
  population$y1 <- population$y0 + 2
  population
}

collect_study <- function(population, seed = 9001L) {
  set.seed(seed)
  frame <- unique(population[c("school", "district")])
  selected <- unlist(lapply(
    split(frame$school, frame$district),
    sample,
    size = 12
  ))
  sampled <- population[population$school %in% selected, ]
  treated <- unlist(lapply(
    split(
      unique(sampled[c("school", "district")])$school,
      unique(sampled[c("school", "district")])$district
    ),
    sample,
    size = 6
  ))
  roster <- sampled[c("id", "school", "district")]
  roster$offer <- as.integer(roster$school %in% treated)
  roster$school_population <- 40L
  roster$inclusion_probability <- 12 / 40
  roster$weight <- 1 / roster$inclusion_probability
  study <- superdf(
    roster,
    "id",
    "pupil",
    paste0("simulated_school_study_", seed),
    notes = paste("Collection seed", seed),
    roster = roster_spec(
      "randomized_units",
      source = "Generated assignment roster before outcome collection",
      completeness = "asserted_complete",
      scope = "school_offer",
      evidence = "assignment_roster"
    ),
    evidence = list(evidence_spec(
      "assignment_roster",
      "generated:assignment-roster",
      producer = "SuperDF simulation",
      description = "Full generated randomized roster; completeness asserted by generator"
    )),
    sampling = list(sampling_spec(
      "school_sample",
      "320 pupils in 80 schools across two districts",
      list(sampling_stage(
        "school",
        strata = "district",
        method = "srs",
        replacement = "without",
        probability = "inclusion_probability",
        fpc = "school_population"
      )),
      source = "Sample 12 of 40 schools per district; observe every pupil in selected schools"
    )),
    weights = list(weight_spec(
      "sample_weight",
      "weight",
      sampling = "school_sample",
      role = "base",
      waves = "baseline",
      source = "Inverse school inclusion probability"
    )),
    assignments = list(assignment_spec(
      "school_offer",
      "offer",
      "school",
      blocks = "district",
      mechanism = "complete",
      prob = 0.5,
      source = "Randomly assign six of twelve sampled schools per district"
    ))
  )
  observed <- data.frame(
    id = sampled$id,
    baseline = sampled$baseline,
    followup = ifelse(roster$offer == 1L, sampled$y1, sampled$y0)
  )
  observed <- observed[sample.int(nrow(observed)), ]
  join_superdf(
    study,
    observed,
    "id",
    list(
      variable_spec(
        "baseline",
        units = "score points",
        wave = "baseline",
        source = "Baseline instrument",
        status = "documented"
      ),
      variable_spec(
        "followup",
        units = "score points",
        wave = "followup",
        source = "Follow-up instrument",
        status = "documented"
      )
    ),
    source = "Simulated complete collection; records arrive in shuffled order"
  )
}

analyze_study <- function(study) {
  baseline <- estimate_mean(study, "baseline", "sample_weight")
  effect <- estimate_effect(study, "followup", "school_offer")
  baseline_ci <- confint(baseline$fit, df = baseline$analysis$design_df)
  table <- data.frame(
    estimand = c("population_baseline_mean", "randomized_roster_effect"),
    estimate = c(unname(coef(baseline$fit)), unname(effect$fit$coefficients)),
    se = c(unname(survey::SE(baseline$fit)), unname(effect$fit$std.error)),
    lower = c(baseline_ci[1, 1], unname(effect$fit$conf.low)),
    upper = c(baseline_ci[1, 2], unname(effect$fit$conf.high))
  )
  list(table = table, baseline = baseline, effect = effect)
}

run_simulated_study <- function(directory, repetitions = 500L) {
  stopifnot(
    length(repetitions) == 1L,
    repetitions >= 2L,
    repetitions == as.integer(repetitions),
    !dir.exists(directory)
  )
  dir.create(directory, recursive = TRUE)
  population <- simulate_population()
  study <- collect_study(population)
  bundle <- file.path(directory, "collected.superdf")
  write_superdf(study, bundle)
  received <- read_superdf(bundle)
  write_codebook(received, file.path(directory, "codebook.md"))
  example <- analyze_study(received)
  truth <- c(mean(population$baseline), 2)
  example$table$truth <- truth
  utils::write.csv(
    example$table,
    file.path(directory, "single_run.csv"),
    row.names = FALSE
  )
  saveRDS(example, file.path(directory, "analysis.rds"))
  draws <- lapply(seq_len(repetitions), function(i) {
    collected <- collect_study(population, seed = 10000L + i)
    path <- tempfile(fileext = ".superdf")
    on.exit(unlink(path), add = TRUE)
    write_superdf(collected, path)
    result <- analyze_study(read_superdf(path))$table
    result$truth <- truth
    result$replication <- i
    result$covered <- result$lower <= truth & result$upper >= truth
    result
  })
  draws <- do.call(rbind, draws)
  summary <- do.call(
    rbind,
    lapply(split(draws, draws$estimand), function(d) {
      error <- d$estimate - d$truth
      coverage <- mean(d$covered)
      data.frame(
        estimand = d$estimand[[1]],
        repetitions = nrow(d),
        truth = d$truth[[1]],
        mean_estimate = mean(d$estimate),
        bias = mean(error),
        bias_mcse = sd(error) / sqrt(nrow(d)),
        empirical_sd = sd(d$estimate),
        rms_se = sqrt(mean(d$se^2)),
        coverage = coverage,
        coverage_mcse = sqrt(coverage * (1 - coverage) / nrow(d))
      )
    })
  )
  rownames(summary) <- NULL
  utils::write.csv(
    draws,
    file.path(directory, "replications.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    summary,
    file.path(directory, "summary.csv"),
    row.names = FALSE
  )
  list(single_run = example$table, simulation = summary)
}
