# Deterministic fixtures: collection facts are explicit and outcomes do not drive design.
reference_workflows <- function() {
  survey_data <- data.frame(
    id = 1:24,
    district = rep(1:2, each = 12),
    school = rep(1:8, each = 3),
    weight = rep(c(2, 3), each = 12),
    income = c(11:22, 31:42),
    subgroup = rep(c(TRUE, FALSE, TRUE), 8)
  )
  sampled <- superdf(
    survey_data,
    keys = "id",
    unit = "pupil",
    dataset_id = "stratified_sample",
    variables = list(variable_spec(
      "income",
      label = "Monthly income",
      units = "USD",
      source = "Synthetic instrument",
      status = "documented"
    )),
    sampling = list(sampling_spec(
      "schools",
      "Pupils in two districts",
      list(sampling_stage(
        "school",
        method = "srs",
        strata = "district",
        replacement = "with"
      )),
      source = "Synthetic sampling protocol"
    )),
    weights = list(weight_spec(
      "main",
      "weight",
      sampling = "schools",
      source = "Synthetic design weights"
    ))
  )

  replicated <- data.frame(
    id = 1:8,
    income = c(2, 4, 8, 10, 12, 20, 22, 30),
    weight = rep(2, 8)
  )
  for (i in 1:8) {
    replicated[[paste0("rep", i)]] <- rep(2 * 8 / 7, 8)
    replicated[[paste0("rep", i)]][i] <- 0
  }
  replicated <- superdf(
    replicated,
    keys = "id",
    unit = "person",
    dataset_id = "replicate_release",
    weights = list(weight_spec(
      "main",
      "weight",
      role = "final",
      source = "Synthetic delete-one release",
      variance = variance_spec(
        "other",
        replicates = paste0("rep", 1:8),
        scale = 7 / 8,
        rscales = rep(1, 8),
        mse = TRUE
      )
    ))
  )

  experiment <- data.frame(
    id = 1:32,
    district = rep(1:2, each = 16),
    school = rep(1:8, each = 4),
    offer = rep(rep(c(0L, 1L), 4), each = 4)
  )
  experiment <- superdf(
    experiment,
    keys = "id",
    unit = "pupil",
    dataset_id = "blocked_experiment",
    assignments = list(assignment_spec(
      "offer_assignment",
      "offer",
      "school",
      blocks = "district",
      mechanism = "complete",
      prob = 0.5,
      source = "Synthetic assignment protocol"
    ))
  )

  outcomes <- data.frame(
    id = 1:32,
    outcome = seq_len(32) + rep(rep(c(0, 2), 4), each = 4)
  )
  experiment <- join_superdf(
    experiment,
    outcomes,
    by = "id",
    variables = list(variable_spec("outcome", label = "Follow-up outcome")),
    source = "Synthetic follow-up collection"
  )

  waves <- data.frame(
    id = 1:12,
    pre = seq_len(12) * 2,
    post = c(seq_len(10) * 3, NA, NA),
    completed = c(rep(TRUE, 10), FALSE, FALSE),
    pre_weight = rep(2, 12),
    post_weight = c(rep(2.4, 10), NA, NA)
  )
  waves <- superdf(
    waves,
    keys = "id",
    unit = "person",
    dataset_id = "two_wave_release",
    variables = list(
      variable_spec("pre", wave = "pre"),
      variable_spec("post", wave = "post")
    ),
    sampling = list(sampling_spec(
      "people",
      "Study population",
      list(sampling_stage("id", method = "srs", replacement = "with")),
      source = "Synthetic sampling protocol"
    )),
    weights = list(
      weight_spec(
        "pre",
        "pre_weight",
        sampling = "people",
        waves = "pre",
        source = "Synthetic release guide"
      ),
      weight_spec(
        "post",
        "post_weight",
        sampling = "people",
        role = "final",
        waves = "post",
        applies = list(list(variable = "completed", values = list(TRUE))),
        source = "Synthetic release guide"
      )
    )
  )
  list(
    survey = sampled,
    replicates = replicated,
    experiment = experiment,
    waves = waves
  )
}

write_example_bundles <- function(directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  examples <- reference_workflows()
  for (name in names(examples)) {
    write_superdf(
      examples[[name]],
      file.path(directory, paste0(name, ".superdf"))
    )
  }
  invisible(examples)
}
