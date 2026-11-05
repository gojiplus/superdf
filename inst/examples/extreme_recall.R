run_extreme_recall <- function(repo, output, seed = 1L) {
  repo <- normalizePath(repo, mustWork = TRUE)
  if (file.exists(output)) {
    stop("Choose a new output directory.")
  }
  dir.create(output, recursive = TRUE)
  output <- normalizePath(output, mustWork = TRUE)
  reproduced <- reproduce_recall(repo, seed) # nolint: object_usage_linter.
  hashes <- reproduced$flow$source_hashes
  evidence <- lapply(seq_along(hashes), function(i) {
    evidence_spec(
      paste0("source", i),
      names(hashes)[i],
      hashes[[i]],
      producer = "extreme_recall_2023 research repository",
      description = "Pinned source artifact; the hash identifies bytes, not validity."
    )
  })
  source_id <- function(path) paste0("source", match(path, names(hashes)))
  instrument <- source_id("data/lucid/extreme_recall.docx")
  cleaning <- source_id("scripts/00_survey_df_clean.R")
  results <- source_id("scripts/02_results_w2.R")
  raw <- read_qualtrics(
    file.path(repo, "data/lucid/er2_lucid_jan2023.csv"),
    dataset_id = "extreme_recall_lucid_responses",
    preview = "include"
  )
  imported_file <- file.path(output, "imported.studyframe")
  write_studyframe(raw, imported_file)
  evidence <- c(
    context(raw)$evidence,
    evidence,
    list(evidence_spec(
      "imported_bundle",
      "imported.studyframe",
      digest::digest(file = imported_file, algo = "sha256"),
      description = "Unmodified imported responses and their import diagnostics."
    ))
  )
  variables <- context(raw)$variables
  for (i in seq_along(variables)) {
    id <- variables[[i]]$id
    if (grepl("^Q[456]_1_[123]$", id)) {
      parts <- strsplit(id, "_", fixed = TRUE)[[1]]
      variables[[i]]$instrument <- instrument_ref(
        instrument,
        paste(parts[1:2], collapse = "."),
        parts[3]
      )
      variables[[i]]$evidence <- c(variables[[i]]$evidence, list(instrument))
    }
  }
  collected <- studyframe(
    as.data.frame(raw),
    "response_id",
    "survey response",
    "extreme_recall_lucid_collection",
    variables = variables,
    notes = paste(
      "Explicit metadata reconstruction from imported_bundle; raw values unchanged.",
      "The questionnaire documents planned even presentation of state/issue prompts;",
      "execution probabilities and a complete assignment roster are unavailable.",
      "Literal NA strings remain as exported; analysis recoding follows the pinned source."
    ),
    sampling = list(sampling_spec(
      "lucid_recruitment",
      "Lucid-recruited survey respondents",
      state = "unknown",
      status = "unknown",
      source = "No probability sampling frame or inclusion probabilities recovered.",
      evidence = instrument
    )),
    assignments = list(assignment_spec(
      "prompt_assignment",
      "experiment",
      "response_id",
      conditions = c("issue", "state"),
      mechanism = "unknown",
      phase = "planned",
      evidence = instrument,
      source = paste(
        "Questionnaire Survey Flow: BlockRandomizer, evenly present one element;",
        "embedded experiment values issue/state. This is not an execution log."
      )
    )),
    roster = roster_spec(
      "received_responses",
      "All rows of the pinned response export",
      evidence = "imported_bundle"
    ),
    evidence = evidence,
    diagnostics = context(raw)$diagnostics
  )
  pass <- stringr::str_squish(collected$Q4_1_1) == "Extremely interested" &
    stringr::str_squish(collected$Q4_1_2) == "Very interested"
  pass[is.na(pass)] <- FALSE
  collected <- derive_studyframe(
    collected,
    variable_spec(
      "attention_pass",
      label = "Both instructed attention-check answers match",
      source = "00_survey_df_clean.R:95-104",
      status = "documented",
      evidence = c(cleaning, instrument)
    ),
    pass,
    c("Q4_1_1", "Q4_1_2"),
    "Squish whitespace; retain Extremely interested on Q4.1_1 AND Very interested on Q4.1_2."
  )
  selected <- collected[pass, ]
  stopifnot(nrow(selected) == reproduced$flow$attention_pass)
  stopifnot(setequal(selected$response_id, reproduced$data$ResponseId))
  write_studyframe(collected, file.path(output, "collected.studyframe"))
  selected_file <- file.path(output, "selected.studyframe")
  write_studyframe(selected, selected_file)
  write_codebook(collected, file.path(output, "collection_codebook.md"))
  evidence <- c(
    evidence,
    list(evidence_spec(
      "selected_bundle",
      "selected.studyframe",
      digest::digest(file = selected_file, algo = "sha256"),
      description = paste(
        "Attention-pass view with all 904 received-response roster rows retained;",
        "analysis expands each retained response to six recall slots."
      )
    ))
  )
  d <- reproduced$data
  data <- data.frame(
    response_id = d$ResponseId,
    question = d$question_open,
    party = d$party_open_q,
    recall_position = as.integer(d$number),
    recall_order = d$number_inverse,
    extremity = d$recipient.cfscore.dyn_scaled_prop_comb
  )
  definition <- function(id, label, sources = c(cleaning, results)) {
    variable_spec(id, label = label, status = "documented", evidence = sources)
  }
  analysis <- studyframe(
    data,
    c("response_id", "question"),
    "respondent by party by recall slot",
    "extreme_recall_table3_model1",
    version = paste0("seed-", seed),
    notes = paste(
      "Derived from selected_bundle, preserving six slots per selected response.",
      "Reproduces the checked-in Table 3 model 1 under an explicit seed;",
      "not a claim of equality to an archived published coefficient.",
      "Coding uses source grouped length-two weighted permutations recycled within groups.",
      "Intervals condition on name coding, DIME linkage, sample selection, and rescaling.",
      "Original proportional scores include wrong-party recalls; no silent same-party restriction.",
      paste(
        "Observed collection:",
        reproduced$flow$collection_start,
        "to",
        reproduced$flow$collection_end
      )
    ),
    variables = list(
      definition(
        "response_id",
        "Source response identifier; dependence cluster"
      ),
      definition(
        "question",
        "Q5.1_1..3 Democratic or Q6.1_1..3 Republican recall slot"
      ),
      definition("party", "Party named in the recall prompt"),
      definition(
        "recall_position",
        "Position in the three response slots: 1, 2, 3"
      ),
      definition(
        "recall_order",
        "Original number_inverse: (4 - recall_position) / 3"
      ),
      definition(
        "extremity",
        paste(
          "Original proportional-coded dynamic CF score, absolute value for Democratic prompts,",
          "pooled min-max rescaled to 0-1 over available selected recall slots.",
          "NA means no usable source-derived score; not zero extremity."
        )
      )
    ),
    roster = roster_spec(
      "received_responses",
      "Derived recall slots for attention-pass responses; not a randomized or population roster.",
      evidence = "selected_bundle"
    ),
    evidence = evidence,
    diagnostics = list(analysis_issue(
      "measurement_uncertainty_unpropagated",
      "choice",
      "extremity",
      "Regression uncertainty conditions on source coding and linked ideology scores.",
      c(cleaning, results),
      "Use coding sensitivity or a measurement-error model for broader uncertainty."
    ))
  )
  request <- regression_request(
    "extremity",
    "recall_order",
    cluster = "response_id",
    se_type = "stata",
    missing = "omit",
    target = paste(
      "Linear association between reverse-scaled recall position and coded ideological",
      "extremity among score-observed recall slots of attention-pass Lucid respondents."
    ),
    assumptions = c(
      "Respondents are independent clusters; dependence within each respondent is unrestricted.",
      "The fitted linear association and clustered large-sample approximation are adequate for this selected sample.",
      "Complete-case omission restricts the target; it does not establish ignorable missingness.",
      "Name coding, linked scores, and pooled rescaling are treated as fixed; their uncertainty is not propagated.",
      "No causal interpretation or extrapolation to a probability-sampled population is asserted."
    )
  )
  bundle <- file.path(output, "analysis.studyframe")
  write_studyframe(analysis, bundle)
  received <- read_studyframe(bundle)
  stopifnot(identical(as.data.frame(analysis), as.data.frame(received)))
  stopifnot(identical(collection_data(analysis), collection_data(received)))
  stopifnot(identical(context(analysis), context(received)))
  check <- check_analysis(received, request)
  stopifnot(check$ok)
  result <- run_analysis(received, request)
  stopifnot(result$analysis$used_rows == reproduced$flow$model_observations)
  stopifnot(result$analysis$cluster_count == reproduced$flow$model_clusters)
  stopifnot(result$analysis$missing_rows == reproduced$flow$missing_outcome)
  expected <- reproduced$estimatr_stata
  for (field in c(
    "coefficients",
    "std.error",
    "p.value",
    "conf.low",
    "conf.high",
    "df"
  )) {
    stopifnot(isTRUE(all.equal(
      unname(result$fit[[field]]),
      unname(expected[[field]]),
      tolerance = 1e-10
    )))
  }
  stopifnot(isTRUE(all.equal(
    unname(coef(result$fit)),
    unname(coef(reproduced$baseline)),
    tolerance = 1e-10
  )))
  stopifnot(isTRUE(all.equal(
    as.numeric(result$fit$std.error),
    as.numeric(fixest::se(reproduced$baseline)),
    tolerance = 1e-10
  )))
  unsupported <- check_analysis(
    selected,
    analysis_request(
      "effect",
      "attention_pass",
      assignment = "prompt_assignment"
    )
  )
  stopifnot(!unsupported$ok)
  write_codebook(received, file.path(output, "analysis_codebook.md"))
  write_json <- function(value, name) {
    jsonlite::write_json(
      value,
      file.path(output, name),
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null",
      digits = NA
    )
  }
  write_json(request, "request.json")
  write_json(result$analysis, "receipt.json")
  write_json(reproduced$flow, "flow.json")
  write_json(reproduced$joins, "joins.json")
  write_json(reproduced$diagnostics, "measurement.json")
  write_json(reproduced$warnings, "source_warnings.json")
  sensitivity <- reproduced$same_party_sensitivity
  write_json(
    list(
      definition = "Same-party scored recalls only, holding source scaling and seed fixed",
      observations = stats::nobs(sensitivity),
      coefficients = as.list(stats::coef(sensitivity)),
      standard_errors = as.list(as.numeric(fixest::se(sensitivity)))
    ),
    "same_party_sensitivity.json"
  )
  write_json(unsupported, "causal_requirements.json")
  utils::write.csv(
    data.frame(
      term = names(coef(result$fit)),
      estimate = unname(coef(result$fit)),
      standard_error = result$fit$std.error,
      conf_low = result$fit$conf.low,
      conf_high = result$fit$conf.high,
      df = result$fit$df
    ),
    file.path(output, "coefficients.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    as.data.frame(table(
      attention_pass = pass,
      prompt = collected$experiment
    )),
    file.path(output, "selection.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    as.data.frame(table(
      recall_position = data$recall_position,
      recall_order = data$recall_order
    )),
    file.path(output, "recodes.csv"),
    row.names = FALSE
  )
  writeLines(capture.output(sessionInfo()), file.path(output, "session.txt"))
  invisible(list(
    bundle = bundle,
    request = request,
    result = result,
    flow = reproduced$flow
  ))
}
