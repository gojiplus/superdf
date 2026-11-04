# Synthetic vendor-shaped collection; no external account or respondent data.
run_qualtrics_workflow <- function(directory, seed = 913L) {
  stopifnot(!dir.exists(directory))
  dir.create(directory, recursive = TRUE)
  set.seed(seed)
  roster_data <- data.frame(
    unit_id = seq_len(40L),
    offer = sample(rep(0:1, each = 20L))
  )
  roster_file <- file.path(directory, "assignment.csv")
  utils::write.csv(roster_data, roster_file, row.names = FALSE)
  assignment_hash <- digest::digest(file = roster_file, algo = "sha256")
  roster <- superdf(
    roster_data,
    "unit_id",
    "person",
    "synthetic_qualtrics_study",
    assignments = list(assignment_spec(
      "offer_assignment",
      "offer",
      "unit_id",
      mechanism = "complete",
      prob = 0.5,
      source = "Assign exactly 20 of 40 units by uniform shuffle",
      evidence = "assignment_file"
    )),
    roster = roster_spec(
      "randomized_units",
      "Full externally generated assignment roster",
      "asserted_complete",
      "offer_assignment",
      "assignment_file"
    ),
    evidence = list(evidence_spec(
      "assignment_file",
      normalizePath(roster_file),
      assignment_hash,
      producer = "SuperDF synthetic example",
      description = paste("Generated complete roster; seed", seed)
    ))
  )
  instrument <- list(
    SurveyEntry = list(
      SurveyID = "SV_SUPERDF_EXAMPLE",
      SurveyName = "Synthetic outcome collection"
    ),
    SurveyElements = list(list(
      Element = "SQ",
      PrimaryAttribute = "QID1",
      Payload = list(
        QuestionID = "QID1",
        QuestionType = "TE",
        Selector = "SL",
        QuestionText = "Outcome score"
      )
    ))
  )
  qsf_file <- file.path(directory, "instrument.qsf")
  jsonlite::write_json(instrument, qsf_file, auto_unbox = TRUE, pretty = TRUE)
  collection_hash <- digest::digest(file = qsf_file, algo = "sha256")
  responses <- data.frame(
    ResponseId = paste0("R_", roster_data$unit_id),
    unit_id = roster_data$unit_id,
    Status = 0L,
    Finished = 1L,
    QID1 = 10 + 2 * roster_data$offer + rnorm(40)
  )
  responses <- responses[sample.int(nrow(responses)), ]
  csv_file <- file.path(directory, "responses.csv")
  labels <- c(
    "Response ID",
    "Stable roster identifier",
    "Response type",
    "Finished",
    "Outcome score"
  )
  import_ids <- c(
    "_recordId",
    "embeddedData-unit_id",
    "status",
    "finished",
    "QID1"
  )
  metadata <- rbind(
    labels,
    vapply(
      import_ids,
      function(id) {
        as.character(jsonlite::toJSON(list(ImportId = id), auto_unbox = TRUE))
      },
      character(1)
    )
  )
  export <- as.data.frame(
    rbind(metadata, as.matrix(responses)),
    stringsAsFactors = FALSE
  )
  names(export) <- names(responses)
  utils::write.csv(export, csv_file, row.names = FALSE, na = "")
  study <- read_qualtrics(
    csv_file,
    qsf_file,
    roster = roster,
    respondent_key = "unit_id",
    column_types = c(unit_id = "integer", QID1 = "number"),
    expected_qsf_sha256 = collection_hash,
    expected_survey_id = "SV_SUPERDF_EXAMPLE",
    instrument_version = "synthetic-collection-v1"
  )
  bundle <- file.path(directory, "study.superdf")
  write_superdf(study, bundle)
  received <- read_superdf(bundle)
  write_codebook(received, file.path(directory, "codebook.md"))
  result <- estimate_effect(received, "QID1", "offer_assignment")
  saveRDS(result, file.path(directory, "analysis.rds"))
  list(study = received, analysis = result, bundle = bundle, truth = 2)
}
