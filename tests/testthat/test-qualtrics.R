qualtrics_files <- function() {
  list(
    responses = test_path("fixtures", "qualtrics", "responses.csv"),
    qsf = test_path("fixtures", "qualtrics", "survey.qsf")
  )
}

qualtrics_read <- function(...) {
  files <- qualtrics_files()
  read_qualtrics(
    files$responses,
    files$qsf,
    column_types = c(id = "integer", QID1 = "number"),
    ...
  )
}

qualtrics_changed_csv <- function(change) {
  d <- .q_csv(qualtrics_files()$responses)$data
  d <- change(d)
  names(d)[names(d) == "response_id"] <- "_recordId"
  path <- tempfile(fileext = ".csv")
  utils::write.csv(d, path, row.names = FALSE, na = "")
  path
}

qualtrics_changed_qsf <- function(change) {
  q <- jsonlite::read_json(qualtrics_files()$qsf)
  path <- tempfile(fileext = ".qsf")
  jsonlite::write_json(change(q), path, auto_unbox = TRUE)
  path
}

qualtrics_roster <- function() {
  studyframe(
    data.frame(id = 1:5, offer = c(0L, 1L, 0L, 1L, 0L)),
    "id",
    "person",
    "fixture_roster",
    assignments = list(assignment_spec(
      "offer",
      "offer",
      "id",
      mechanism = "complete",
      prob = 0.4,
      source = "Synthetic realized complete randomization",
      evidence = "assignment"
    )),
    roster = roster_spec(
      "randomized_units",
      "Synthetic complete roster",
      "asserted_complete",
      "offer",
      "assignment"
    ),
    evidence = list(evidence_spec("assignment", "generated:fixture-assignment"))
  )
}

test_that("three-row headers map stable IDs, numeric recodes and form fields", {
  x <- qualtrics_read()
  expect_equal(x$id, c(2L, 4L, 1L, 3L))
  expect_equal(x$QID1, c(12, NA, 10, 15))
  expect_equal(x$QID2, c(1, 0, 0, 1))
  expect_equal(x$finished, c(TRUE, FALSE, TRUE, TRUE))
  ctx <- context(x)
  expect_equal(ctx$roster$kind, "received_responses")
  expect_equal(ctx$roster$completeness, "unknown")
  variables <- stats::setNames(
    ctx$variables,
    vapply(ctx$variables, function(v) v$id, character(1))
  )
  expect_equal(variables$QID2$categories[[1]], list(value = 0, label = "No"))
  expect_equal(variables$QID3_2$instrument$subquestion, "2")
  expect_equal(variables$QID3_2$label, "Comments — Second item")
  expect_length(variables$QID3_2$missing, 0)
  expect_match(ctx$evidence[[1]]$sha256, "^[a-f0-9]{64}$")
  expect_true(all(
    c(
      "qualtrics_instrument_match_unknown",
      "qualtrics_randomization_unmapped"
    ) %in%
      vapply(ctx$diagnostics, function(d) d$code, character(1))
  ))
  expect_length(ctx$assignments, 0)
})

test_that("response-only imports preserve labels and values without inventing coding", {
  x <- read_qualtrics(qualtrics_files()$responses)
  expect_equal(x$QID1, c("12", NA, "10", "15"))
  expect_equal(x$QID2, c("1", "0", "0", "1"))
  vars <- context(x)$variables
  expect_true(all(vapply(vars, function(v) is.null(v$instrument), logical(1))))
  expect_true(all(vapply(
    vars,
    function(v) length(v$categories) == 0L,
    logical(1)
  )))
  expect_true(
    "qualtrics_instrument_missing" %in%
      vapply(context(x)$diagnostics, function(d) d$code, character(1))
  )
  expect_length(context(x)$evidence, 1)
  expect_equal(nrow(x), 4)
  unknown <- qualtrics_changed_csv(function(d) {
    d$status[1] <- "unrecognized"
    d
  })
  y <- read_qualtrics(unknown)
  expect_equal(nrow(y), 4)
  expect_true(
    "qualtrics_status_unrecognized" %in%
      vapply(context(y)$diagnostics, function(d) d$code, character(1))
  )
  channel <- qualtrics_changed_csv(function(d) {
    d$distributionChannel <- c("preview", "anonymous", "anonymous", "anonymous")
    d
  })
  expect_error(read_qualtrics(channel), "explicit policy")
  expect_equal(nrow(read_qualtrics(channel, preview = "exclude")), 3)
})

test_that("instrument identity and numeric coding mismatches fail", {
  f <- qualtrics_files()
  expect_error(
    qualtrics_read(expected_qsf_sha256 = paste(rep("0", 64), collapse = "")),
    "hash differs"
  )
  expect_error(qualtrics_read(expected_survey_id = "SV_other"), "survey ID")
  x <- qualtrics_read(
    expected_qsf_sha256 = .file_hash(f$qsf),
    instrument_version = "collection-v1"
  )
  expect_equal(context(x)$evidence[[1]]$version, "collection-v1")
  expect_false(
    "qualtrics_instrument_match_unknown" %in%
      vapply(context(x)$diagnostics, function(d) d$code, character(1))
  )
  changed <- qualtrics_changed_qsf(function(q) {
    q$SurveyElements[[2]]$Payload$RecodeValues <- list(`1` = "3", `2` = "4")
    q
  })
  expect_error(read_qualtrics(f$responses, changed), "Response codes disagree")
  expect_error(
    read_qualtrics(
      f$responses,
      changed,
      expected_qsf_sha256 = .file_hash(f$qsf)
    ),
    "hash differs"
  )
})

test_that("duplicate response IDs and repeat submissions are never resolved silently", {
  f <- qualtrics_files()
  duplicate <- qualtrics_changed_csv(function(d) {
    d$response_id[2] <- d$response_id[1]
    d
  })
  expect_error(read_qualtrics(duplicate, f$qsf), "Response IDs")
  repeated <- qualtrics_changed_csv(function(d) {
    d$id[2] <- d$id[1]
    d
  })
  expect_error(
    read_qualtrics(
      repeated,
      f$qsf,
      roster = qualtrics_roster(),
      respondent_key = "id",
      column_types = c(id = "integer")
    ),
    "repeat submissions"
  )
})

test_that("preview and test policies are explicit and incomplete field records remain", {
  f <- qualtrics_files()
  for (status in c("1", "2", "17", "256", "Survey Preview", "Survey Test")) {
    path <- qualtrics_changed_csv(function(d) {
      d$status[1] <- status
      d
    })
    expect_error(read_qualtrics(path, f$qsf), "explicit policy")
    kept <- read_qualtrics(path, f$qsf, preview = "include")
    excluded <- read_qualtrics(path, f$qsf, preview = "exclude")
    expect_equal(nrow(kept), 4)
    expect_equal(nrow(excluded), 3)
    expect_false(excluded$finished[excluded$response_id == "R_4"])
  }
})

test_that("unsupported questions and ambiguous header mappings fail", {
  f <- qualtrics_files()
  unsupported <- qualtrics_changed_qsf(function(q) {
    q$SurveyElements[[2]]$Payload$QuestionType <- "Matrix"
    q
  })
  expect_error(read_qualtrics(f$responses, unsupported), "Unsupported question")
  multiselect <- qualtrics_changed_qsf(function(q) {
    q$SurveyElements[[2]]$Payload$Selector <- "MAVR"
    q
  })
  expect_error(read_qualtrics(f$responses, multiselect), "Unsupported question")
  unknown <- qualtrics_changed_csv(function(d) {
    names(d)[names(d) == "QID1"] <- "QID99"
    d
  })
  expect_error(read_qualtrics(unknown, f$qsf), "missing from QSF")
  no_recode <- qualtrics_changed_qsf(function(q) {
    q$SurveyElements[[2]]$Payload$RecodeValues <- NULL
    q
  })
  expect_error(read_qualtrics(f$responses, no_recode), "RecodeValues")
  malformed <- tempfile(fileext = ".csv")
  raw <- readLines(f$responses)
  raw[3] <- sub("ImportId", "WrongId", raw[3], fixed = TRUE)
  writeLines(raw, malformed)
  expect_error(read_qualtrics(malformed, f$qsf), "Every export column")
})

test_that("roster joins match keys, retain nonrespondents and forbid assignment overwrites", {
  roster <- qualtrics_roster()
  x <- qualtrics_read(roster = roster, respondent_key = "embeddedData-id")
  expect_equal(x$id, 1:5)
  expect_equal(x$QID1, c(10, 12, 15, NA, NA))
  expect_equal(x$offer, roster$offer)
  expect_equal(context(x)$roster, context(roster)$roster)
  expect_equal(context(x)$assignments, context(roster)$assignments)
  expect_equal(nrow(collection_data(x)), 5)
  expect_error(
    estimate_effect(x, "QID1", "offer"),
    "[Mm]issing|[Aa]ttrition|[Oo]bserved"
  )
  f <- qualtrics_files()
  overwrite <- qualtrics_changed_csv(function(d) {
    d$offer <- "1"
    d
  })
  expect_error(
    read_qualtrics(
      overwrite,
      f$qsf,
      roster = roster,
      respondent_key = "id",
      column_types = c(id = "integer")
    ),
    "overwrite"
  )
  outside <- qualtrics_changed_csv(function(d) {
    d$id[1] <- "999"
    d
  })
  expect_error(
    read_qualtrics(
      outside,
      f$qsf,
      roster = roster,
      respondent_key = "id",
      column_types = c(id = "integer")
    ),
    "absent"
  )
  expect_error(
    read_qualtrics(f$responses, f$qsf, roster = roster, respondent_key = "id"),
    "key types differ"
  )
})

test_that("synthetic collection reaches estimatr and a portable bundle", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("arrow")
  source(
    system.file("examples", "qualtrics_workflow.R", package = "studyframe"),
    local = TRUE
  )
  result <- run_qualtrics_workflow(tempfile("qualtrics-study-"))
  expect_s3_class(result$analysis$fit, "difference_in_means")
  expect_equal(nrow(result$study), 40)
  direct <- estimatr::difference_in_means(
    QID1 ~ offer,
    data = as.data.frame(result$study)
  )
  expect_equal(result$analysis$fit$coefficients, direct$coefficients)
  expect_equal(result$analysis$fit$std.error, direct$std.error)
  expect_equal(context(result$study)$roster$kind, "randomized_units")
})
test_that("instrument mismatch errors retain machine-readable import issues", {
  failed <- tryCatch(
    qualtrics_read(expected_survey_id = "SV_other"),
    studyframe_error = identity
  )
  expect_s3_class(failed, "studyframe_import_error")
  expect_identical(failed$issues[[1]]$code, "qualtrics_survey_mismatch")
  expect_identical(failed$issues[[1]]$category, "invalid")
  expect_identical(conditionCall(failed)[[1]], quote(read_qualtrics))
  expect_identical(
    jsonlite::fromJSON(.json(failed$issues), simplifyVector = FALSE),
    failed$issues
  )
})
