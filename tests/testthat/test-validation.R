test_that("consistency failures identify the caller and share a condition class", {
  x <- studyframe(data.frame(id = 1:3), "id", "person", "validation")
  failed <- tryCatch(
    rename_studyframe(x, c(id = "")),
    studyframe_error = identity
  )
  expect_s3_class(failed, "studyframe_validation_error")
  expect_identical(conditionCall(failed)[[1]], quote(rename_studyframe))
  expect_match(conditionMessage(failed), "nonempty destinations", fixed = TRUE)
  expect_identical(names(x), "id")
})

test_that("derivation descriptions and join sources must be nonempty strings", {
  x <- studyframe(data.frame(id = 1:3), "id", "person", "validation")
  for (value in list(NULL, NA_character_, character(), c("a", "b"), 1, " ")) {
    derived <- tryCatch(
      derive_studyframe(x, variable_spec("y"), 1:3, "id", value),
      studyframe_validation_error = identity
    )
    joined <- tryCatch(
      join_studyframe(
        x,
        data.frame(id = 1:3, y = 4:6),
        "id",
        list(variable_spec("y")),
        value
      ),
      studyframe_validation_error = identity
    )
    expect_s3_class(derived, "studyframe_validation_error")
    expect_s3_class(joined, "studyframe_validation_error")
  }
})

test_that("analysis errors retain literal issue data without evaluating message markup", {
  issue <- analysis_issue(
    "missing_evidence",
    "missing",
    "sampling",
    "Missing {collection_protocol}.",
    sources = "protocol",
    remedy = "Attach {.file protocol.json}."
  )
  failed <- tryCatch(.analysis_abort(issue), studyframe_error = identity)
  expect_s3_class(failed, "studyframe_analysis_error")
  expect_identical(failed$issues, list(issue))
  expect_match(
    conditionMessage(failed),
    "Missing {collection_protocol}.",
    fixed = TRUE
  )
  expect_identical(
    jsonlite::fromJSON(.json(failed$issues), simplifyVector = FALSE),
    list(issue)
  )
})

test_that("policy errors show the public argument and reject abbreviations", {
  expect_snapshot(error = TRUE, read_qualtrics("unused.csv", preview = "ex"))
})
