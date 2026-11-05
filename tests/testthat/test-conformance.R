test_that("independent-producer bundles retain structural and inference boundaries", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("zip")
  skip_if_not_installed("estimatr")
  cases <- jsonlite::read_json(system.file(
    "conformance",
    "cases.json",
    package = "studyframe"
  ))
  for (case in cases) {
    path <- test_path(
      "fixtures",
      "conformance",
      paste0(case$name, ".studyframe")
    )
    study <- tryCatch(read_studyframe(path), error = identity)
    expect_identical(!inherits(study, "error"), case$valid, info = case$name)
    if (!case$valid) {
      next
    }
    expect_identical(study$id, 1:8, info = case$name)
    expect_equal(collection_data(study), as.data.frame(study), info = case$name)
    request <- analysis_request("effect", "y", assignment = "trial")
    checked <- check_analysis(study, request)
    expect_identical(checked$ok, case$analysis_ok, info = case$name)
    if (case$analysis_ok) {
      expect_equal(
        unname(stats::coef(run_analysis(study, request)$fit)),
        4,
        info = case$name
      )
    } else {
      failed <- tryCatch(
        run_analysis(study, request),
        studyframe_analysis_error = identity
      )
      expect_identical(failed$issues, checked$issues, info = case$name)
    }
  }
})
