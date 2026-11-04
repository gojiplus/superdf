test_that("undefined effect uncertainty is reported instead of a successful receipt", {
  skip_if_not_installed("estimatr")
  original <- reference_workflows()$experiment
  ctx <- context(original)
  assignment <- ctx$assignments[[1]]
  assignment$unit <- "id"
  assignment["blocks"] <- list(NULL)
  x <- studyframe(
    data.frame(id = 1:8, offer = rep(0:1, 4), outcome = rep(c(1, 5), 4)),
    "id",
    "person",
    "constant_arms",
    assignments = list(assignment),
    roster = ctx$roster,
    evidence = ctx$evidence
  )
  request <- analysis_request("effect", "outcome", assignment = assignment$id)
  expect_true(check_analysis(x, request)$ok)
  failed <- tryCatch(
    run_analysis(x, request),
    studyframe_analysis_error = identity
  )
  expect_s3_class(failed, "studyframe_analysis_error")
  expect_identical(failed$issues[[1]]$code, "inference_unavailable")
})

test_that("a Student t mean requires positive design degrees of freedom", {
  skip_if_not_installed("survey")
  x <- studyframe(
    data.frame(id = 1:4, y = c(1, 2, 4, 8), w = 1, r1 = 1, r2 = 1),
    "id",
    "person",
    "rank_one",
    weights = list(weight_spec(
      "main",
      "w",
      variance = variance_spec(
        "other",
        c("r1", "r2"),
        scale = 1,
        rscales = c(1, 1)
      )
    ))
  )
  request <- analysis_request("mean", "y", weight = "main")
  checked <- check_analysis(x, request)
  expect_false(checked$ok)
  expect_identical(checked$issues[[1]]$code, "design_df_insufficient")
  failed <- tryCatch(
    run_analysis(x, request),
    studyframe_analysis_error = identity
  )
  expect_s3_class(failed, "studyframe_analysis_error")
  expect_identical(failed$issues, checked$issues)
})

test_that("finite zero-uncertainty intervals retain supported backend results", {
  skip_if_not_installed("survey")
  skip_if_not_installed("estimatr")
  x <- studyframe(
    data.frame(id = 1:4, y = 3, w = 1),
    "id",
    "person",
    "constant_mean",
    sampling = list(sampling_spec(
      "s",
      stages = list(sampling_stage("id", method = "srs", replacement = "with"))
    )),
    weights = list(weight_spec("main", "w", sampling = "s"))
  )
  mean <- estimate_mean(x, "y", "main")
  expect_equal(unname(coef(mean$fit)), 3)
  expect_equal(as.numeric(survey::SE(mean$fit)), 0)
  expect_equal(
    unname(stats::confint(mean$fit, df = mean$analysis$design_df)),
    matrix(c(3, 3), 1)
  )
  original <- reference_workflows()$experiment
  ctx <- context(original)
  assignment <- ctx$assignments[[1]]
  assignment$unit <- "id"
  assignment$blocks <- "block"
  paired <- studyframe(
    data.frame(
      id = 1:8,
      block = rep(1:4, each = 2),
      offer = rep(0:1, 4),
      outcome = rep(c(1, 5), 4)
    ),
    "id",
    "person",
    "constant_pair_effects",
    assignments = list(assignment),
    roster = ctx$roster,
    evidence = ctx$evidence
  )
  effect <- estimate_effect(paired, "outcome", assignment$id)
  expect_equal(unname(effect$fit$coefficients), 4)
  expect_equal(unname(effect$fit$std.error), 0)
  expect_equal(unname(effect$fit$df), 3)
  expect_equal(c(effect$fit$conf.low, effect$fit$conf.high), c(4, 4))
})

test_that("finite observations cannot produce a successful nonfinite interval", {
  skip_if_not_installed("survey")
  x <- studyframe(
    data.frame(id = 1:4, y = c(-1e308, 1e308, -1e308, 1e308), w = 1),
    "id",
    "person",
    "overflowing_variance",
    sampling = list(sampling_spec(
      "s",
      stages = list(sampling_stage("id", method = "srs", replacement = "with"))
    )),
    weights = list(weight_spec("main", "w", sampling = "s"))
  )
  failed <- tryCatch(
    estimate_mean(x, "y", "main"),
    studyframe_analysis_error = identity
  )
  expect_s3_class(failed, "studyframe_analysis_error")
  expect_identical(failed$issues[[1]]$code, "inference_unavailable")
})
