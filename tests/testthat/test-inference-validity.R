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

test_that("zero-uncertainty means retain their finite point intervals", {
  skip_if_not_installed("survey")
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
})

test_that("paired effect uncertainty agrees with native and analytical results", {
  skip_if_not_installed("estimatr")
  original <- reference_workflows()$experiment
  ctx <- context(original)
  assignment <- ctx$assignments[[1]]
  assignment$unit <- "id"
  assignment$blocks <- "block"
  observations <- data.frame(
    id = 1:8,
    block = rep(1:4, each = 2),
    offer = rep(0:1, 4),
    outcome = c(1, 3, 2, 6, 4, 5, 8, 13)
  )
  paired <- studyframe(
    observations,
    "id",
    "person",
    "paired_effects",
    assignments = list(assignment),
    roster = ctx$roster,
    evidence = ctx$evidence
  )
  direct <- estimatr::difference_in_means(
    outcome ~ offer,
    data = observations,
    blocks = block,
    condition1 = 0,
    condition2 = 1
  )
  effect <- estimate_effect(paired, "outcome", assignment$id)
  expect_equal(unname(effect$fit$coefficients), 3)
  expect_equal(unname(effect$fit$std.error), sqrt(5 / 6))
  expect_equal(unname(effect$fit$df), 3)
  expect_equal(
    effect$fit[c(
      "coefficients",
      "std.error",
      "df",
      "conf.low",
      "conf.high",
      "p.value"
    )],
    direct[c(
      "coefficients",
      "std.error",
      "df",
      "conf.low",
      "conf.high",
      "p.value"
    )]
  )
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
