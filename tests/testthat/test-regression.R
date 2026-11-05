regression_study <- function() {
  data <- data.frame(
    id = 1:24,
    respondent = rep(1:8, each = 3),
    order = rep(1:3, 8),
    y = c(
      3,
      6,
      4,
      5,
      4,
      9,
      2,
      8,
      7,
      6,
      2,
      3,
      8,
      5,
      9,
      4,
      3,
      8,
      2,
      7,
      6,
      6,
      5,
      7
    )
  )
  studyframe(
    data,
    "id",
    "recall",
    "recall",
    roster = roster_spec("received_responses")
  )
}

recall_request <- function(cluster = NULL, se_type = NULL, missing = "fail") {
  regression_request(
    "y",
    "order",
    cluster,
    se_type,
    missing,
    target = "Linear recall-order association among the observed recalls",
    assumptions = c(
      "Respondents are independent; dependence within respondent is allowed when clustered.",
      "Selection into the observed sample limits generalization."
    )
  )
}

test_that("regression estimates and uncertainty agree with the native backend", {
  skip_if_not_installed("estimatr")
  x <- regression_study()
  data <- as.data.frame(x)
  for (se in c("HC2", "CR2", "stata")) {
    cluster <- if (se == "HC2") NULL else "respondent"
    request <- recall_request(cluster, se)
    expected <- if (is.null(cluster)) {
      estimatr::lm_robust(y ~ order, data = data, se_type = se)
    } else {
      estimatr::lm_robust(
        y ~ order,
        data = data,
        clusters = respondent,
        se_type = se
      )
    }
    checked <- check_analysis(x, request)
    expect_true(checked$ok)
    actual <- run_analysis(x, request)
    fields <- c(
      "coefficients",
      "std.error",
      "df",
      "p.value",
      "conf.low",
      "conf.high"
    )
    expect_equal(actual$fit[fields], expected[fields])
    expect_identical(actual$analysis$input_digest, checked$input_digest)
    expect_identical(actual$analysis$request, request)
    expect_identical(actual$analysis$inference, "model_based_association")
    expect_identical(actual$analysis$model_assumptions, request$assumptions)
    expect_identical(actual$analysis$se_type, se)
    expect_identical(actual$analysis$roster$kind, "received_responses")
    expect_identical(actual$analysis$used_rows, 24L)
    expect_identical(
      actual$analysis$cluster_count,
      if (is.null(cluster)) NULL else 8L
    )
  }
  intercept <- regression_request(
    "y",
    target = "Observed mean",
    assumptions = "Independent rows"
  )
  expect_equal(unname(coef(run_analysis(x, intercept)$fit)), mean(x$y))
})

test_that("regression checks prepare the model without calling the estimator", {
  skip_if_not_installed("estimatr")
  local_mocked_bindings(
    lm_robust = function(...) stop("regression estimator was called"),
    .package = "estimatr"
  )
  expect_true(
    check_analysis(regression_study(), recall_request("respondent"))$ok
  )
})

test_that("regression requests cannot smuggle formulas or design declarations", {
  x <- regression_study()
  original <- recall_request("respondent")
  replace_fields <- function(request, changes) {
    request[names(changes)] <- changes
    request
  }
  empty_assumptions <- original
  empty_assumptions$assumptions <- list()
  bad <- list(
    replace_fields(original, list(predictors = list("order + respondent"))),
    replace_fields(original, list(predictors = list("order", "order"))),
    replace_fields(original, list(outcome = "system('false')")),
    replace_fields(original, list(predictors = list("y"))),
    replace_fields(original, list(target = " ")),
    empty_assumptions,
    replace_fields(original, list(assumptions = list(""))),
    replace_fields(original, list(weight = "main")),
    replace_fields(original, list(assignment = "randomized")),
    replace_fields(original, list(domain = TRUE)),
    replace_fields(original, list(se_type = "HC2")),
    replace_fields(recall_request(), list(se_type = "stata"))
  )
  for (request in bad) {
    checked <- check_analysis(x, request)
    expect_false(checked$ok)
    expect_identical(checked$issues[[1]]$code, "invalid_request")
  }
  request <- jsonlite::fromJSON(
    jsonlite::toJSON(original, auto_unbox = TRUE, null = "null"),
    simplifyVector = FALSE
  )
  expect_true(check_analysis(x, request)$ok)
})

test_that("regression omission and restricted views retain the collection history", {
  skip_if_not_installed("estimatr")
  data <- as.data.frame(regression_study())
  data$y[1] <- NA
  data$order[2] <- NA
  data$respondent[3] <- NA_integer_
  x <- studyframe(data, "id", "recall", "missing")
  view <- x[1:18, ]
  checked <- check_analysis(view, recall_request("respondent"))
  expect_identical(checked$issues[[1]]$code, "missing_model_values")
  request <- recall_request("respondent", missing = "omit")
  actual <- run_analysis(view, request)
  expected <- estimatr::lm_robust(
    y ~ order,
    data = data[4:18, ],
    clusters = respondent,
    se_type = "CR2"
  )
  expect_equal(coef(actual$fit), coef(expected))
  expect_equal(actual$fit$std.error, expected$std.error)
  expect_identical(actual$analysis$roster_rows, 24L)
  expect_identical(actual$analysis$current_rows, 18L)
  expect_identical(actual$analysis$used_rows, 15L)
  expect_identical(actual$analysis$missing_rows, 3L)
  expect_identical(actual$analysis$cluster_count, 5L)
  expect_identical(actual$analysis$history, context(view)$history)
  expect_equal(nrow(collection_data(view)), 24L)
})

test_that("nonestimable regressions fail before fitting", {
  skip_if_not_installed("estimatr")
  x <- regression_study()
  expect_identical(
    check_analysis(x[1:2, ], recall_request())$issues[[1]]$code,
    "regression_replication_insufficient"
  )
  expect_identical(
    check_analysis(x[1:3, ], recall_request("respondent"))$issues[[1]]$code,
    "regression_clusters_insufficient"
  )
  data <- as.data.frame(x)
  data$duplicate <- data$order * 2
  rank_deficient <- studyframe(data, "id", "recall", "rank")
  request <- recall_request()
  request$predictors <- list("order", "duplicate")
  checked <- check_analysis(rank_deficient, request)
  expect_identical(checked$issues[[1]]$code, "regression_rank_deficient")
  failed <- tryCatch(
    run_analysis(rank_deficient, request),
    studyframe_analysis_error = identity
  )
  expect_identical(failed$issues, checked$issues)
  request$predictors <- list("absent")
  expect_identical(
    check_analysis(x, request)$issues[[1]]$code,
    "regression_variable_unknown"
  )
  data$order <- as.character(data$order)
  text <- studyframe(data, "id", "recall", "text")
  expect_identical(
    check_analysis(text, recall_request())$issues[[1]]$code,
    "regression_type_unsupported"
  )
  data$order <- as.numeric(data$order)
  data$respondent <- as.numeric(data$respondent)
  noninteger_clusters <- studyframe(data, "id", "recall", "cluster")
  expect_identical(
    check_analysis(noninteger_clusters, recall_request("respondent"))$issues[[
      1
    ]]$code,
    "regression_cluster_type"
  )
})

test_that("regression refuses nonfinite coefficient inference", {
  skip_if_not_installed("estimatr")
  local_mocked_bindings(
    lm_robust = function(...) {
      list(
        coefficients = c(1, 2),
        std.error = c(0.5, Inf),
        df = c(6, 6),
        p.value = c(0.2, 0.2),
        conf.low = c(0, 0),
        conf.high = c(2, 4)
      )
    },
    .package = "estimatr"
  )
  failed <- tryCatch(
    run_analysis(regression_study(), recall_request()),
    studyframe_analysis_error = identity
  )
  expect_identical(failed$issues[[1]]$code, "inference_unavailable")
})
test_that("regression requests reject shaped containers and accept JSON arrays", {
  x <- studyframe(
    data.frame(id = 1:5, y = c(1, 4, 2, 5, 3)),
    "id",
    "person",
    "regression"
  )
  request <- regression_request(
    "y",
    target = "Association",
    assumptions = "Independent observations"
  )
  invalid <- list(structure(request, class = "data.frame", row.names = 1L))
  for (value in list(
    matrix("id", 1L),
    list(c("id", "y")),
    list(NA_character_),
    list(named = "id")
  )) {
    candidate <- request
    candidate$predictors <- value
    invalid[[length(invalid) + 1L]] <- candidate
  }
  for (candidate in invalid) {
    checked <- check_analysis(x, candidate)
    expect_identical(checked$ok, FALSE)
    expect_identical(checked$issues[[1]]$code, "invalid_request")
  }
  roundtrip <- jsonlite::fromJSON(.json(request), simplifyVector = FALSE)
  expect_identical(check_analysis(x, roundtrip)$ok, TRUE)
})

test_that("regression scalar fields and array elements retain their JSON types", {
  x <- studyframe(
    data.frame(id = 1:5, y = c(1, 4, 2, 5, 3)),
    "id",
    "person",
    "regression"
  )
  request <- regression_request(
    "y",
    cluster = "id",
    target = "Association",
    assumptions = "Independent clusters"
  )
  for (field in c("outcome", "cluster", "se_type", "missing", "target")) {
    for (value in list(matrix(request[[field]], 1L), I(request[[field]]))) {
      candidate <- request
      candidate[[field]] <- value
      checked <- check_analysis(x, candidate)
      expect_identical(checked$ok, FALSE)
      expect_identical(checked$issues[[1]]$code, "invalid_request")
    }
  }
  for (value in list(
    list(matrix("Independent clusters", 1L)),
    list(I("Independent clusters"))
  )) {
    candidate <- request
    candidate$assumptions <- value
    expect_identical(
      check_analysis(x, candidate)$issues[[1]]$code,
      "invalid_request"
    )
  }
})
