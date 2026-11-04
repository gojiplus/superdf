test_that("survey estimates and domains reproduce the original design", {
  skip_if_not_installed("survey")
  x <- reference_workflows()$survey
  d <- as.data.frame(x)
  direct <- survey::svydesign(
    ids = ~school,
    strata = ~district,
    weights = ~weight,
    data = d,
    nest = TRUE
  )
  expected <- survey::svymean(~income, direct)
  result <- estimate_mean(x, "income", "main")
  expect_equal(coef(result$fit), coef(expected))
  expect_equal(survey::SE(result$fit), survey::SE(expected))
  expect_equal(result$analysis$design_df, survey::degf(direct))
  selected <- d$school %in% c(1, 2, 5)
  view <- x[selected, ]
  expect_error(estimate_mean(view, "income", "main"), "domain = TRUE")
  domain <- estimate_mean(view, "income", "main", domain = TRUE)
  expected_domain <- survey::svymean(~income, direct[selected, ])
  expect_equal(coef(domain$fit), coef(expected_domain))
  expect_equal(survey::SE(domain$fit), survey::SE(expected_domain))
  expect_equal(domain$analysis$design_df, survey::degf(direct[selected, ]))
  expect_equal(nrow(collection_data(view)), nrow(d))
})

test_that("replicate method, scaling, and centering survive translation", {
  skip_if_not_installed("survey")
  x <- reference_workflows()$replicates
  d <- as.data.frame(x)
  direct <- survey::svrepdesign(
    variables = d,
    weights = d$weight,
    repweights = d[paste0("rep", 1:8)],
    type = "other",
    combined.weights = TRUE,
    scale = 7 / 8,
    rscales = rep(1, 8),
    mse = TRUE
  )
  result <- estimate_mean(x, "income", "main")
  expected <- survey::svymean(~income, direct)
  expect_equal(coef(result$fit), coef(expected))
  expect_equal(survey::SE(result$fit), survey::SE(expected))
  expect_equal(result$analysis$design_df, survey::degf(direct))
})

test_that("blocked cluster experiment reproduces estimatr", {
  skip_if_not_installed("estimatr")
  x <- reference_workflows()$experiment
  d <- as.data.frame(x)
  direct <- estimatr::difference_in_means(
    outcome ~ offer,
    data = d,
    blocks = district,
    clusters = school,
    condition1 = 0,
    condition2 = 1
  )
  result <- estimate_effect(x, "outcome", "offer_assignment")
  expect_equal(result$fit$coefficients, direct$coefficients)
  expect_equal(result$fit$std.error, direct$std.error)
  expect_equal(result$fit$df, direct$df)
  reordered <- x[rev(seq_len(nrow(x))), ]
  renamed <- rename_studyframe(
    reordered,
    c(outcome = "outcome measured", school = "school_id")
  )
  expect_equal(
    estimate_effect(renamed, "outcome", "offer_assignment")$fit$coefficients,
    direct$coefficients
  )
  expect_error(
    estimate_effect(x[-1, ], "outcome", "offer_assignment"),
    "Attrition or restriction"
  )
})

test_that("joining incomplete follow-up preserves randomized units and exposes attrition", {
  skip_if_not_installed("estimatr")
  x <- reference_workflows()$experiment
  followed <- join_studyframe(
    x,
    data.frame(id = 2:32, followup = seq_len(31)),
    "id",
    list(variable_spec("followup")),
    "Incomplete second follow-up"
  )
  expect_equal(followed$id, x$id)
  expect_true(is.na(followed$followup[[1]]))
  expect_error(
    estimate_effect(followed, "followup", "offer_assignment"),
    "requires complete numeric outcomes"
  )
})

test_that("weight applicability is checked against both rows and measurement wave", {
  skip_if_not_installed("survey")
  x <- reference_workflows()$waves
  expect_error(estimate_mean(x, "post", "pre"), "measurement wave")
  expect_error(estimate_mean(x, "post", "post"), "not applicable")
  view <- x[x$completed, ]
  result <- estimate_mean(view, "post", "post")
  d <- as.data.frame(view)
  expected <- survey::svymean(
    ~post,
    survey::svydesign(ids = ~id, weights = ~post_weight, data = d)
  )
  expect_equal(coef(result$fit), coef(expected))
  expect_equal(survey::SE(result$fit), survey::SE(expected))
  expect_equal(result$analysis$applicable_rows, 10L)
  expect_false(result$analysis$domain)
})

test_that("two-stage SRS with FPC reproduces survey", {
  skip_if_not_installed("survey")
  e <- new.env()
  utils::data("api", package = "survey", envir = e)
  d <- e$apiclus2[c("dnum", "snum", "fpc1", "fpc2", "api00")]
  d$fpc2 <- as.numeric(d$fpc2)
  d$id <- seq_len(nrow(d))
  direct <- survey::svydesign(
    ids = ~ dnum + snum,
    fpc = ~ fpc1 + fpc2,
    data = d
  )
  d$weight <- as.numeric(weights(direct))
  x <- studyframe(
    d,
    "id",
    "school",
    "api_two_stage",
    sampling = list(sampling_spec(
      "sample",
      "API schools",
      list(
        sampling_stage(
          "dnum",
          method = "srs",
          replacement = "without",
          fpc = "fpc1"
        ),
        sampling_stage(
          "snum",
          method = "srs",
          replacement = "without",
          fpc = "fpc2"
        )
      ),
      source = "survey API example"
    )),
    weights = list(weight_spec(
      "main",
      "weight",
      sampling = "sample",
      source = "Design probabilities"
    ))
  )
  expected <- survey::svymean(~api00, direct)
  result <- estimate_mean(x, "api00", "main")
  expect_equal(coef(result$fit), coef(expected))
  expect_equal(survey::SE(result$fit), survey::SE(expected))
})

test_that("missing codes remain in storage and omission is explicit", {
  skip_if_not_installed("survey")
  d <- data.frame(id = 1:4, y = c(1, 2, -9, 4), w = rep(1, 4))
  x <- studyframe(
    d,
    "id",
    "person",
    "missing",
    variables = list(variable_spec(
      "y",
      missing = list(list(value = -9, reason = "refused"))
    )),
    sampling = list(sampling_spec(
      "s",
      stages = list(sampling_stage("id", method = "srs", replacement = "with"))
    )),
    weights = list(weight_spec("w", "w", sampling = "s"))
  )
  expect_equal(x$y[[3]], -9)
  expect_error(estimate_mean(x, "y", "w"), "Missing outcomes")
  result <- estimate_mean(x, "y", "w", missing = "omit")
  expect_equal(unname(coef(result$fit)), 7 / 3)
  expect_equal(result$analysis$missing_rows, 1L)
  expect_equal(result$analysis$used_rows, 3L)
})

test_that("unknown and unsupported designs are never silently simplified", {
  skip_if_not_installed("survey")
  d <- data.frame(id = 1:4, y = 1:4, w = rep(1, 4))
  make <- function(stage, adjustments = character()) {
    studyframe(
      d,
      "id",
      "person",
      "unsupported",
      sampling = list(sampling_spec("s", stages = list(stage))),
      weights = list(weight_spec(
        "w",
        "w",
        sampling = "s",
        adjustments = adjustments
      ))
    )
  }
  unknown <- make(sampling_stage("id", method = "srs"))
  expect_true(validate_studyframe(unknown))
  expect_error(estimate_mean(unknown, "y", "w"), "replacement must be declared")
  expect_error(
    estimate_mean(
      make(sampling_stage("id", method = "pps", replacement = "with")),
      "y",
      "w"
    ),
    "SRS stages only"
  )
  expect_error(
    estimate_mean(
      make(sampling_stage("id", method = "srs", replacement = "without")),
      "y",
      "w"
    ),
    "finite-population"
  )
  expect_error(
    estimate_mean(
      make(
        sampling_stage("id", method = "srs", replacement = "with"),
        "calibration"
      ),
      "y",
      "w"
    ),
    "Adjusted weights"
  )
})

test_that("single-PSU policy is explicit and global options are restored", {
  skip_if_not_installed("survey")
  x <- reference_workflows()$survey
  old <- options(
    survey.lonely.psu = "average",
    survey.adjust.domain.lonely = TRUE
  )
  on.exit(options(old), add = TRUE)
  estimate_mean(x, "income", "main")
  expect_identical(getOption("survey.lonely.psu"), "average")
  expect_true(getOption("survey.adjust.domain.lonely"))
})

test_that("FPC fractions and population sizes can differ across stages", {
  skip_if_not_installed("survey")
  d <- data.frame(
    id = 1:8,
    school = rep(1:4, each = 2),
    person = rep(1:2, 4),
    school_population = rep(8, 8),
    person_fraction = rep(0.5, 8),
    weight = rep(4, 8),
    y = seq_len(8)
  )
  x <- studyframe(
    d,
    "id",
    "person",
    "mixed_fpc",
    sampling = list(sampling_spec(
      "s",
      stages = list(
        sampling_stage(
          "school",
          method = "srs",
          replacement = "without",
          fpc = "school_population"
        ),
        sampling_stage(
          "person",
          method = "srs",
          replacement = "without",
          fpc = "person_fraction",
          fpc_kind = "sampling_fraction"
        )
      )
    )),
    weights = list(weight_spec("w", "weight", sampling = "s"))
  )
  d$person_population <- 4
  direct <- survey::svydesign(
    ids = ~ school + person,
    fpc = ~ school_population + person_population,
    weights = ~weight,
    data = d,
    nest = TRUE
  )
  result <- estimate_mean(x, "y", "w")
  expect_equal(survey::SE(result$fit), survey::SE(survey::svymean(~y, direct)))
})

test_that("standard replicate methods preserve the released variance convention", {
  skip_if_not_installed("survey")
  d <- as.data.frame(reference_workflows()$survey)
  d <- d[d$school %in% c(1, 2, 5, 6), ]
  direct <- survey::svydesign(
    ids = ~school,
    strata = ~district,
    weights = ~weight,
    data = d,
    nest = TRUE
  )
  set.seed(128)
  for (method in c("BRR", "Fay", "JKn", "bootstrap")) {
    args <- list(design = direct, type = method, mse = FALSE)
    if (method == "Fay") {
      args$fay.rho <- 0.3
    }
    if (method == "bootstrap") {
      args$replicates <- 15
    }
    replicated <- do.call(survey::as.svrepdesign, args)
    repweights <- as.matrix(weights(replicated, type = "analysis"))
    columns <- paste0("rep", seq_len(ncol(repweights)))
    data <- d
    data[columns] <- repweights
    spec <- variance_spec(
      method,
      replicates = columns,
      mse = FALSE,
      rho = if (method == "Fay") 0.3 else NULL
    )
    if (!method %in% c("BRR", "Fay")) {
      spec$scale <- replicated$scale
      spec$rscales <- as.list(replicated$rscales)
    }
    x <- studyframe(
      data,
      "id",
      "pupil",
      "replication_methods",
      weights = list(weight_spec("w", "weight", variance = spec))
    )
    expected <- survey::svymean(~income, replicated)
    result <- estimate_mean(x, "income", "w")
    expect_equal(coef(result$fit), coef(expected), info = method)
    expect_equal(survey::SE(result$fit), survey::SE(expected), info = method)
  }
})
