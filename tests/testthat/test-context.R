test_that("schema and semantic validation distinguish incomplete from inconsistent", {
  x <- studyframe(data.frame(id = 1:3, y = c(1, NA, 3)), "id", "person", "unknown")
  expect_true(validate_studyframe(x))
  expect_length(context(x)$sampling, 0)
  expect_identical(context(x)$variables[[2]]$status, "unknown")
  expect_error(
    studyframe(data.frame(id = c(1L, 1L)), "id", "person", "duplicates"),
    "Duplicate observation"
  )
  expect_error(
    studyframe(data.frame(id = c(1L, NA)), "id", "person", "missing"),
    "nonmissing"
  )
  expect_error(
    studyframe(
      data.frame(id = 1:2),
      "id",
      "person",
      "bad",
      sampling = list(sampling_spec(
        "s",
        stages = list(sampling_stage("absent", method = "srs"))
      ))
    ),
    "unknown variable"
  )
  expect_error(
    studyframe(
      data.frame(id = 1:2, f = factor(c("a", "b"))),
      "id",
      "person",
      "factor"
    ),
    "primitive"
  )
  expect_error(
    studyframe(data.frame(id = 1:2, y = c(Inf, 0)), "id", "person", "infinite"),
    "Infinite"
  )
})

test_that("renames preserve identities and unsupported mutations cannot masquerade as valid", {
  x <- reference_workflows()$survey
  y <- rename_studyframe(x, c(id = "person_id", income = "monthly_income"))
  expect_identical(context(y)$units$keys, list("id"))
  expect_identical(context(y)$variables[[1]]$id, context(x)$variables[[1]]$id)
  expect_true(validate_studyframe(y))
  expect_error(x$income <- x$income * 2, "direct mutation")
  expect_error(x[1, "income"] <- 10, "direct mutation")
  expect_error(names(x) <- names(x), "direct mutation")
  forged <- x
  class(forged) <- "data.frame"
  forged$income <- forged$income * 2
  class(forged) <- c("studyframe", "data.frame")
  expect_error(validate_studyframe(forged), "Unsupported mutation")
  expect_error(x[, c("income", "weight")], "keys must remain")
  expect_error(x[c(1, 1), ], "Duplicate observation")
})

test_that("views preserve rosters and bindings require one compatible collection", {
  x <- reference_workflows()$survey
  first <- x[1:12, ]
  last <- x[13:24, ]
  combined <- rbind(first, last)
  expect_equal(as.data.frame(combined), as.data.frame(x))
  expect_equal(collection_data(first), collection_data(x))
  expect_error(rbind(first, first), "Duplicate observation")
  expect_error(
    rbind(x, reference_workflows()$experiment),
    "different collection contexts"
  )
  expect_error(cbind(x, data.frame(a = 1:24)), "explicit keys")
  expect_error(
    aggregate(x, by = list(x$district), FUN = mean),
    "observational unit"
  )
})

test_that("derived variables retain definitions without overwriting collection facts", {
  x <- reference_workflows()$survey
  view <- x[1:6, ]
  y <- derive_studyframe(
    view,
    variable_spec("annual_income", units = "USD/year"),
    view$income * 12,
    inputs = "income",
    description = "Monthly income multiplied by twelve"
  )
  expect_equal(y$annual_income, view$income * 12)
  expect_true(all(is.na(collection_data(y)$annual_income[-(1:6)])))
  expect_equal(tail(context(y)$history, 1)[[1]]$operation, "derive")
  expect_error(
    derive_studyframe(x, variable_spec("income"), x$income, "income", "Overwrite"),
    "new variable"
  )
})

test_that("joins use keys, preserve order, and refuse unit multiplication", {
  x <- reference_workflows()$survey
  lookup <- data.frame(school = c(8L, 2L, 1L), label = c("eight", "two", "one"))
  joined <- join_studyframe(
    x,
    lookup,
    "school",
    list(variable_spec("label")),
    "School register"
  )
  expect_equal(joined$label, lookup$label[match(x$school, lookup$school)])
  expect_equal(joined$id, x$id)
  expect_error(
    join_studyframe(
      x,
      rbind(lookup, lookup[1, ]),
      "school",
      list(variable_spec("label")),
      "Register"
    ),
    "duplicate units"
  )
  expect_true(validate_studyframe(joined))
})

test_that("typed applicability and assignment codes cannot coerce silently", {
  d <- data.frame(
    id = 1:4,
    z = c(0L, 1L, 0L, 1L),
    flag = rep(TRUE, 4),
    w = rep(1, 4)
  )
  expect_error(
    studyframe(
      d,
      "id",
      "person",
      "typed",
      weights = list(weight_spec(
        "main",
        "w",
        applies = list(list(variable = "flag", values = list(1)))
      ))
    ),
    "Applicability values"
  )
  expect_error(
    studyframe(
      d,
      "id",
      "person",
      "typed",
      assignments = list(assignment_spec(
        "trial",
        "z",
        "id",
        conditions = c("0", "1")
      ))
    ),
    "Assignment conditions"
  )
  expect_error(
    studyframe(
      d,
      "id",
      "person",
      "typed",
      variables = list(variable_spec(
        "z",
        missing = list(list(value = 0.5, reason = "invalid"))
      ))
    ),
    "Code type"
  )
})

test_that("operation and result provenance identifies exact inputs", {
  skip_if_not_installed("survey")
  x <- reference_workflows()$survey
  y <- derive_studyframe(
    x,
    variable_spec("double_income"),
    x$income * 2,
    "income",
    "Multiply by two"
  )
  event <- tail(studyframe::context(y)$history, 1)[[1]]
  expect_identical(event$parents, list(attr(x, "seal")))
  expect_identical(event$inputs, list("income"))
  expect_identical(event$outputs, list("double_income"))
  result <- estimate_mean(y, "double_income", "main")
  expect_identical(result$analysis$input_digest, attr(y, "seal"))
})
