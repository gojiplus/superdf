test_that("typed recoding keeps source measurements and records its definition", {
  x <- studyframe(
    data.frame(id = 1:4, answer = c(1L, 2L, -9L, NA_integer_)),
    "id",
    "person",
    "codes"
  )
  view <- x[1:3, ]
  y <- recode_studyframe(
    view,
    variable_spec("yes"),
    "answer",
    c(1L, 2L, -9L),
    c(TRUE, FALSE, NA),
    description = "Yes versus no; refused stays missing"
  )
  expect_identical(y$yes, c(TRUE, FALSE, NA))
  expect_identical(y$answer, view$answer)
  expect_true(is.na(collection_data(y)$yes[[4]]))
  event <- tail(context(y)$history, 1)[[1]]
  expect_identical(event$inputs, list("answer"))
  expect_identical(event$outputs, list("yes"))
  expect_match(event$details, "refused stays missing", fixed = TRUE)
  expect_true(validate_studyframe(y))
})

test_that("keeping unmapped missing codes cannot turn them into measurements", {
  missing <- list(list(value = -99, reason = "refused"))
  x <- studyframe(
    data.frame(id = 1:3, answer = c(1, -99, 3)),
    "id",
    "person",
    "codes",
    variables = list(variable_spec("answer", missing = missing))
  )
  expect_error(
    recode_studyframe(
      x,
      variable_spec("new"),
      "answer",
      1,
      10,
      "keep",
      description = "Replace one only"
    ),
    "original missing declarations"
  )
  kept <- recode_studyframe(
    x,
    variable_spec("new", missing = missing),
    "answer",
    1,
    10,
    "keep",
    description = "Replace one and retain refusal"
  )
  expect_identical(kept$new, c(10, -99, 3))
  expect_identical(context(kept)$variables[[3]]$missing, missing)
  mapped <- recode_studyframe(
    x,
    variable_spec("new"),
    "answer",
    c(1, -99),
    c(10, NA_real_),
    "keep",
    description = "Replace one and convert refusal to NA"
  )
  expect_identical(mapped$new, c(10, NA_real_, 3))
})

test_that("unmapped codes and type coercion require explicit decisions", {
  x <- studyframe(
    data.frame(id = 1:3, answer = c(1L, 2L, NA_integer_)),
    "id",
    "person",
    "codes"
  )
  recode <- function(from, to, unmatched = "error") {
    recode_studyframe(
      x,
      variable_spec("new"),
      "answer",
      from,
      to,
      unmatched,
      description = "Explicit mapping"
    )
  }
  expect_error(recode(1L, 3L), "unmapped")
  expect_error(recode("1", 3L), "Source code types")
  expect_error(recode(c(1L, 1L), c(2L, 3L)), "distinct")
  expect_error(recode(1L, "yes", "keep"), "storage types")
  expect_identical(recode(1L, 3L, "keep")$new, c(3L, 2L, NA_integer_))
  expect_identical(recode(1L, TRUE, "missing")$new, c(TRUE, NA, NA))
})
