test_that("joins distinguish numeric keys without display rounding", {
  keys <- c(1, 1 + .Machine$double.eps, 2)
  x <- studyframe(
    data.frame(id = 1:3, key = keys),
    "id",
    "person",
    "numeric_keys"
  )
  y <- data.frame(key = keys[c(2, 1)], value = c(20, 10))
  result <- join_studyframe(
    x,
    y,
    "key",
    list(variable_spec("value")),
    "Measurement register"
  )
  expect_identical(result$value, y$value[match(keys, y$key)])
  expect_identical(result$value, c(10, 20, NA_real_))
  expect_true(validate_studyframe(result))
})

test_that("composite joins preserve exact keys and full roster alignment", {
  keys <- c(1, 1 + .Machine$double.eps)
  x <- studyframe(
    data.frame(
      id = 1:5,
      key = c(keys, keys, keys[[1]]),
      site = c("a|1", "a|1", "b", "b", "a|1")
    ),
    "id",
    "person",
    "composite_keys"
  )
  y <- data.frame(
    key = c(keys[[2]], keys[[1]], keys[[2]]),
    site = c("b", "a|1", "a|1"),
    value = c(40, 10, 20)
  )
  view <- x[c(5, 2, 3), ]
  first <- join_studyframe(
    view,
    y,
    c("key", "site"),
    list(variable_spec("value")),
    "Register"
  )
  second <- join_studyframe(
    view,
    y[c(3, 1, 2), ],
    c("key", "site"),
    list(variable_spec("value")),
    "Register"
  )
  expect_identical(first$value, c(10, 20, NA_real_))
  expect_identical(collection_data(first)$value, c(10, 20, NA_real_, 40, 10))
  expect_identical(first$value, second$value)
  expect_identical(collection_data(first), collection_data(second))
  expect_identical(first$id, view$id)
})

test_that("empty right tables leave every roster measurement missing", {
  x <- studyframe(
    data.frame(id = 1:2, key = c(1, 2)),
    "id",
    "person",
    "empty_lookup"
  )
  y <- data.frame(key = numeric(), value = numeric())
  result <- join_studyframe(
    x,
    y,
    "key",
    list(variable_spec("value")),
    "Empty register"
  )
  expect_identical(result$value, c(NA_real_, NA_real_))
})

test_that("ambiguous right-hand column names cannot discard measurements", {
  x <- studyframe(data.frame(id = 1:2), "id", "person", "column_names")
  duplicated <- data.frame(
    id = 1:2,
    outcome = c(10, 20),
    outcome = c(30, 40),
    check.names = FALSE
  )
  expect_error(
    join_studyframe(
      x,
      duplicated,
      "id",
      list(variable_spec("outcome")),
      "Register"
    ),
    "Right-hand columns must have unique, nonempty names"
  )
  unnamed <- data.frame(id = 1:2, outcome = c(10, 20))
  names(unnamed)[[2]] <- ""
  expect_error(
    join_studyframe(
      x,
      unnamed,
      "id",
      list(variable_spec("outcome", column = "")),
      "Register"
    ),
    "Right-hand columns must have unique, nonempty names"
  )
})
