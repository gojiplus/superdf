test_that("nonfinite numeric metadata cannot silently become unknown JSON values", {
  observations <- data.frame(id = 1:4, outcome = c(1, 2, 4, 8), weight = 1)
  construct <- function(value) {
    studyframe(
      observations,
      "id",
      "person",
      "scale_boundary",
      weights = list(weight_spec(
        "main",
        "weight",
        variance = variance_spec(scale = value)
      ))
    )
  }
  for (value in c(Inf, -Inf, NaN, NA_real_)) {
    expect_error(construct(value), "Numeric metadata must be finite")
  }
  expect_true(validate_studyframe(construct(NULL)))
  expect_true(validate_studyframe(construct(1)))
})
