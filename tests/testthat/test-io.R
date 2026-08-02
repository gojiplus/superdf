test_that("CSV round trip preserves metadata", {
  df <- SuperDataFrame(
    data.frame(x = 1:3, y = c("a", "b", "c")),
    version = "1.0",
    author = "Me",
    notes = "Some notes"
  )
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)

  expect_no_warning(write_superdf_csv(df, f))

  back <- read_superdf_csv(f)
  expect_equal(version(back), "1.0")
  expect_equal(author(back), "Me")
  expect_equal(notes(back), "Some notes")
})

test_that("CSV round trip preserves the data itself", {
  df <- SuperDataFrame(
    data.frame(x = 1:3, y = c("a", "b", "c")),
    version = "1.0"
  )
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  write_superdf_csv(df, f)
  back <- read_superdf_csv(f)

  expect_equal(names(back), c("x", "y"))
  expect_equal(dim(back), c(3L, 2L))
  expect_equal(as.data.frame(back), as.data.frame(df))
})

test_that("metadata comments are actually present in the file", {
  df <- SuperDataFrame(data.frame(x = 1:2), version = "2.1", author = "A B")
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  write_superdf_csv(df, f)

  lines <- readLines(f)
  expect_true(any(grepl("^# SuperDataFrame Metadata", lines)))
  expect_true(any(grepl("^# Version: 2\\.1$", lines)))
  expect_true(any(grepl("^# Author: A B$", lines)))
  # Notes were empty, so no Notes comment
  expect_false(any(grepl("^# Notes:", lines)))
})

test_that("non-default row names survive the round trip", {
  df <- SuperDataFrame(data.frame(x = 1:5, y = letters[1:5]), version = "1.0")
  sub <- df[3:5, ]
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  write_superdf_csv(sub, f)
  back <- read_superdf_csv(f)

  expect_equal(rownames(back), c("3", "4", "5"))
  expect_equal(as.data.frame(back), as.data.frame(sub))
})

test_that("row.names = FALSE round trips without a spurious column", {
  df <- SuperDataFrame(data.frame(x = 1:3, y = c("a", "b", "c")),
    version = "9.9"
  )
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  write_superdf_csv(df, f, row.names = FALSE)
  back <- read_superdf_csv(f)

  expect_equal(names(back), c("x", "y"))
  expect_equal(version(back), "9.9")
  expect_equal(as.data.frame(back), as.data.frame(df))
})

test_that("dialect arguments are ignored with a warning, as in write.csv", {
  df <- SuperDataFrame(data.frame(x = 1:2), version = "1.0")
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)

  expect_warning(write_superdf_csv(df, f, sep = ";"), "'sep' ignored")
  expect_equal(version(read_superdf_csv(f)), "1.0")
  expect_equal(names(read_superdf_csv(f)), "x")
})

test_that("a plain CSV without metadata still reads correctly", {
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  utils::write.csv(data.frame(x = 1:3, y = 4:6), f, row.names = FALSE)
  back <- read_superdf_csv(f)

  expect_s3_class(back, "superdf")
  expect_equal(names(back), c("x", "y"))
  expect_equal(version(back), "")
})
