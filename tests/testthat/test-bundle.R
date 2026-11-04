test_that("every reference workflow round trips with its roster and context", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("zip")
  examples <- reference_workflows()
  examples$survey <- rename_superdf(
    examples$survey[c(8, 2, 1), ],
    c(income = "monthly income")
  )
  for (x in examples) {
    f <- tempfile(fileext = ".superdf")
    on.exit(unlink(f), add = TRUE)
    write_superdf(x, f)
    back <- read_superdf(f)
    expect_equal(as.data.frame(back), as.data.frame(x))
    expect_equal(collection_data(back), collection_data(x))
    expect_equal(context(back), context(x))
    expect_error(write_superdf(x, f), "Destination exists")
    expect_error(read_superdf(f, max_bytes = 10), "size limit")
  }
})

test_that("bundle corruption and untracked entries are refused", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("zip")
  x <- reference_workflows()$survey
  f <- tempfile(fileext = ".superdf")
  folder <- tempfile()
  dir.create(folder)
  on.exit(unlink(c(f, folder), recursive = TRUE), add = TRUE)
  write_superdf(x, f)
  utils::unzip(f, exdir = folder)
  writeLines("{}", file.path(folder, "context.json"))
  zip::zipr(f, list.files(folder), root = folder)
  expect_error(read_superdf(f), "integrity failure")
  writeLines("extra", file.path(folder, "extra.txt"))
  zip::zipr(f, list.files(folder), root = folder)
  expect_error(read_superdf(f), "Unexpected")
})

test_that("codebooks are generated from the same declarations", {
  x <- reference_workflows()$survey
  f <- tempfile(fileext = ".md")
  on.exit(unlink(f), add = TRUE)
  write_codebook(x, f)
  text <- paste(readLines(f), collapse = "\n")
  expect_match(text, "Monthly income", fixed = TRUE)
  expect_match(text, "Synthetic sampling protocol", fixed = TRUE)
  expect_error(write_codebook(x, f), "destination exists")
})
