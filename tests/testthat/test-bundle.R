test_that("every reference workflow round trips with its roster and context", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("zip")
  examples <- reference_workflows()
  examples$survey <- rename_studyframe(
    examples$survey[c(8, 2, 1), ],
    c(income = "monthly income")
  )
  for (x in examples) {
    f <- tempfile(fileext = ".studyframe")
    on.exit(unlink(f), add = TRUE)
    write_studyframe(x, f)
    back <- read_studyframe(f)
    expect_equal(as.data.frame(back), as.data.frame(x))
    expect_equal(collection_data(back), collection_data(x))
    expect_equal(context(back), context(x))
    expect_error(write_studyframe(x, f), "Destination exists")
    expect_error(read_studyframe(f, max_bytes = 10), "size limit")
  }
})

test_that("writing non-ASCII responses preserves the original object's validity", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("zip")
  skip_if_not(l10n_info()[["UTF-8"]])
  response <- rawToChar(as.raw(c(99, 97, 102, 195, 169)))
  expect_identical(Encoding(response), "unknown")
  x <- studyframe(
    data.frame(id = 1L, response = response),
    "id", "person", "text-response"
  )
  f <- tempfile(fileext = ".studyframe")
  on.exit(unlink(f), add = TRUE)
  write_studyframe(x, f)
  expect_true(validate_studyframe(x))
  back <- read_studyframe(f)
  expect_identical(as.data.frame(back), as.data.frame(x))
  expect_identical(collection_data(back), collection_data(x))
  expect_identical(context(back), context(x))
  forged <- x
  class(forged) <- "data.frame"
  forged$response <- "changed"
  class(forged) <- c("studyframe", "data.frame")
  expect_error(validate_studyframe(forged), "Unsupported mutation")
})

test_that("empty views round trip with the full collection roster", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("zip")
  x <- reference_workflows()$survey[FALSE, ]
  f <- tempfile(fileext = ".studyframe")
  on.exit(unlink(f), add = TRUE)
  write_studyframe(x, f)
  back <- read_studyframe(f)
  expect_identical(as.data.frame(back), as.data.frame(x))
  expect_identical(collection_data(back), collection_data(x))
  expect_equal(context(back), context(x))
})

test_that("bundle corruption and untracked entries are refused", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("zip")
  x <- reference_workflows()$survey
  f <- tempfile(fileext = ".studyframe")
  folder <- tempfile()
  dir.create(folder)
  on.exit(unlink(c(f, folder), recursive = TRUE), add = TRUE)
  write_studyframe(x, f)
  utils::unzip(f, exdir = folder)
  writeLines("{}", file.path(folder, "context.json"))
  zip::zipr(f, list.files(folder), root = folder)
  expect_error(read_studyframe(f), "integrity failure")
  writeLines("extra", file.path(folder, "extra.txt"))
  zip::zipr(f, list.files(folder), root = folder)
  expect_error(read_studyframe(f), "Unexpected")
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
