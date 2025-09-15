test_that("SuperDataFrame creation works", {
  # Basic creation
  df <- SuperDataFrame(
    data.frame(x = 1:3, y = 4:6),
    version = "1.0",
    author = "Test User",
    notes = "Test data"
  )
  
  expect_s3_class(df, "superdf")
  expect_s3_class(df, "data.frame")
  expect_equal(version(df), "1.0")
  expect_equal(author(df), "Test User")
  expect_equal(notes(df), "Test data")
})

test_that("metadata accessors work", {
  df <- SuperDataFrame(
    data.frame(a = 1, b = 2),
    version = "2.0",
    author = "Author",
    notes = "Notes"
  )
  
  # Test getters
  expect_equal(version(df), "2.0")
  expect_equal(author(df), "Author")
  expect_equal(notes(df), "Notes")
  
  # Test setters
  version(df) <- "3.0"
  author(df) <- "New Author"
  notes(df) <- "New Notes"
  
  expect_equal(version(df), "3.0")
  expect_equal(author(df), "New Author")
  expect_equal(notes(df), "New Notes")
})

test_that("subsetting preserves metadata", {
  df <- SuperDataFrame(
    data.frame(x = 1:5, y = letters[1:5]),
    version = "1.0",
    author = "Test"
  )
  
  # Row subsetting
  subset_rows <- df[1:3, ]
  expect_s3_class(subset_rows, "superdf")
  expect_equal(version(subset_rows), "1.0")
  expect_equal(nrow(subset_rows), 3)
  
  # Column subsetting
  subset_cols <- df[, "x", drop = FALSE]
  expect_s3_class(subset_cols, "superdf")
  expect_equal(version(subset_cols), "1.0")
  expect_equal(ncol(subset_cols), 1)
  
  # Logical subsetting
  logical_subset <- df[df$x > 2, ]
  expect_s3_class(logical_subset, "superdf")
  expect_equal(version(logical_subset), "1.0")
  expect_equal(nrow(logical_subset), 3)
})

test_that("rbind preserves metadata from first SuperDataFrame", {
  df1 <- SuperDataFrame(
    data.frame(x = 1:2, y = c("a", "b")),
    version = "1.0",
    author = "First"
  )
  
  df2 <- SuperDataFrame(
    data.frame(x = 3:4, y = c("c", "d")),
    version = "2.0",
    author = "Second"
  )
  
  combined <- rbind(df1, df2)
  expect_s3_class(combined, "superdf")
  expect_equal(version(combined), "1.0")  # From first
  expect_equal(author(combined), "First")
  expect_equal(nrow(combined), 4)
})

test_that("cbind preserves metadata from first SuperDataFrame", {
  df1 <- SuperDataFrame(
    data.frame(x = 1:3),
    version = "1.0",
    author = "First"
  )
  
  df2 <- data.frame(y = 4:6)  # Regular data.frame
  
  combined <- cbind(df1, df2)
  expect_s3_class(combined, "superdf")
  expect_equal(version(combined), "1.0")
  expect_equal(ncol(combined), 2)
})

test_that("update_metadata creates new version", {
  df <- SuperDataFrame(
    data.frame(x = 1:3),
    version = "1.0",
    author = "Original"
  )
  
  df_updated <- update_metadata(df, 
                               version = "2.0", 
                               notes = "Updated")
  
  # Original unchanged
  expect_equal(version(df), "1.0")
  expect_equal(notes(df), "")
  
  # New version updated
  expect_equal(version(df_updated), "2.0")
  expect_equal(author(df_updated), "Original")  # Preserved
  expect_equal(notes(df_updated), "Updated")
})

test_that("conversion to/from data.frame works", {
  # Create SuperDataFrame
  df <- SuperDataFrame(
    data.frame(x = 1:3, y = 4:6),
    version = "1.0"
  )
  
  # Convert to regular data.frame
  regular_df <- as.data.frame(df)
  expect_s3_class(regular_df, "data.frame")
  expect_false(inherits(regular_df, "superdf"))
  
  # Convert back to SuperDataFrame
  back_to_super <- as_superdf(regular_df, version = "2.0", author = "Converter")
  expect_s3_class(back_to_super, "superdf")
  expect_equal(version(back_to_super), "2.0")
  expect_equal(author(back_to_super), "Converter")
})

test_that("print method shows data and metadata", {
  df <- SuperDataFrame(
    data.frame(x = 1:2, y = c("a", "b")),
    version = "1.0",
    author = "Test User",
    notes = "Test notes"
  )
  
  # Capture print output
  output <- capture.output(print(df))
  
  # Should contain data
  expect_true(any(grepl("x.*y", output)))
  expect_true(any(grepl("1.*a", output)))
  
  # Should contain metadata
  expect_true(any(grepl("Metadata:", output)))
  expect_true(any(grepl("Version: 1.0", output)))
  expect_true(any(grepl("Author.*Test User", output)))
})

test_that("empty metadata displays correctly", {
  df <- SuperDataFrame(data.frame(x = 1:2))
  
  # Should have empty metadata
  expect_equal(version(df), "")
  expect_equal(author(df), "")
  expect_equal(notes(df), "")
  
  # Print should show "(no metadata)"
  output <- capture.output(print(df))
  expect_true(any(grepl("\\(no metadata\\)", output)))
})

test_that("SuperDataFrame works like regular data.frame", {
  df <- SuperDataFrame(
    data.frame(
      x = 1:5,
      y = rnorm(5),
      group = rep(c("A", "B"), length.out = 5)
    ),
    version = "test"
  )
  
  # Test data.frame-like operations
  expect_equal(nrow(df), 5)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("x", "y", "group"))
  
  # Test column access
  expect_equal(df$x, 1:5)
  expect_equal(df[["x"]], 1:5)
  
  # Test summary
  summary_output <- capture.output(summary(df))
  expect_true(any(grepl("SuperDataFrame Summary", summary_output)))
  expect_true(any(grepl("x.*:", summary_output)))  # Data summary
})