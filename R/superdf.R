#' SuperDataFrame: Data Frames with Persistent Metadata
#'
#' @description
#' SuperDataFrame extends R's data.frame to include persistent metadata
#' (version, author, notes) that survives data operations and I/O.
#'
#' @details
#' SuperDataFrame objects are fully compatible with existing data.frame
#' operations while providing additional functionality for data lineage
#' and documentation. The metadata is stored as attributes and persists
#' through subsetting, combining, and I/O operations.
#'
#' @name superdf-package
#' @aliases superdf
#' @docType package
NULL

# Metadata class ----
new_metadata <- function(version = "", author = "", notes = "") {
  structure(
    list(
      version = as.character(version),
      author = as.character(author),
      notes = as.character(notes)
    ),
    class = "superdf_metadata"
  )
}

#' @export
print.superdf_metadata <- function(x, ...) {
  cat("Metadata:\n")
  if (nchar(x$version) > 0) cat("  Version:", x$version, "\n")
  if (nchar(x$author) > 0) cat("  Author: ", x$author, "\n")
  if (nchar(x$notes) > 0) cat("  Notes:  ", x$notes, "\n")
  if (nchar(x$version) == 0 && nchar(x$author) == 0 && nchar(x$notes) == 0) {
    cat("  (no metadata)\n")
  }
}

# Main constructor ----

#' Create a SuperDataFrame
#'
#' Creates a data.frame with persistent metadata that survives operations and I/O.
#'
#' @param data A data.frame or object that can be converted to data.frame
#' @param version Character string describing the data version
#' @param author Character string describing the data author/creator
#' @param notes Character string with additional notes about the data
#' @param metadata A superdf_metadata object (alternative to individual parameters)
#'
#' @return A SuperDataFrame object that inherits from data.frame
#'
#' @examples
#' # Create with individual metadata fields
#' df <- SuperDataFrame(
#'   data.frame(x = 1:5, y = letters[1:5]),
#'   version = "1.0",
#'   author = "Data Analyst",
#'   notes = "Sample dataset"
#' )
#'
#' # Access metadata
#' version(df)
#' author(df)
#' notes(df)
#'
#' # Metadata persists through operations
#' subset_df <- df[df$x > 2, ]
#' version(subset_df)  # Still "1.0"
#'
#' @export
SuperDataFrame <- function(data = data.frame(), version = "", author = "", notes = "", metadata = NULL) {
  # Ensure data is a data.frame
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }

  # Create or use provided metadata
  if (is.null(metadata)) {
    metadata <- new_metadata(version, author, notes)
  }

  # Store metadata as an attribute
  attr(data, "superdf_metadata") <- metadata

  # Set S3 class for method dispatch
  class(data) <- c("superdf", "data.frame")

  return(data)
}

# Metadata accessors ----

#' Get or set metadata components
#'
#' @param x A SuperDataFrame object
#' @param value New value to set
#' @param ... Additional arguments (unused)
#'
#' @return For getters: the metadata value. For setters: the modified object.
#'
#' @examples
#' df <- SuperDataFrame(data.frame(x = 1:3), version = "1.0", author = "Me")
#' version(df)         # "1.0"
#' author(df)          # "Me"
#' notes(df)           # ""
#'
#' version(df) <- "2.0"
#' version(df)         # "2.0"
#'
#' @name metadata-accessors
NULL

#' @rdname metadata-accessors
#' @export
version <- function(x, ...) UseMethod("version")

#' @rdname metadata-accessors
#' @export
version.superdf <- function(x, ...) {
  meta <- attr(x, "superdf_metadata")
  if (is.null(meta)) "" else meta$version
}

#' @rdname metadata-accessors
#' @export
author <- function(x, ...) UseMethod("author")

#' @rdname metadata-accessors
#' @export
author.superdf <- function(x, ...) {
  meta <- attr(x, "superdf_metadata")
  if (is.null(meta)) "" else meta$author
}

#' @rdname metadata-accessors
#' @export
notes <- function(x, ...) UseMethod("notes")

#' @rdname metadata-accessors
#' @export
notes.superdf <- function(x, ...) {
  meta <- attr(x, "superdf_metadata")
  if (is.null(meta)) "" else meta$notes
}

#' @rdname metadata-accessors
#' @export
metadata <- function(x, ...) UseMethod("metadata")

#' @rdname metadata-accessors
#' @export
metadata.superdf <- function(x, ...) {
  attr(x, "superdf_metadata")
}

# Metadata setters ----

#' @rdname metadata-accessors
#' @export
`version<-` <- function(x, value) UseMethod("version<-")

#' @rdname metadata-accessors
#' @export
`version<-.superdf` <- function(x, value) {
  meta <- attr(x, "superdf_metadata")
  if (is.null(meta)) meta <- new_metadata()
  meta$version <- as.character(value)
  attr(x, "superdf_metadata") <- meta
  x
}

#' @rdname metadata-accessors
#' @export
`author<-` <- function(x, value) UseMethod("author<-")

#' @rdname metadata-accessors
#' @export
`author<-.superdf` <- function(x, value) {
  meta <- attr(x, "superdf_metadata")
  if (is.null(meta)) meta <- new_metadata()
  meta$author <- as.character(value)
  attr(x, "superdf_metadata") <- meta
  x
}

#' @rdname metadata-accessors
#' @export
`notes<-` <- function(x, value) UseMethod("notes<-")

#' @rdname metadata-accessors
#' @export
`notes<-.superdf` <- function(x, value) {
  meta <- attr(x, "superdf_metadata")
  if (is.null(meta)) meta <- new_metadata()
  meta$notes <- as.character(value)
  attr(x, "superdf_metadata") <- meta
  x
}

# Update metadata (immutable) ----

#' Update SuperDataFrame metadata
#'
#' Creates a new SuperDataFrame with updated metadata, leaving the original unchanged.
#'
#' @param x A SuperDataFrame
#' @param version New version string (optional)
#' @param author New author string (optional)
#' @param notes New notes string (optional)
#' @param metadata A complete metadata object (optional)
#'
#' @return A new SuperDataFrame with updated metadata
#'
#' @examples
#' df <- SuperDataFrame(data.frame(x = 1:3), version = "1.0")
#' df2 <- update_metadata(df, version = "2.0", author = "New Author")
#' version(df)   # Still "1.0"
#' version(df2)  # Now "2.0"
#'
#' @export
update_metadata <- function(x, version = NULL, author = NULL, notes = NULL, metadata = NULL) {
  UseMethod("update_metadata")
}

#' @export
update_metadata.superdf <- function(x, version = NULL, author = NULL, notes = NULL, metadata = NULL) {
  if (!is.null(metadata)) {
    new_meta <- metadata
  } else {
    current <- attr(x, "superdf_metadata")
    if (is.null(current)) current <- new_metadata()

    new_meta <- new_metadata(
      version = if (!is.null(version)) version else current$version,
      author = if (!is.null(author)) author else current$author,
      notes = if (!is.null(notes)) notes else current$notes
    )
  }

  result <- x
  attr(result, "superdf_metadata") <- new_meta
  result
}

# Data frame methods ----

#' @export
`[.superdf` <- function(x, i, j, drop = if (missing(i)) TRUE else FALSE) {
  # Call the default data.frame method
  result <- NextMethod("[")

  # If result is still a data.frame, preserve metadata
  if (is.data.frame(result)) {
    attr(result, "superdf_metadata") <- attr(x, "superdf_metadata")
    class(result) <- c("superdf", "data.frame")
  }

  result
}

#' @export
rbind.superdf <- function(..., deparse.level = 1) {
  args <- list(...)
  metadata_source <- NULL

  # Find first SuperDataFrame to get metadata from
  for (arg in args) {
    if (inherits(arg, "superdf")) {
      metadata_source <- attr(arg, "superdf_metadata")
      break
    }
  }

  # Convert all SuperDataFrames to regular data.frames for rbind
  args <- lapply(args, function(x) {
    if (inherits(x, "superdf")) {
      class(x) <- "data.frame"
    }
    x
  })

  result <- do.call(rbind.data.frame, c(args, list(deparse.level = deparse.level)))

  # Restore metadata if found
  if (!is.null(metadata_source)) {
    attr(result, "superdf_metadata") <- metadata_source
    class(result) <- c("superdf", "data.frame")
  }

  result
}

#' @export
cbind.superdf <- function(..., deparse.level = 1) {
  args <- list(...)
  metadata_source <- NULL

  # Find first SuperDataFrame to get metadata from
  for (arg in args) {
    if (inherits(arg, "superdf")) {
      metadata_source <- attr(arg, "superdf_metadata")
      break
    }
  }

  # Convert to regular data.frames for cbind
  args <- lapply(args, function(x) {
    if (inherits(x, "superdf")) {
      class(x) <- "data.frame"
    }
    x
  })

  result <- do.call(cbind.data.frame, c(args, list(deparse.level = deparse.level)))

  # Restore metadata if found
  if (!is.null(metadata_source)) {
    attr(result, "superdf_metadata") <- metadata_source
    class(result) <- c("superdf", "data.frame")
  }

  result
}

# Display methods ----

#' @export
print.superdf <- function(x, ...) {
  # Print the data.frame part first
  class(x) <- "data.frame"
  print(x, ...)

  # Then print metadata
  cat("\n")
  print(attr(x, "superdf_metadata"))

  # Restore class
  class(x) <- c("superdf", "data.frame")
  invisible(x)
}

#' @export
summary.superdf <- function(object, ...) {
  cat("SuperDataFrame Summary\n")
  cat("=====================\n\n")

  # Print metadata first
  print(attr(object, "superdf_metadata"))
  cat("\nData Summary:\n")

  # Then call regular summary on data
  class(object) <- "data.frame"
  summary(object, ...)
}

# Conversion functions ----

#' @export
as.data.frame.superdf <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- "data.frame"
  x
}

#' Convert data.frame to SuperDataFrame
#'
#' @param x A data.frame
#' @param version Version string
#' @param author Author string
#' @param notes Notes string
#' @param metadata Complete metadata object
#'
#' @return A SuperDataFrame
#'
#' @examples
#' regular_df <- data.frame(x = 1:3, y = 4:6)
#' super_df <- as_superdf(regular_df, version = "1.0", author = "Converter")
#' version(super_df)
#'
#' @export
as_superdf <- function(x, version = "", author = "", notes = "", metadata = NULL) {
  SuperDataFrame(x, version, author, notes, metadata)
}

# I/O functions ----

#' Write SuperDataFrame to CSV with metadata
#'
#' @param x A SuperDataFrame
#' @param file File path
#' @param ... Additional arguments passed to write.csv
#'
#' @examples
#' \dontrun{
#' df <- SuperDataFrame(data.frame(x = 1:3), version = "1.0", author = "Me")
#' write_superdf_csv(df, "data.csv")
#' }
#'
#' @export
write_superdf_csv <- function(x, file, ...) {
  if (!inherits(x, "superdf")) {
    stop("x must be a SuperDataFrame")
  }

  metadata_obj <- attr(x, "superdf_metadata")

  if (!is.null(file) && file != "") {
    # Write metadata as header comments
    cat("# SuperDataFrame Metadata\n", file = file)
    if (nchar(metadata_obj$version) > 0) {
      cat("# Version:", metadata_obj$version, "\n", file = file, append = TRUE)
    }
    if (nchar(metadata_obj$author) > 0) {
      cat("# Author:", metadata_obj$author, "\n", file = file, append = TRUE)
    }
    if (nchar(metadata_obj$notes) > 0) {
      cat("# Notes:", metadata_obj$notes, "\n", file = file, append = TRUE)
    }

    # Write the actual data
    utils::write.csv(as.data.frame(x), file = file, append = TRUE, ...)
  } else {
    utils::write.csv(as.data.frame(x), file = file, ...)
  }
}

#' Read SuperDataFrame from CSV with metadata
#'
#' @param file File path
#' @param ... Additional arguments passed to read.csv
#'
#' @return A SuperDataFrame with metadata extracted from comments
#'
#' @examples
#' \dontrun{
#' df <- read_superdf_csv("data.csv")
#' version(df)
#' }
#'
#' @export
read_superdf_csv <- function(file, ...) {
  version <- ""
  author <- ""
  notes <- ""

  # Try to read metadata from comments if file exists
  if (file.exists(file)) {
    lines <- readLines(file, n = 20)
    comment_lines <- lines[grepl("^#", lines)]

    # Extract metadata from comments
    version_line <- comment_lines[grepl("Version:", comment_lines)]
    author_line <- comment_lines[grepl("Author:", comment_lines)]
    notes_line <- comment_lines[grepl("Notes:", comment_lines)]

    if (length(version_line) > 0) {
      version <- trimws(gsub("^#\\s*Version:\\s*", "", version_line[1]))
    }
    if (length(author_line) > 0) {
      author <- trimws(gsub("^#\\s*Author:\\s*", "", author_line[1]))
    }
    if (length(notes_line) > 0) {
      notes <- trimws(gsub("^#\\s*Notes:\\s*", "", notes_line[1]))
    }
  }

  # Read the data (skip comment lines)
  data <- utils::read.csv(file, comment.char = "#", ...)

  # Create SuperDataFrame with metadata
  SuperDataFrame(data, version = version, author = author, notes = notes)
}