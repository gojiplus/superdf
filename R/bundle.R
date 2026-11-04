.file_hash <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

#' Save or read data and context as one portable bundle
#' @param x A superdf.
#' @param file Bundle filename. Writers refuse to overwrite existing files.
#' @param max_bytes Maximum total uncompressed size accepted when reading.
#' @return write_superdf returns the filename invisibly; read_superdf returns a
#'   validated superdf. A bundle is a ZIP archive with two Parquet tables,
#'   context.json, and a manifest containing SHA-256 integrity hashes.
#'   Hashes detect corruption, not the truth or authenticity of a declaration.
#' @name bundles
#' @export
write_superdf <- function(x, file) {
  validate_superdf(x)
  .require("arrow")
  .require("zip")
  .assert(
    !file.exists(file),
    "Destination exists; choose a new bundle filename."
  )
  folder <- tempfile("superdf-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  arrow::write_parquet(.plain(x), file.path(folder, "data.parquet"))
  arrow::write_parquet(
    attr(x, "reference"),
    file.path(folder, "roster.parquet")
  )
  writeLines(
    .json(context(x)),
    file.path(folder, "context.json"),
    useBytes = TRUE
  )
  payloads <- c("data.parquet", "roster.parquet", "context.json")
  hashes <- stats::setNames(
    lapply(payloads, function(f) .file_hash(file.path(folder, f))),
    payloads
  )
  writeLines(
    .json(list(format = "superdf-bundle/1", sha256 = hashes)),
    file.path(folder, "manifest.json")
  )
  destination <- file.path(
    normalizePath(dirname(file), mustWork = TRUE),
    basename(file)
  )
  archive <- tempfile(
    "superdf-",
    tmpdir = dirname(destination),
    fileext = ".zip"
  )
  on.exit(unlink(archive), add = TRUE)
  zip::zipr(archive, c("manifest.json", payloads), root = folder)
  .assert(
    file.rename(archive, destination),
    "Could not install the completed bundle."
  )
  invisible(file)
}

#' @rdname bundles
#' @export
read_superdf <- function(file, max_bytes = 256 * 1024^2) {
  .require("arrow")
  inventory <- utils::unzip(file, list = TRUE)
  expected <- c(
    "manifest.json",
    "context.json",
    "data.parquet",
    "roster.parquet"
  )
  .assert(
    nrow(inventory) == 4L && setequal(inventory$Name, expected),
    "Unexpected or duplicate bundle entries."
  )
  .assert(
    is.numeric(max_bytes) &&
      length(max_bytes) == 1L &&
      is.finite(max_bytes) &&
      max_bytes > 0 &&
      sum(inventory$Length) <= max_bytes,
    "Bundle exceeds the uncompressed size limit."
  )
  folder <- tempfile("superdf-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  utils::unzip(file, files = expected, exdir = folder)
  manifest <- jsonlite::read_json(
    file.path(folder, "manifest.json"),
    simplifyVector = FALSE
  )
  .assert(
    length(manifest) == 2L &&
      setequal(names(manifest), c("format", "sha256")) &&
      identical(manifest$format, "superdf-bundle/1"),
    "Unsupported bundle manifest."
  )
  payloads <- setdiff(expected, "manifest.json")
  .assert(
    setequal(names(manifest$sha256), payloads),
    "Manifest must hash every payload."
  )
  for (name in payloads) {
    .assert(
      identical(manifest$sha256[[name]], .file_hash(file.path(folder, name))),
      paste("Bundle integrity failure:", name)
    )
  }
  ctx <- jsonlite::read_json(
    file.path(folder, "context.json"),
    simplifyVector = FALSE
  )
  jsonvalidate::json_validate(
    .json(ctx),
    .schema_path(),
    engine = "ajv",
    error = TRUE
  )
  data <- as.data.frame(arrow::read_parquet(file.path(folder, "data.parquet")))
  ref <- as.data.frame(arrow::read_parquet(file.path(folder, "roster.parquet")))
  out <- .new_superdf(data, ctx, ref)
  validate_superdf(out)
  out
}

#' Generate a readable codebook from the attached declarations
#' @param x A superdf.
#' @param file Markdown destination, which must not already exist.
#' @return The filename invisibly. The codebook is a view of the JSON context;
#'   the bundle remains the complete machine-readable representation.
#' @export
write_codebook <- function(x, file) {
  validate_superdf(x)
  .assert(!file.exists(file), "Codebook destination exists.")
  ctx <- context(x)
  escape <- function(s) gsub("[\r\n]", " ", gsub("|", "\\|", s, fixed = TRUE))
  lines <- c(
    paste("#", escape(ctx$dataset$id)),
    "",
    paste("Dataset version:", escape(ctx$dataset$version)),
    paste("Observation:", escape(ctx$units$observation)),
    paste(
      "Current rows:",
      nrow(x),
      "Collection roster:",
      nrow(attr(x, "reference"))
    ),
    "",
    "| Variable ID | Column | Definition | Units | Wave | Status |",
    "|---|---|---|---|---|---|"
  )
  for (v in ctx$variables) {
    cells <- c(
      v$id,
      v$column,
      v$label,
      if (is.null(v$units)) "unknown" else v$units,
      if (is.null(v$wave)) "unknown" else v$wave,
      v$status
    )
    lines <- c(
      lines,
      paste0(
        "| ",
        paste(vapply(cells, escape, character(1)), collapse = " | "),
        " |"
      )
    )
  }
  for (v in ctx$variables) {
    if (length(v$missing) || length(v$categories)) {
      lines <- c(lines, "", paste("## Codes:", v$id), "")
      for (code in v$categories) {
        lines <- c(
          lines,
          paste0(
            "- ",
            escape(as.character(code$value)),
            ": ",
            escape(code$label)
          )
        )
      }
      for (code in v$missing) {
        lines <- c(
          lines,
          paste0(
            "- Missing ",
            escape(as.character(code$value)),
            ": ",
            code$reason
          )
        )
      }
    }
  }
  lines <- c(
    lines,
    "",
    "## Collection and analysis declarations",
    "",
    "These declarations are documented facts or assumptions, not proof of identification.",
    "",
    "```json",
    jsonlite::prettify(.json(list(
      sampling = ctx$sampling,
      weights = ctx$weights,
      assignments = ctx$assignments
    ))),
    "```",
    "",
    "## Transformation history",
    "",
    "```json",
    jsonlite::prettify(.json(ctx$history)),
    "```"
  )
  writeLines(lines, file, useBytes = TRUE)
  invisible(file)
}
