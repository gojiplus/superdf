#' Data and the context needed to interpret it
#' @description studyframe binds observations, a collection roster, and portable
#'   declarations. Supported operations update that context. Analysis adapters
#'   check their own requirements; metadata is not a certificate of identification.
#' @name studyframe-package
#' @keywords internal
"_PACKAGE"

.array <- function(x) unname(as.list(x))
.strings <- function(x) unname(unlist(x, use.names = FALSE))
.plain <- function(x) {
  attributes(x) <- attributes(x)[c("names", "row.names")]
  class(x) <- "data.frame"
  x
}
.storage <- function(x) {
  if (is.object(x) || !is.atomic(x) || !is.null(dim(x))) {
    stop(
      "Use primitive integer, numeric, character, or logical columns.",
      call. = FALSE
    )
  }
  switch(
    typeof(x),
    integer = "integer",
    double = "number",
    character = "string",
    logical = "boolean",
    stop("Unsupported column type.", call. = FALSE)
  )
}
.json <- function(x) {
  jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}
.schema_path <- function() {
  system.file(
    "schema",
    "context.schema.json",
    package = "studyframe",
    mustWork = TRUE
  )
}
.col <- function(context, id) {
  vars <- context$variables
  found <- which(vapply(vars, function(v) identical(v$id, id), logical(1)))
  if (length(found) != 1L) {
    stop("Unknown variable identifier: ", id, call. = FALSE)
  }
  vars[[found]]$column
}
.keys <- function(data, context) {
  columns <- vapply(
    context$units$keys,
    function(k) .col(context, k),
    character(1)
  )
  if (!all(columns %in% names(data))) {
    stop("Observation keys must remain in the data.", call. = FALSE)
  }
  parts <- lapply(data[columns], function(v) {
    if (!(is.character(v) || is.integer(v)) || anyNA(v)) {
      stop(
        "Keys must be nonmissing character or integer columns.",
        call. = FALSE
      )
    }
    if (!length(v)) {
      return(character())
    }
    v <- enc2utf8(as.character(v))
    paste0(nchar(v, type = "bytes"), ":", v)
  })
  result <- do.call(paste, c(parts, list(sep = "|")))
  if (anyDuplicated(result)) {
    stop("Duplicate observation keys.", call. = FALSE)
  }
  result
}
.seal <- function(data, context, reference) {
  digest::digest(
    list(lapply(data, identity), context, lapply(reference, identity)),
    algo = "sha256",
    serializeVersion = 2
  )
}
.new_studyframe <- function(data, context, reference) {
  data <- .plain(data)
  reference <- .plain(reference)
  rownames(data) <- NULL
  rownames(reference) <- NULL
  seal <- .seal(data, context, reference)
  structure(
    data,
    class = c("studyframe", "data.frame"),
    context = context,
    reference = reference,
    seal = seal
  )
}
.record <- function(
  context,
  operation,
  details,
  before,
  after,
  parents,
  inputs = character(),
  outputs = character()
) {
  context$history[[length(context$history) + 1L]] <- list(
    operation = operation,
    details = details,
    rows_before = as.integer(before),
    rows_after = as.integer(after),
    parents = .array(parents),
    inputs = .array(inputs),
    outputs = .array(outputs)
  )
  context
}

#' Bind observations to their collection context
#' @param data A data.frame of primitive columns. Factors, dates, list columns,
#'   infinities, and NaN must be converted explicitly before construction.
#' @param keys Character vector of stable variable identifiers uniquely identifying rows.
#' @param unit Description of the observational unit.
#' @param dataset_id Dataset identifier.
#' @param version Dataset version (distinct from the schema version).
#' @param author Responsible person or organization.
#' @param notes Free-text context.
#' @param variables List of variable_spec declarations. Undeclared columns receive
#'   an unknown definition. IDs default to column names and must be syntactic identifiers.
#' @param sampling List of sampling_spec declarations.
#' @param weights List of weight_spec declarations.
#' @param assignments List of assignment_spec declarations.
#' @param roster A roster_spec. Ordinary imports default to unknown completeness.
#' @param evidence List of evidence_spec source records.
#' @param diagnostics List of analysis_issue records retaining import uncertainty.
#' @return A studyframe inheriting from data.frame, with its original roster attached.
#' @export
#' @examples
#' x <- studyframe(data.frame(id = 1:3, income = c(10, 20, 30)),
#'   keys = "id", unit = "person", dataset_id = "example"
#' )
#' x[c(3, 1), ]
#' context(x)$units
studyframe <- function(
  data,
  keys,
  unit,
  dataset_id,
  version = "",
  author = "",
  notes = "",
  variables = list(),
  sampling = list(),
  weights = list(),
  assignments = list(),
  roster = roster_spec(),
  evidence = list(),
  diagnostics = list()
) {
  if (!is.data.frame(data)) {
    stop("data must be a data.frame.", call. = FALSE)
  }
  data <- .plain(data)
  if (anyDuplicated(names(data)) || any(!nzchar(names(data)))) {
    stop("Column names must be unique and nonempty.")
  }
  declared <- vapply(variables, function(v) v$column, character(1))
  if (anyDuplicated(declared) || !all(declared %in% names(data))) {
    stop("Variable declarations must name distinct existing columns.")
  }
  for (column in names(data)) {
    i <- match(column, declared)
    if (is.na(i)) {
      variables[[length(variables) + 1L]] <- variable_spec(column)
      declared <- c(declared, column)
      i <- length(variables)
    }
    variables[[i]]$storage <- .storage(data[[column]])
  }
  ctx <- list(
    schema_version = "0.3.0",
    dataset = list(
      id = dataset_id,
      version = version,
      author = author,
      notes = notes
    ),
    units = list(observation = unit, keys = .array(keys)),
    variables = .array(variables),
    sampling = .array(sampling),
    weights = .array(weights),
    assignments = .array(assignments),
    history = list(),
    roster = roster,
    evidence = .array(evidence),
    diagnostics = .array(diagnostics)
  )
  x <- .new_studyframe(data, ctx, data)
  validate_studyframe(x)
  x
}

#' Inspect attached context and roster
#' @param x A studyframe.
#' @return context returns a list; collection_data returns the original roster with
#'   supported derivations and renames applied. as.data.frame explicitly extracts
#'   the current observations without metadata.
#' @name context
#' @export
context <- function(x) {
  if (!inherits(x, "studyframe")) {
    stop("Expected a studyframe.", call. = FALSE)
  }
  attr(x, "context", exact = TRUE)
}

#' @rdname context
#' @export
collection_data <- function(x) {
  validate_studyframe(x)
  attr(x, "reference", exact = TRUE)
}

#' @export
as.data.frame.studyframe <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  .plain(x)
}

#' @export
print.studyframe <- function(x, ...) {
  validate_studyframe(x)
  print(.plain(x), ...)
  ctx <- context(x)
  cat(
    "\nstudyframe:",
    ctx$dataset$id,
    "| unit:",
    ctx$units$observation,
    "| current / roster:",
    nrow(x),
    "/",
    nrow(attr(x, "reference")),
    "\n"
  )
  cat(
    "Declarations:",
    length(ctx$sampling),
    "sampling;",
    length(ctx$weights),
    "weights;",
    length(ctx$assignments),
    "assignment.\n"
  )
  invisible(x)
}
