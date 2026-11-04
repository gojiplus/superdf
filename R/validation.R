.unique_ids <- function(records, what) {
  ids <- vapply(records, function(x) x$id, character(1))
  if (anyDuplicated(ids)) {
    stop("Duplicate ", what, " identifiers.", call. = FALSE)
  }
  ids
}
.references <- function(ctx) {
  refs <- .strings(ctx$units$keys)
  for (s in ctx$sampling) {
    for (stage in s$stages) {
      refs <- c(
        refs,
        stage$unit,
        stage$strata,
        stage$probability,
        stage$fpc$variable
      )
    }
  }
  for (w in ctx$weights) {
    refs <- c(refs, w$variable, .strings(w$variance$replicates))
    for (rule in w$applies) {
      refs <- c(refs, rule$variable)
    }
  }
  for (a in ctx$assignments) {
    refs <- c(refs, a$treatment, a$unit, a$blocks)
  }
  for (event in ctx$history) {
    refs <- c(refs, .strings(event$inputs), .strings(event$outputs))
  }
  unique(refs)
}
.assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}
.code_type <- function(value, storage) {
  switch(
    storage,
    integer = is.numeric(value) && is.finite(value) && value == floor(value),
    number = is.numeric(value) && is.finite(value),
    string = is.character(value),
    boolean = is.logical(value)
  )
}
.validate_tables <- function(data, ctx, reference) {
  ids <- .unique_ids(ctx$variables, "variable")
  sampling_ids <- .unique_ids(ctx$sampling, "sampling")
  .unique_ids(ctx$weights, "weight")
  assignment_ids <- .unique_ids(ctx$assignments, "assignment")
  evidence_ids <- .unique_ids(ctx$evidence, "evidence")
  sources <- .strings(ctx$roster$evidence)
  for (item in c(ctx$variables, ctx$sampling, ctx$weights, ctx$assignments)) {
    sources <- c(sources, .strings(item$evidence))
    if (!is.null(item$instrument)) sources <- c(sources, item$instrument$source)
  }
  for (issue in ctx$diagnostics) {
    sources <- c(sources, .strings(issue$sources))
  }
  .assert(
    all(sources %in% evidence_ids),
    "A declaration references unknown evidence."
  )
  roster <- ctx$roster
  if (!is.null(roster$scope)) {
    allowed <- if (roster$kind == "randomized_units") {
      assignment_ids
    } else {
      sampling_ids
    }
    .assert(
      roster$scope %in% allowed,
      "Roster scope does not match its declaration kind."
    )
  }
  if (roster$completeness == "asserted_complete") {
    .assert(
      roster$kind != "unknown" &&
        nzchar(trimws(roster$source)) &&
        length(roster$evidence) > 0L,
      "Asserted roster completeness requires a known kind, a source, and evidence references."
    )
    if (roster$kind %in% c("sampled_units", "randomized_units")) {
      .assert(
        !is.null(roster$scope),
        "Complete sampled or randomized rosters require a scope declaration."
      )
    }
  }
  columns <- vapply(ctx$variables, function(v) v$column, character(1))
  .assert(!anyDuplicated(columns), "Variable columns must be unique.")
  .assert(
    setequal(columns, names(reference)),
    "Variable registry must describe every roster column exactly once."
  )
  .assert(
    !anyDuplicated(names(data)) && all(names(data) %in% columns),
    "Current columns must be distinct registered columns."
  )
  .assert(
    all(.references(ctx) %in% ids),
    "A declaration references an unknown variable identifier."
  )
  variable_type <- function(id) ctx$variables[[match(id, ids)]]$storage
  for (weight in ctx$weights) {
    for (rule in weight$applies) {
      .assert(
        all(vapply(
          rule$values,
          .code_type,
          logical(1),
          variable_type(rule$variable)
        )),
        "Applicability values must match the referenced variable's storage type."
      )
    }
  }
  for (assignment in ctx$assignments) {
    .assert(
      all(vapply(
        assignment$conditions,
        .code_type,
        logical(1),
        variable_type(assignment$treatment)
      )),
      "Assignment conditions must match the treatment storage type."
    )
  }
  for (w in ctx$weights) {
    .assert(
      is.null(w$sampling) || w$sampling %in% sampling_ids,
      "Weight references an unknown sampling design."
    )
    v <- w$variance
    .assert(
      length(v$rscales) == 0L || length(v$rscales) == length(v$replicates),
      "rscales must match replicate columns."
    )
    .assert(
      v$method != "taylor" || length(v$replicates) == 0L,
      "Taylor variance cannot declare replicate columns."
    )
    .assert(
      v$method == "taylor" || length(v$replicates) >= 2L,
      "Replication requires at least two replicate columns."
    )
    .assert(v$method != "Fay" || !is.null(v$rho), "Fay variance requires rho.")
    .assert(
      v$method == "Fay" || is.null(v$rho),
      "rho applies only to Fay variance."
    )
  }
  for (v in ctx$variables) {
    values <- reference[[v$column]]
    .assert(
      identical(.storage(values), v$storage),
      paste("Storage type disagrees for", v$id)
    )
    if (is.numeric(values)) {
      .assert(
        !any(is.infinite(values) | is.nan(values)),
        "Infinite and NaN values are not supported; use explicit missing codes or NA."
      )
    }
    for (entry in c(v$missing, v$categories)) {
      type_ok <- .code_type(entry$value, v$storage)
      .assert(type_ok, paste("Code type disagrees for", v$id))
    }
    for (codes in list(v$missing, v$categories)) {
      encoded <- vapply(
        codes,
        function(z) as.character(.json(z$value)),
        character(1)
      )
      .assert(!anyDuplicated(encoded), paste("Duplicate codes for", v$id))
    }
  }
  original <- .keys(reference, ctx)
  current <- .keys(data, ctx)
  positions <- match(current, original)
  .assert(
    !anyNA(positions),
    "Current observations are absent from the collection roster."
  )
  for (column in names(data)) {
    .assert(
      identical(data[[column]], reference[[column]][positions]),
      paste("Current values disagree with collection context for", column)
    )
  }
  invisible(TRUE)
}

#' Validate structure and consistency without certifying an analysis
#' @param x A superdf.
#' @return TRUE invisibly, or an error describing structural inconsistency or an
#'   unsupported mutation. Estimation adapters separately check sufficiency.
#' @export
validate_superdf <- function(x) {
  ctx <- context(x)
  ref <- attr(x, "reference", exact = TRUE)
  .assert(is.data.frame(ref), "Missing collection roster.")
  .assert(
    identical(attr(x, "seal", exact = TRUE), .seal(.plain(x), ctx, ref)),
    "Unsupported mutation detected. Use SuperDF operations or explicitly construct a new dataset."
  )
  jsonvalidate::json_validate(
    .json(ctx),
    .schema_path(),
    engine = "ajv",
    error = TRUE
  )
  .validate_tables(.plain(x), ctx, ref)
  invisible(TRUE)
}
