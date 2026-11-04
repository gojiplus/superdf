#' @export
`[.superdf` <- function(x, i, j, drop = FALSE) {
  validate_superdf(x)
  .assert(
    identical(drop, FALSE),
    "Use as.data.frame(x) to explicitly extract data without context."
  )
  data <- .plain(x)
  if ((nargs() - !missing(drop)) < 3L) {
    result <- if (missing(i)) data else data[i]
  } else {
    result <- if (missing(i) && missing(j)) {
      data
    } else if (missing(i)) {
      data[, j, drop = FALSE]
    } else if (missing(j)) {
      data[i, , drop = FALSE]
    } else {
      data[i, j, drop = FALSE]
    }
  }
  ctx <- .record(
    context(x),
    "subset",
    paste(deparse(sys.call()), collapse = " "),
    nrow(x),
    nrow(result),
    attr(x, "seal")
  )
  out <- .new_superdf(result, ctx, attr(x, "reference"))
  validate_superdf(out)
  out
}

.mutation_error <- function() {
  stop(
    "Use derive_superdf(), rename_superdf(), or an explicit new dataset; direct mutation is unsupported.",
    call. = FALSE
  )
}

#' @export
`[<-.superdf` <- function(x, i, j, ..., value) .mutation_error()
#' @export
`[[<-.superdf` <- function(x, ..., value) .mutation_error()
#' @export
`$<-.superdf` <- function(x, name, value) .mutation_error()
#' @export
`names<-.superdf` <- function(x, value) .mutation_error()
#' @export
`dimnames<-.superdf` <- function(x, value) .mutation_error()

#' Rename columns while retaining their scientific identities
#' @param x A superdf.
#' @param names Named character vector: old column names as names, new names as values.
#' @return A superdf with updated column mappings and history.
#' @export
rename_superdf <- function(x, names) {
  validate_superdf(x)
  .assert(
    is.character(names) &&
      !is.null(base::names(names)) &&
      !anyDuplicated(base::names(names)),
    "Supply a named character vector of old = new column names."
  )
  ctx <- context(x)
  ref <- attr(x, "reference")
  data <- .plain(x)
  .assert(
    all(base::names(names) %in% base::names(data)) &&
      !anyNA(names) &&
      all(nzchar(names)),
    "Renames must refer to current columns and have nonempty destinations."
  )
  for (k in seq_along(ctx$variables)) {
    old <- ctx$variables[[k]]$column
    if (old %in% base::names(names)) {
      ctx$variables[[k]]$column <- unname(names[[old]])
    }
  }
  new_names <- vapply(ctx$variables, function(v) v$column, character(1))
  .assert(!anyDuplicated(new_names), "Rename creates duplicate column names.")
  replace <- function(n) {
    ifelse(n %in% base::names(names), names[match(n, base::names(names))], n)
  }
  base::names(data) <- unname(replace(base::names(data)))
  base::names(ref) <- unname(replace(base::names(ref)))
  ctx <- .record(
    ctx,
    "rename",
    as.character(.json(as.list(names))),
    nrow(x),
    nrow(x),
    attr(x, "seal")
  )
  out <- .new_superdf(data, ctx, ref)
  validate_superdf(out)
  out
}

#' Add a derived variable with its definition
#' @param x A superdf.
#' @param variable A variable_spec for a new variable.
#' @param values Values in current row order, exactly one per row.
#' @param inputs Stable identifiers of input variables.
#' @param description Human-readable derivation; arbitrary code is not evaluated.
#' @return A superdf. Roster rows outside the current view receive NA for the new
#'   measurement. Existing measurements and collection facts cannot be overwritten.
#' @export
derive_superdf <- function(x, variable, values, inputs, description) {
  validate_superdf(x)
  ctx <- context(x)
  .assert(
    length(description) == 1L && nzchar(description),
    "Describe the derivation."
  )
  .assert(
    length(values) == nrow(x),
    "Derived values must have one entry per current row."
  )
  .assert(
    !variable$id %in% vapply(ctx$variables, function(v) v$id, character(1)) &&
      !variable$column %in% names(attr(x, "reference")),
    "Derivation must create a new variable."
  )
  for (id in inputs) {
    .assert(
      .col(ctx, id) %in% names(x),
      "Derivation inputs must be in the current view."
    )
  }
  variable$storage <- .storage(values)
  data <- .plain(x)
  ref <- attr(x, "reference")
  positions <- match(.keys(data, ctx), .keys(ref, ctx))
  full <- values[rep(NA_integer_, nrow(ref))]
  full[positions] <- values
  data[[variable$column]] <- values
  ref[[variable$column]] <- full
  ctx$variables[[length(ctx$variables) + 1L]] <- variable
  ctx <- .record(
    ctx,
    "derive",
    description,
    nrow(x),
    nrow(x),
    attr(x, "seal"),
    inputs,
    variable$id
  )
  out <- .new_superdf(data, ctx, ref)
  validate_superdf(out)
  out
}

#' Join measurements without multiplying observational units
#' @param x A superdf.
#' @param y A data.frame of additional measurements.
#' @param by Column names present in both inputs. The right-hand keys must be unique.
#' @param variables Variable declarations for every additional column.
#' @param source Description or identifier of the joined source.
#' @return A superdf with a many-to-one left join applied to its full roster.
#' @export
join_superdf <- function(x, y, by, variables, source) {
  validate_superdf(x)
  .assert(
    is.data.frame(y) &&
      length(by) > 0L &&
      all(by %in% names(y)) &&
      all(by %in% names(x)),
    "Join keys must exist in both inputs."
  )
  .assert(
    !anyNA(y[by]) && !anyDuplicated(y[by]),
    "Right-hand join keys must be nonmissing and unique; join would duplicate units."
  )
  .assert(
    length(source) == 1L && nzchar(source),
    "Provide the source of joined measurements."
  )
  added <- setdiff(names(y), by)
  .assert(
    length(added) > 0L && !any(added %in% names(attr(x, "reference"))),
    "Joined columns must be new measurements."
  )
  .assert(
    setequal(vapply(variables, function(v) v$column, character(1)), added),
    "Declare every joined measurement."
  )
  ctx <- context(x)
  ref <- attr(x, "reference")
  .assert(!anyNA(ref[by]), "Join keys must be nonmissing in the roster.")
  .assert(
    all(vapply(
      by,
      function(k) identical(.storage(ref[[k]]), .storage(y[[k]])),
      logical(1)
    )),
    "Join key storage types must agree; convert explicitly before joining."
  )
  # A many-to-one join permits repeated left keys, so match row tuples directly.
  tuples <- function(d) {
    do.call(
      paste,
      c(
        lapply(d[by], function(v) {
          paste0(nchar(as.character(v), type = "bytes"), ":", v)
        }),
        sep = "|"
      )
    )
  }
  positions <- match(tuples(ref), tuples(y))
  current <- match(.keys(.plain(x), ctx), .keys(ref, ctx))
  data <- .plain(x)
  for (v in variables) {
    v$storage <- .storage(y[[v$column]])
    ref[[v$column]] <- y[[v$column]][positions]
    data[[v$column]] <- ref[[v$column]][current]
    ctx$variables[[length(ctx$variables) + 1L]] <- v
  }
  ctx <- .record(
    ctx,
    "join",
    as.character(.json(list(by = .array(by), source = source))),
    nrow(x),
    nrow(x),
    attr(x, "seal"),
    outputs = vapply(variables, function(v) v$id, character(1))
  )
  out <- .new_superdf(data, ctx, ref)
  validate_superdf(out)
  out
}

#' @export
rbind.superdf <- function(..., deparse.level = 1) {
  xs <- list(...)
  .assert(
    all(vapply(xs, inherits, logical(1), "superdf")),
    "Bind only SuperDF views of the same collection."
  )
  lapply(xs, validate_superdf)
  x <- xs[[1]]
  comparable <- function(z) {
    ctx <- context(z)
    ctx$history <- list()
    ctx
  }
  .assert(
    all(vapply(
      xs,
      function(z) {
        identical(comparable(z), comparable(x)) &&
          identical(attr(z, "reference"), attr(x, "reference"))
      },
      logical(1)
    )),
    "Cannot bind different collection contexts. Define the combined study explicitly."
  )
  data <- do.call(base::rbind, lapply(xs, .plain))
  ctx <- .record(
    context(x),
    "bind",
    "Disjoint views of the same roster",
    sum(vapply(xs, nrow, integer(1))),
    nrow(data),
    vapply(xs, function(z) attr(z, "seal"), character(1))
  )
  out <- .new_superdf(data, ctx, attr(x, "reference"))
  validate_superdf(out)
  out
}

#' @export
cbind.superdf <- function(..., deparse.level = 1) {
  stop(
    "Use join_superdf() with explicit keys and measurement definitions.",
    call. = FALSE
  )
}
#' @export
merge.superdf <- function(x, y, ...) {
  stop(
    "Use join_superdf() with explicit keys and measurement definitions.",
    call. = FALSE
  )
}
#' @export
#' @importFrom stats aggregate
aggregate.superdf <- function(x, ...) {
  stop(
    "Aggregation changes the observational unit. Extract data explicitly and declare the resulting dataset and design.",
    call. = FALSE
  )
}
