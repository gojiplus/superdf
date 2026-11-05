#' Recode values into a new measurement with an explicit mapping
#' @param x A studyframe.
#' @param variable A variable_spec for the new measurement.
#' @param input Stable identifier of the source variable.
#' @param from Distinct, nonmissing source codes with types compatible with input.
#' @param to Replacement values, one for each source code. NA is allowed.
#' @param unmatched How to handle other nonmissing values: error, missing, or keep.
#'   Keeping values requires the same storage type as the replacement vector.
#'   Unmapped declared missing codes must retain their missing declarations in
#'   the new variable when values are kept.
#' @param description Explanation of the recode. The mapping is also recorded.
#' @return A new studyframe revision. Original measurements and declared design
#'   variables are preserved. Ordinary NA stays NA; declared numeric missing codes
#'   are recoded only if the explicit mapping includes them.
#' @export
recode_studyframe <- function(
  x,
  variable,
  input,
  from,
  to,
  unmatched = c("error", "missing", "keep"),
  description
) {
  validate_studyframe(x)
  unmatched <- rlang::arg_match(unmatched)
  column <- .col(context(x), input)
  .assert(column %in% names(x), "Recode input must be in the current view.")
  values <- x[[column]]
  .assert(
    length(from) > 0L && length(from) == length(to),
    "Provide equal-length, nonempty source and replacement vectors."
  )
  .assert(
    !anyNA(from) && !anyDuplicated(from),
    "Source codes must be distinct and nonmissing."
  )
  storage <- .storage(values)
  .assert(
    all(vapply(as.list(from), .code_type, logical(1), storage)),
    "Source code types must match the input variable."
  )
  replacement_storage <- .storage(to)
  positions <- match(values, from)
  absent <- is.na(positions) & !is.na(values)
  .assert(
    unmatched != "error" || !any(absent),
    "Some observed values are unmapped; specify their mapping or an unmatched policy."
  )
  result <- to[positions]
  if (unmatched == "keep") {
    .assert(
      identical(storage, replacement_storage),
      "Keeping unmatched values requires matching storage types."
    )
    source <- .choose(context(x)$variables, input, "variable")
    for (entry in source$missing) {
      if (!entry$value %in% from) {
        preserved <- any(vapply(
          variable$missing,
          function(target) {
            identical(target$value, entry$value) &&
              identical(target$reason, entry$reason)
          },
          logical(1)
        ))
        .assert(
          preserved,
          paste(
            "Keeping unmapped declared missing codes requires their original",
            "missing declarations in the new variable, or an explicit mapping."
          )
        )
      }
    }
    result[absent] <- values[absent]
  }
  .assert(
    length(description) == 1L &&
      !is.na(description) &&
      nzchar(trimws(description)),
    "Describe the recode."
  )
  detail <- as.character(.json(list(
    description = description,
    input = input,
    from = .array(from),
    to = .array(to),
    unmatched = unmatched
  )))
  derive_studyframe(x, variable, result, inputs = input, description = detail)
}
