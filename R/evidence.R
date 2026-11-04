#' Identify source evidence without certifying its claims
#' @param id Stable evidence identifier.
#' @param uri Source location or identifier. Sources are never fetched automatically.
#' @param sha256 SHA-256 of the identified artifact bytes, or NULL when unavailable.
#' @param producer Person or organization supplying the artifact.
#' @param captured_at UTC capture time in YYYY-MM-DDTHH:MM:SSZ format, or NULL.
#' @param version Artifact or instrument version, or NULL.
#' @param description Description of the source and its scope.
#' @return A portable evidence record. A hash identifies bytes, not scientific truth.
#' @export
evidence_spec <- function(
  id,
  uri,
  sha256 = NULL,
  producer = "",
  captured_at = NULL,
  version = NULL,
  description = ""
) {
  list(
    id = id,
    uri = uri,
    sha256 = sha256,
    producer = producer,
    captured_at = captured_at,
    version = version,
    description = description
  )
}

#' Declare what the attached roster represents
#' @param kind unknown, received_responses, invited_units, sampled_units, or randomized_units.
#' @param source Explanation of the roster's source and completeness assertion.
#' @param completeness unknown, partial, or asserted_complete. Assertions are not independently verified.
#' @param scope Sampling or assignment declaration ID to which completeness applies.
#' @param evidence Character vector of evidence identifiers supporting the declaration.
#' @return A portable roster declaration.
#' @export
roster_spec <- function(
  kind = "unknown",
  source = "",
  completeness = "unknown",
  scope = NULL,
  evidence = character()
) {
  list(
    kind = kind,
    source = source,
    completeness = completeness,
    scope = scope,
    evidence = .array(evidence)
  )
}

#' Map a measurement to a versioned instrument artifact
#' @param source Evidence identifier for the actual instrument artifact.
#' @param question Vendor question identifier.
#' @param subquestion Vendor subquestion identifier, or NULL.
#' @return An instrument mapping for variable_spec.
#' @export
instrument_ref <- function(source, question, subquestion = NULL) {
  list(source = source, question = question, subquestion = subquestion)
}

#' Attach attributed evidence and unresolved import diagnostics
#' @param x A superdf.
#' @param evidence List of evidence_spec records. Reusing an ID with different content fails.
#' @param diagnostics List of analysis_issue records.
#' @return A new superdf revision. Existing evidence is never overwritten.
#' @export
attach_evidence <- function(x, evidence, diagnostics = list()) {
  validate_superdf(x)
  ctx <- context(x)
  for (item in evidence) {
    ids <- vapply(ctx$evidence, function(z) z$id, character(1))
    found <- match(item$id, ids)
    if (!is.na(found)) {
      .assert(
        identical(ctx$evidence[[found]], item),
        "Conflicting evidence for an existing ID."
      )
    } else {
      ctx$evidence[[length(ctx$evidence) + 1L]] <- item
    }
  }
  ctx$diagnostics <- c(ctx$diagnostics, diagnostics)
  ctx <- .record(
    ctx,
    "attach_evidence",
    "Attach source artifacts and diagnostics",
    nrow(x),
    nrow(x),
    attr(x, "seal")
  )
  out <- .new_superdf(.plain(x), ctx, attr(x, "reference"))
  validate_superdf(out)
  out
}
