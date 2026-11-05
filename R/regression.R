#' Request a model-based linear association
#'
#' This adapter fits unweighted ordinary least squares with an intercept and
#' numeric predictors. It does not interpret a coefficient as a causal effect or
#' certify population representativeness. Sampling and assignment declarations
#' remain attached to the study but are not used by this estimator.
#'
#' @param outcome Stable numeric outcome identifier.
#' @param predictors Character vector of stable numeric predictor identifiers.
#'   Empty requests an intercept-only model. Create categorical indicators and
#'   interactions as documented studyframe derivations before requesting a model.
#' @param cluster Stable character or integer cluster identifier, or NULL.
#' @param se_type HC2 for unclustered inference; CR2 or stata for clustered
#'   inference. NULL selects HC2 without clusters and CR2 with clusters.
#' @param missing fail or omit. Omission applies jointly to all model variables
#'   and the cluster identifier and does not establish a missing-data assumption.
#' @param target Explicit description of the association and population of
#'   observations to which the model is intended to apply.
#' @param assumptions Nonempty character vector of the analyst's assumptions,
#'   including the justification for independence between observations or clusters
#'   and the interpretation of selection and missingness.
#' @return A portable regression request for check_analysis and run_analysis.
#' @seealso [estimatr::lm_robust()]
#' @export
regression_request <- function(
  outcome,
  predictors = character(),
  cluster = NULL,
  se_type = NULL,
  missing = "fail",
  target,
  assumptions
) {
  if (is.null(se_type)) se_type <- if (is.null(cluster)) "HC2" else "CR2"
  list(
    kind = "regression",
    outcome = outcome,
    predictors = .array(predictors),
    cluster = cluster,
    se_type = se_type,
    missing = missing,
    target = target,
    assumptions = .array(assumptions)
  )
}

.regression_issue <- function(request) {
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  identifier <- function(x) scalar(x) && grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
  array <- function(x, predicate) {
    (is.character(x) || is.list(x)) &&
      is.null(names(x)) && all(vapply(x, predicate, logical(1)))
  }
  fields <- c(
    "kind", "outcome", "predictors", "cluster", "se_type", "missing",
    "target", "assumptions"
  )
  valid <- identical(sort(names(request)), sort(fields)) &&
    identifier(request$outcome) &&
    array(request$predictors, identifier) &&
    !anyDuplicated(.strings(request$predictors)) &&
    !request$outcome %in% .strings(request$predictors) &&
    (is.null(request$cluster) || identifier(request$cluster)) &&
    scalar(request$se_type) &&
    request$se_type %in% (if (is.null(request$cluster)) "HC2" else c("CR2", "stata")) &&
    scalar(request$missing) && request$missing %in% c("fail", "omit") &&
    scalar(request$target) && nzchar(trimws(request$target)) &&
    length(request$assumptions) > 0L &&
    array(request$assumptions, function(x) scalar(x) && nzchar(trimws(x)))
  if (isTRUE(valid)) {
    return(NULL)
  }
  analysis_issue(
    "invalid_request", "invalid", "request",
    paste(
      "Regression requires numeric-variable identifiers, a supported variance",
      "choice, an explicit target and assumptions, and no design-weight or assignment options."
    ),
    remedy = "Construct a request with regression_request()."
  )
}

.prepare_regression <- function(x, request) {
  ctx <- context(x)
  predictors <- .strings(request$predictors)
  ids <- unique(c(request$outcome, predictors, request$cluster))
  registered <- vapply(ctx$variables, function(v) v$id, character(1))
  .need(
    all(ids %in% registered),
    "A requested regression variable is not registered.",
    code = "regression_variable_unknown", category = "missing"
  )
  .present(x, ids)
  reference <- attr(x, "reference")
  positions <- match(.keys(.plain(x), ctx), .keys(reference, ctx))
  data <- .engine_data(x)[positions, ids, drop = FALSE]
  numeric_ids <- c(request$outcome, predictors)
  .need(
    all(vapply(data[numeric_ids], is.numeric, logical(1))),
    "Regression outcomes and predictors must be numeric; derive explicit indicators for categories.",
    code = "regression_type_unsupported", category = "unsupported"
  )
  if (!is.null(request$cluster)) {
    clusters <- data[[request$cluster]]
    .need(
      is.character(clusters) || is.integer(clusters),
      "Cluster identifiers must be character or integer.",
      code = "regression_cluster_type", category = "unsupported"
    )
  }
  complete <- stats::complete.cases(data)
  absent <- sum(!complete)
  .need(
    request$missing == "omit" || absent == 0L,
    "Missing model variables: choose a missing-data policy explicitly.",
    code = "missing_model_values", category = "choice"
  )
  data <- data[complete, , drop = FALSE]
  terms <- if (length(predictors)) paste0("`", predictors, "`") else "1"
  formula <- stats::reformulate(terms, response = paste0("`", request$outcome, "`"))
  design <- stats::model.matrix(stats::reformulate(terms), data = data)
  .need(
    nrow(design) > ncol(design),
    "Regression requires more complete observations than coefficients.",
    code = "regression_replication_insufficient", category = "unsupported"
  )
  .need(
    qr(design)$rank == ncol(design),
    "Regression predictors and intercept must have full column rank.",
    code = "regression_rank_deficient", category = "unsupported"
  )
  cluster_count <- NULL
  args <- list(formula = formula, data = data, se_type = request$se_type)
  if (!is.null(request$cluster)) {
    cluster_count <- length(unique(data[[request$cluster]]))
    .need(
      cluster_count >= 2L,
      "Clustered regression requires at least two observed clusters.",
      code = "regression_clusters_insufficient", category = "unsupported"
    )
    args$clusters <- as.name(request$cluster)
  }
  list(
    args = args,
    analysis = list(
      dataset = ctx$dataset,
      inference = "model_based_association",
      target = request$target,
      interpretation = paste(
        "A linear association under the supplied model assumptions;",
        "this does not establish causal identification or population representativeness."
      ),
      model_assumptions = request$assumptions,
      outcome = request$outcome,
      predictors = request$predictors,
      intercept = TRUE,
      cluster = request$cluster,
      cluster_count = cluster_count,
      se_type = request$se_type,
      missing = request$missing,
      roster_rows = nrow(reference),
      current_rows = nrow(x),
      used_rows = nrow(data),
      missing_rows = absent,
      current_keys = .array(.keys(.plain(x), ctx)),
      history = ctx$history,
      estimator = "lm_robust",
      backend = "estimatr",
      backend_version = as.character(utils::packageVersion("estimatr"))
    )
  )
}

.validate_regression_inference <- function(fit, request) {
  count <- 1L + length(request$predictors)
  finite <- function(x) is.numeric(x) && length(x) == count && all(is.finite(x))
  valid <- all(vapply(
    list(stats::coef(fit), fit$std.error, fit$df, fit$p.value, fit$conf.low, fit$conf.high),
    finite, logical(1)
  )) && all(fit$std.error >= 0) && all(fit$df > 0) &&
    all(fit$p.value >= 0 & fit$p.value <= 1) && all(fit$conf.low <= fit$conf.high)
  .need(
    valid,
    "The backend could not provide finite estimates and uncertainty for every regression coefficient.",
    code = "inference_unavailable", category = "unsupported", path = request$outcome,
    remedy = "Review model rank, outcome variation, numerical scale, and independent replication."
  )
  invisible(TRUE)
}
