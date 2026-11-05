#' Describe an analysis requirement or import uncertainty
#' @param code Stable machine-readable issue code.
#' @param category invalid, missing, choice, unsupported, or dependency.
#' @param path Schema path, declaration ID, or variable ID affected.
#' @param message Human-readable explanation.
#' @param sources Character vector of supporting evidence IDs.
#' @param remedy Specific action that could resolve the issue.
#' @return A portable issue record.
#' @export
analysis_issue <- function(
  code,
  category,
  path,
  message,
  sources = character(),
  remedy = ""
) {
  list(
    code = code,
    category = category,
    path = path,
    message = message,
    sources = .array(sources),
    remedy = remedy
  )
}

.analysis_abort <- function(issues) {
  if (!is.null(issues$code)) {
    issues <- list(issues)
  }
  condition <- structure(
    list(
      message = paste(
        vapply(issues, function(z) z$message, character(1)),
        collapse = "\n"
      ),
      call = NULL,
      issues = issues
    ),
    class = c("studyframe_analysis_error", "error", "condition")
  )
  stop(condition)
}

.need <- function(
  ok,
  message,
  code = "invalid_design",
  category = "invalid",
  path = "",
  sources = character(),
  remedy = "Review the referenced data and design declaration."
) {
  if (!isTRUE(ok)) {
    .analysis_abort(analysis_issue(
      code,
      category,
      path,
      message,
      sources,
      remedy
    ))
  }
}

#' Request a supported analysis with explicit design choices
#' @param kind mean or effect. These target a declared population mean or randomized-roster effect respectively.
#' @param outcome Stable numeric outcome identifier.
#' @param weight Named weight declaration for a mean.
#' @param assignment Named assignment declaration for an effect.
#' @param domain TRUE explicitly declares a restricted mean analysis to be a population domain.
#' @param missing fail or omit for means. Effects require complete numeric outcomes.
#' @return A plain analysis request. check_analysis validates it against a study.
#' @export
analysis_request <- function(
  kind,
  outcome,
  weight = NULL,
  assignment = NULL,
  domain = FALSE,
  missing = "fail"
) {
  list(
    kind = kind,
    outcome = outcome,
    weight = weight,
    assignment = assignment,
    domain = domain,
    missing = missing
  )
}

.request_issue <- function(request) {
  if (is.list(request) && identical(request$kind, "regression")) {
    return(.regression_issue(request))
  }
  fields <- c("kind", "outcome", "weight", "assignment", "domain", "missing")
  scalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  identifier <- function(x) scalar(x) && grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
  valid <- is.list(request) &&
    identical(sort(names(request)), sort(fields)) &&
    scalar(request$kind) &&
    request$kind %in% c("mean", "effect") &&
    identifier(request$outcome) &&
    is.logical(request$domain) &&
    length(request$domain) == 1L &&
    !is.na(request$domain) &&
    scalar(request$missing) &&
    request$missing %in% c("fail", "omit") &&
    (is.null(request$weight) || identifier(request$weight)) &&
    (is.null(request$assignment) || identifier(request$assignment))
  if (!valid) {
    return(analysis_issue(
      "invalid_request",
      "invalid",
      "request",
      "Use analysis_request with a supported kind, identifiers, domain, and missing policy.",
      remedy = "Construct a request with analysis_request()."
    ))
  }
  if (request$kind == "mean" && !is.null(request$assignment)) {
    return(analysis_issue(
      "combined_inference_unsupported",
      "unsupported",
      "request.assignment",
      "The mean adapter does not combine sampling and assignment inference.",
      remedy = "Request the mean using its sampling weight, or a separate randomized-roster effect."
    ))
  }
  if (
    request$kind == "effect" &&
      (!is.null(request$weight) || request$domain || request$missing != "fail")
  ) {
    return(analysis_issue(
      "effect_policy_unsupported",
      "unsupported",
      "request",
      "The effect adapter requires a full roster, complete outcomes, and no sampling-weight extrapolation.",
      remedy = "Use the complete randomized roster, or a separately supported estimator."
    ))
  }
  NULL
}

.prepare_request <- function(x, request) {
  issue <- .request_issue(request)
  if (!is.null(issue)) {
    return(list(ok = FALSE, issues = list(issue)))
  }
  valid <- tryCatch(
    {
      validate_studyframe(x)
      NULL
    },
    error = identity
  )
  if (inherits(valid, "error")) {
    return(list(
      ok = FALSE,
      issues = list(analysis_issue(
        "invalid_context",
        "invalid",
        "context",
        conditionMessage(valid),
        remedy = "Repair or reimport the dataset without bypassing validation."
      ))
    ))
  }
  ctx <- context(x)
  issues <- new.env(parent = emptyenv())
  issues$items <- list()
  add <- function(
    code,
    category,
    path,
    message,
    sources = character(),
    remedy = "Supply the referenced declaration or evidence."
  ) {
    issues$items[[length(issues$items) + 1L]] <- analysis_issue(
      code,
      category,
      path,
      message,
      sources,
      remedy
    )
  }
  variables <- vapply(ctx$variables, function(v) v$id, character(1))
  if (!request$outcome %in% variables) {
    add(
      "outcome_unknown",
      "missing",
      request$outcome,
      "The requested outcome is not registered."
    )
  } else if (!.col(ctx, request$outcome) %in% names(x)) {
    add(
      "outcome_absent",
      "missing",
      request$outcome,
      "The requested outcome is absent from the current view."
    )
  }
  if (request$kind == "mean") {
    ids <- vapply(ctx$weights, function(w) w$id, character(1))
    if (is.null(request$weight) || !request$weight %in% ids) {
      add(
        "weight_required",
        "choice",
        "request.weight",
        "Select an existing weight identifier explicitly."
      )
    }
    backend <- "survey"
  } else if (request$kind == "regression") {
    backend <- "estimatr"
  } else {
    ids <- vapply(ctx$assignments, function(a) a$id, character(1))
    if (is.null(request$assignment) || !request$assignment %in% ids) {
      add(
        "assignment_required",
        "choice",
        "request.assignment",
        "Select an existing assignment identifier explicitly."
      )
    }
    roster <- ctx$roster
    if (
      roster$kind != "randomized_units" ||
        roster$completeness != "asserted_complete" ||
        is.null(roster$scope) ||
        !identical(roster$scope, request$assignment)
    ) {
      add(
        "randomized_roster_required",
        "missing",
        "roster",
        paste(
          "Effect inference requires a sourced, asserted-complete randomized roster",
          "scoped to the selected assignment; response rows alone cannot establish this."
        ),
        .strings(roster$evidence),
        "Import the authoritative assignment roster and declare its source, evidence, and assignment scope."
      )
    }
    backend <- "estimatr"
  }
  if (!requireNamespace(backend, quietly = TRUE)) {
    add(
      "missing_dependency",
      "dependency",
      backend,
      paste("Install the", backend, "package for this operation."),
      remedy = "Install the required package on the execution host."
    )
  }
  if (length(issues$items)) {
    return(list(ok = FALSE, issues = issues$items))
  }
  prepared <- tryCatch(
    {
      if (request$kind == "mean") {
        .prepare_mean(
          x,
          request$outcome,
          request$weight,
          request$domain,
          request$missing
        )
      } else if (request$kind == "regression") {
        .prepare_regression(x, request)
      } else {
        .prepare_effect(x, request$outcome, request$assignment)
      }
    },
    studyframe_analysis_error = identity,
    error = identity
  )
  if (inherits(prepared, "studyframe_analysis_error")) {
    return(list(ok = FALSE, issues = prepared$issues))
  }
  if (inherits(prepared, "error")) {
    return(list(
      ok = FALSE,
      issues = list(analysis_issue(
        "backend_preparation_failed",
        "invalid",
        backend,
        conditionMessage(prepared),
        remedy = "Review the backend's design requirements and the supplied design values."
      ))
    ))
  }
  list(ok = TRUE, issues = list(), prepared = prepared)
}

#' Check requirements without fitting an outcome model
#' @param x A studyframe.
#' @param request An analysis_request or regression_request.
#' @return A list with ok, issues, request, and input_digest. Survey designs may be
#'   constructed, but no outcome estimator is called. Scientific assertions
#'   remain assertions even when the request passes.
#' @export
check_analysis <- function(x, request) {
  checked <- .prepare_request(x, request)
  list(
    ok = checked$ok,
    issues = checked$issues,
    request = request,
    input_digest = attr(x, "seal", exact = TRUE)
  )
}

.validate_inference <- function(fit, request, prepared) {
  if (request$kind == "regression") {
    return(.validate_regression_inference(fit, request))
  }
  scalar <- function(value) {
    is.numeric(value) && length(value) == 1L && is.finite(value)
  }
  estimate <- stats::coef(fit)
  if (request$kind == "mean") {
    standard_error <- as.numeric(survey::SE(fit))
    df <- prepared$analysis$design_df
  } else {
    standard_error <- fit$std.error
    df <- fit$df
  }
  require_inference <- function(ok) {
    .need(
      ok,
      "The backend could not provide finite estimates and uncertainty for the declared inference.",
      code = "inference_unavailable",
      category = "unsupported",
      path = request$outcome,
      remedy = paste(
        "Review outcome variation, numerical scale, and independent replication;",
        "no valid interval is available from this adapter."
      )
    )
  }
  require_inference(
    scalar(estimate) &&
      scalar(standard_error) &&
      standard_error >= 0 &&
      scalar(df) &&
      df > 0
  )
  interval <- if (request$kind == "mean") {
    stats::confint(fit, level = 0.95, df = df)
  } else {
    require_inference(
      scalar(fit$p.value) && fit$p.value >= 0 && fit$p.value <= 1
    )
    c(fit$conf.low, fit$conf.high)
  }
  require_inference(
    is.numeric(interval) &&
      length(interval) == 2L &&
      all(is.finite(interval)) &&
      interval[[1]] <= interval[[2]]
  )
  invisible(TRUE)
}

#' Fit a supported request after preparing its declared design
#' @param x A studyframe.
#' @param request An analysis_request or regression_request.
#' @return A studyframe_result containing the native fit, analysis receipt, and context.
#'   Failure raises a studyframe_analysis_error condition with structured issues.
#'   Undefined estimates, uncertainty, or intervals raise an inference_unavailable
#'   issue instead of returning a successful analysis receipt.
#' @export
run_analysis <- function(x, request) {
  checked <- .prepare_request(x, request)
  if (!checked$ok) {
    .analysis_abort(checked$issues)
  }
  prepared <- checked$prepared
  if (request$kind == "mean") {
    old <- options(
      survey.lonely.psu = prepared$weight$variance$lonely_psu,
      survey.adjust.domain.lonely = FALSE
    )
    on.exit(options(old), add = TRUE)
  }
  fit <- tryCatch(
    {
      if (request$kind == "mean") {
        survey::svymean(
          .formula(request$outcome),
          prepared$design,
          na.rm = request$missing == "omit"
        )
      } else if (request$kind == "regression") {
        do.call(estimatr::lm_robust, prepared$args)
      } else {
        do.call(estimatr::difference_in_means, prepared$args)
      }
    },
    error = function(e) {
      .analysis_abort(analysis_issue(
        "backend_estimation_failed",
        "invalid",
        "analysis",
        conditionMessage(e),
        remedy = "Review outcome variation and backend estimability requirements."
      ))
    }
  )
  .validate_inference(fit, request, prepared)
  analysis <- prepared$analysis
  ctx <- context(x)
  analysis$request <- request
  analysis$roster <- ctx$roster
  analysis$evidence <- ctx$evidence
  analysis$confidence_interval <- list(
    method = "Student t",
    level = if (request$kind == "mean") 0.95 else 1 - fit$alpha,
    df = if (request$kind == "mean") analysis$design_df else unname(fit$df)
  )
  if (request$kind == "mean") {
    analysis$selected_weight <- .choose(ctx$weights, request$weight, "weight")
    sid <- analysis$selected_weight$sampling
    analysis$selected_design <- if (is.null(sid)) {
      NULL
    } else {
      .choose(ctx$sampling, sid, "sampling")
    }
    analysis$target <- if (is.null(analysis$selected_design)) {
      "Population specified by supplied weights"
    } else {
      analysis$selected_design$population
    }
  } else if (request$kind == "regression") {
    analysis$selected_weight <- NULL
    analysis$selected_design <- NULL
  } else {
    analysis$selected_weight <- NULL
    analysis$selected_design <- .choose(
      ctx$assignments,
      request$assignment,
      "assignment"
    )
  }
  analysis$assumptions <- list(
    declarations = "Supplied assertions, not independently verified facts.",
    roster_completeness = if (ctx$roster$completeness == "asserted_complete") {
      "asserted_not_independently_verified"
    } else {
      ctx$roster$completeness
    },
    missingness = "Omission, when requested, does not establish a nonresponse assumption."
  )
  .analysis_result(fit, x, analysis)
}

#' Estimate a mean using a declared survey design
#' @param x A studyframe.
#' @param outcome Stable numeric outcome identifier.
#' @param weight Explicit weight declaration identifier.
#' @param domain Interpret a restricted view as a population domain.
#' @param missing fail or omit. Omission is recorded and does not justify nonresponse assumptions.
#' @return A studyframe_result; uses the same requirements as check_analysis.
#'   Its receipt specifies 95 percent Student t intervals using the design degrees
#'   of freedom, which must be positive and finite. Obtain those intervals with stats::confint(result$fit,
#'   df = result$analysis$design_df); survey's default confint uses normal intervals.
#' @export
estimate_mean <- function(
  x,
  outcome,
  weight,
  domain = FALSE,
  missing = c("fail", "omit")
) {
  run_analysis(
    x,
    analysis_request(
      "mean",
      outcome,
      weight = weight,
      domain = domain,
      missing = match.arg(missing)
    )
  )
}

#' Estimate an effect on a declared complete randomized roster
#' @param x A studyframe with a sourced asserted-complete randomized roster.
#' @param outcome Stable numeric outcome identifier.
#' @param assignment Explicit assignment declaration identifier.
#' @return A studyframe_result. Supports two-arm complete randomization, including
#'   blocks and clusters, with constant probability and complete outcomes.
#' @export
estimate_effect <- function(x, outcome, assignment) {
  run_analysis(x, analysis_request("effect", outcome, assignment = assignment))
}
