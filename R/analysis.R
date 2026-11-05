.require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .analysis_abort(analysis_issue(
      "missing_dependency",
      "dependency",
      package,
      paste("Install the", package, "package for this operation."),
      remedy = "Install the required package on the execution host."
    ))
  }
}
.choose <- function(records, id, kind) {
  selected <- which(vapply(
    records,
    function(z) identical(z$id, id),
    logical(1)
  ))
  .need(
    length(selected) == 1L,
    paste("Select an existing", kind, "identifier explicitly."),
    code = "declaration_required",
    category = "choice",
    path = kind
  )
  records[[selected]]
}
.engine_data <- function(x) {
  ctx <- context(x)
  data <- attr(x, "reference")
  for (v in ctx$variables) {
    for (code in v$missing) {
      data[[v$column]][data[[v$column]] %in% code$value] <- NA
    }
  }
  names(data) <- vapply(
    names(data),
    function(column) {
      ctx$variables[[which(vapply(
        ctx$variables,
        function(v) identical(v$column, column),
        logical(1)
      ))]]$id
    },
    character(1)
  )
  data
}
.present <- function(x, ids) {
  for (id in ids) {
    .need(
      .col(context(x), id) %in% names(x),
      paste("Required variable is absent from the current view:", id),
      code = "variable_absent",
      category = "missing",
      path = id
    )
  }
}
.formula <- function(ids) stats::reformulate(paste0("`", ids, "`"))
.scope <- function(data, rules) {
  selected <- rep(TRUE, nrow(data))
  for (rule in rules) {
    selected <- selected &
      !is.na(data[[rule$variable]]) &
      data[[rule$variable]] %in% .strings(rule$values)
  }
  selected
}
.known <- function(declaration) {
  .need(
    declaration$phase == "realized",
    "Planned declarations do not establish the realized design.",
    code = "design_not_realized",
    category = "missing",
    path = declaration$id,
    sources = .strings(declaration$evidence)
  )
  .need(
    declaration$status != "unknown",
    "The requested declaration is unknown; provide its documented facts or explicit assumptions.",
    code = "declaration_unknown",
    category = "missing",
    path = declaration$id,
    sources = .strings(declaration$evidence)
  )
  if (!is.null(declaration$state)) {
    .need(
      declaration$state == "known",
      "The requested collection mechanism is not known.",
      code = "mechanism_unknown",
      category = "missing",
      path = declaration$id,
      sources = .strings(declaration$evidence)
    )
  }
}
.numeric_column <- function(data, id, positive = TRUE) {
  values <- data[[id]]
  .need(
    is.numeric(values) &&
      !anyNA(values) &&
      all(is.finite(values)) &&
      all(if (positive) values > 0 else values >= 0),
    paste("Invalid numeric design values in", id)
  )
}

#' Translate an explicitly selected survey specification
#' @param x A studyframe.
#' @param weight Identifier of a weight declaration; never chosen implicitly.
#' @param domain TRUE explicitly interprets a restricted view as a population
#'   domain. The backend design is constructed on the weight's full applicable
#'   roster before subsetting. This does not establish that a restriction is ignorable.
#' @return A survey.design or svyrep.design, with a studyframe_analysis attribute.
#'   Calling survey functions directly uses their own global options; estimate_mean
#'   applies the declared single-PSU policy and records it.
#' @export
as_survey <- function(x, weight, domain = FALSE) {
  validate_studyframe(x)
  .require("survey")
  ctx <- context(x)
  w <- .choose(ctx$weights, weight, "weight")
  .known(w)
  .need(
    w$role %in% c("base", "final"),
    "Survey inference requires a base or final sampling weight."
  )
  full <- .engine_data(x)
  applicable <- .scope(full, w$applies)
  roster_keys <- .keys(attr(x, "reference"), ctx)
  current_keys <- .keys(.plain(x), ctx)
  .need(
    all(current_keys %in% roster_keys[applicable]),
    "The selected weight is not applicable to every current observation."
  )
  .need(
    any(applicable) && length(current_keys) > 0L,
    "No applicable observations."
  )
  restricted <- !setequal(current_keys, roster_keys[applicable])
  .need(
    !restricted || isTRUE(domain),
    "Restricted view: declare domain = TRUE, or use the full applicable roster.",
    code = "domain_required",
    category = "choice"
  )
  data <- full[applicable, , drop = FALSE]
  variance <- w$variance
  refs <- c(
    w$variable,
    .strings(variance$replicates),
    vapply(w$applies, function(r) r$variable, character(1))
  )
  .present(x, refs)
  .numeric_column(data, w$variable)
  if (variance$method == "taylor") {
    s <- .choose(ctx$sampling, w$sampling, "sampling design")
    .known(s)
    .need(
      length(s$stages) > 0L,
      "Taylor variance requires declared sampling stages."
    )
    .need(
      length(w$adjustments) == 0L,
      "Adjusted weights with Taylor variance are unsupported; documented replicate weights are an alternative.",
      code = "adjusted_taylor_unsupported",
      category = "unsupported"
    )
    stages <- s$stages
    replacement <- vapply(stages, function(z) z$replacement, character(1))
    .need(
      all(vapply(stages, function(z) z$method == "srs", logical(1))),
      "This adapter supports SRS stages only; PPS and unknown methods are retained but not translated.",
      code = "sampling_method_unsupported",
      category = "unsupported"
    )
    .need(
      all(replacement != "unknown"),
      "Sampling replacement must be declared.",
      code = "replacement_unknown",
      category = "missing"
    )
    .need(
      length(stages) == 1L || all(replacement == "without"),
      "Multistage translation requires without-replacement stages with FPC information at every stage.",
      code = "multistage_unsupported",
      category = "unsupported"
    )
    ids <- character()
    strata <- character()
    fpcs <- character()
    probabilities <- character()
    for (i in seq_along(stages)) {
      stage <- stages[[i]]
      stage_refs <- c(
        stage$unit,
        stage$strata,
        stage$probability,
        stage$fpc$variable
      )
      .present(x, stage_refs)
      .need(!anyNA(data[[stage$unit]]), "Sampling units cannot be missing.")
      ids <- c(ids, stage$unit)
      stratum <- stage$strata
      if (is.null(stratum)) {
        stratum <- paste0(".studyframe_stratum", i)
        data[[stratum]] <- 1L
      }
      .need(!anyNA(data[[stratum]]), "Sampling strata cannot be missing.")
      strata <- c(strata, stratum)
      if (stage$replacement == "with") {
        .need(
          is.null(stage$fpc),
          "With-replacement sampling cannot carry an FPC in this adapter."
        )
      } else {
        .need(
          !is.null(stage$fpc),
          "Without-replacement sampling requires explicit finite-population information.",
          code = "fpc_required",
          category = "missing"
        )
        .numeric_column(data, stage$fpc$variable)
        f <- data[[stage$fpc$variable]]
        parent_columns <- unique(c(utils::head(ids, -1L), strata))
        parent <- do.call(
          paste,
          c(
            lapply(data[parent_columns], function(z) {
              paste0(nchar(as.character(z)), ":", z)
            }),
            sep = "|"
          )
        )
        count <- stats::ave(seq_len(nrow(data)), parent, FUN = function(rows) {
          length(unique(data[[stage$unit]][rows]))
        })
        .need(
          all(vapply(
            split(f, parent),
            function(z) length(unique(z)) == 1L,
            logical(1)
          )),
          "FPC information must be constant within each sampling stratum and parent unit."
        )
        if (stage$fpc$kind == "sampling_fraction") {
          .need(all(f <= 1), "FPC sampling fractions must be at most one.")
          population <- count / f
        } else {
          .need(
            all(f >= 1 & f == floor(f)),
            "FPC population sizes must be positive integers."
          )
          .need(
            all(f >= count),
            "FPC population size is smaller than the sampled unit count."
          )
          population <- f
        }
        fpc_column <- paste0(".studyframe_population", i)
        data[[fpc_column]] <- population
        fpcs <- c(fpcs, fpc_column)
      }
      if (!is.null(stage$probability)) {
        .numeric_column(data, stage$probability)
        .need(
          all(data[[stage$probability]] <= 1),
          "Inclusion probabilities must not exceed one."
        )
        probabilities <- c(probabilities, stage$probability)
      }
    }
    if (w$role == "base" && length(probabilities) == length(stages)) {
      inverse <- 1 / Reduce(`*`, data[probabilities])
      .need(
        isTRUE(all.equal(
          data[[w$variable]],
          inverse,
          tolerance = 1e-8,
          check.attributes = FALSE
        )),
        "Base weights disagree with declared stage inclusion probabilities."
      )
    }
    args <- list(
      ids = .formula(ids),
      strata = .formula(strata),
      weights = .formula(w$variable),
      data = data,
      nest = TRUE
    )
    if (length(fpcs)) {
      .need(
        any(as.matrix(data[fpcs]) > 1),
        "A census consisting entirely of singleton strata is not supported by this survey adapter."
      )
      args$fpc <- .formula(fpcs)
    }
    design <- do.call(survey::svydesign, args)
  } else {
    replicates <- .strings(variance$replicates)
    for (id in replicates) {
      .numeric_column(data, id, positive = FALSE)
    }
    .need(
      all(vapply(data[replicates], function(z) sum(z) > 0, logical(1))),
      "Each replicate must have positive total weight."
    )
    if (variance$method == "other") {
      .need(
        !is.null(variance$scale) &&
          length(variance$rscales) == length(replicates),
        "Other replicate variance requires scale and one rscale per replicate.",
        code = "replicate_scaling_required",
        category = "missing"
      )
    }
    if (variance$method %in% c("BRR", "Fay")) {
      .need(
        is.null(variance$scale) && length(variance$rscales) == 0L,
        "BRR/Fay scaling is determined by the method; explicit scale/rscales would be ignored by survey.",
        code = "replicate_scaling_unsupported",
        category = "unsupported"
      )
    }
    args <- list(
      variables = data,
      repweights = data[replicates],
      weights = data[[w$variable]],
      type = variance$method,
      combined.weights = variance$combined,
      mse = variance$mse
    )
    if (!is.null(variance$scale)) {
      args$scale <- variance$scale
    }
    if (length(variance$rscales)) {
      args$rscales <- .strings(variance$rscales)
    }
    if (!is.null(variance$rho)) {
      args$rho <- variance$rho
    }
    design <- do.call(survey::svrepdesign, args)
  }
  positions <- match(current_keys, roster_keys[applicable])
  design <- design[positions, ]
  attr(design, "studyframe_analysis") <- list(
    dataset = ctx$dataset,
    weight = weight,
    domain = restricted,
    roster_rows = nrow(full),
    applicable_rows = nrow(data),
    current_rows = nrow(x),
    current_keys = .array(current_keys),
    variance = variance,
    backend = "survey",
    backend_version = as.character(utils::packageVersion("survey"))
  )
  design
}

.analysis_result <- function(fit, x, analysis) {
  analysis$receipt_version <- "0.3.0"
  analysis$input_digest <- attr(x, "seal")
  analysis$digest_method <- paste(
    "SHA-256 of R serialization version 2;",
    "character columns normalized to UTF-8"
  )
  analysis$studyframe_version <- as.character(utils::packageVersion(
    "studyframe"
  ))
  analysis$r_version <- as.character(getRversion())
  structure(
    list(
      result_version = "0.3.0",
      fit = fit,
      analysis = analysis,
      context = context(x)
    ),
    class = "studyframe_result"
  )
}

.prepare_mean <- function(
  x,
  outcome,
  weight,
  domain = FALSE,
  missing = c("fail", "omit")
) {
  missing <- match.arg(missing)
  validate_studyframe(x)
  .present(x, outcome)
  ctx <- context(x)
  variable <- .choose(ctx$variables, outcome, "outcome")
  w <- .choose(ctx$weights, weight, "weight")
  .need(
    variable$storage %in% c("integer", "number"),
    "The mean adapter requires a numeric outcome."
  )
  if (!is.null(variable$wave)) {
    .need(
      variable$wave %in% .strings(w$waves),
      "The weight does not declare support for the outcome's measurement wave.",
      code = "weight_wave_mismatch",
      category = "invalid"
    )
  }
  design <- as_survey(x, weight, domain)
  absent <- sum(is.na(design$variables[[outcome]]))
  .need(
    missing != "fail" || absent == 0L,
    "Missing outcomes: choose a missing-data policy explicitly.",
    code = "missing_outcomes",
    category = "choice"
  )
  .need(absent < nrow(x), "Every outcome is missing.")
  analysis <- attr(design, "studyframe_analysis")
  analysis$outcome <- outcome
  analysis$missing <- missing
  analysis$missing_rows <- absent
  analysis$used_rows <- nrow(x) - absent
  analysis$design_df <- survey::degf(design)
  .need(
    length(analysis$design_df) == 1L &&
      is.finite(analysis$design_df) &&
      analysis$design_df > 0,
    "Student t mean inference requires positive finite design degrees of freedom.",
    code = "design_df_insufficient",
    category = "unsupported",
    path = w$id,
    sources = .strings(w$evidence),
    remedy = paste(
      "Review independent sampling units or replicate-weight rank;",
      "this adapter cannot form its declared interval for this design."
    )
  )
  analysis$estimator <- "svymean"
  analysis$adjust_domain_lonely <- FALSE
  list(design = design, weight = w, analysis = analysis)
}

.prepare_effect <- function(x, outcome, assignment) {
  validate_studyframe(x)
  .require("estimatr")
  ctx <- context(x)
  a <- .choose(ctx$assignments, assignment, "assignment")
  .known(a)
  .need(
    a$mechanism == "complete" && !is.null(a$prob),
    "The effect adapter requires complete randomization with a declared probability.",
    code = "assignment_method_unsupported",
    category = "unsupported"
  )
  .need(
    length(ctx$units$keys) == 1L,
    "Repeated-measure/compound-key effect analyses require a new analysis dataset; this adapter does not support them.",
    code = "compound_effect_unsupported",
    category = "unsupported"
  )
  .present(x, c(outcome, a$treatment, a$unit, a$blocks))
  ref <- attr(x, "reference")
  .need(
    setequal(.keys(.plain(x), ctx), .keys(ref, ctx)),
    "Attrition or restriction: the effect adapter requires the full randomized roster.",
    code = "randomized_units_absent",
    category = "invalid"
  )
  data <- .engine_data(x)
  .need(
    is.numeric(data[[outcome]]) && !anyNA(data[[outcome]]),
    "Missing or nonnumeric outcomes: the effect adapter requires complete numeric outcomes.",
    code = "effect_outcomes_incomplete",
    category = "missing"
  )
  z <- data[[a$treatment]]
  conditions <- .strings(a$conditions)
  .need(
    !anyNA(z) && all(z %in% conditions),
    "Assignment codes disagree with declared conditions."
  )
  .need(!anyNA(data[[a$unit]]), "Assignment units cannot be missing.")
  groups <- split(seq_len(nrow(data)), data[[a$unit]])
  for (rows in groups) {
    .need(
      length(unique(z[rows])) == 1L,
      "Treatment varies within an assignment unit."
    )
    if (!is.null(a$blocks)) {
      .need(
        !anyNA(data[[a$blocks]][rows]) &&
          length(unique(data[[a$blocks]][rows])) == 1L,
        "Assignment units must nest in blocks."
      )
    }
  }
  units <- vapply(groups, function(rows) rows[[1]], integer(1))
  block <- if (is.null(a$blocks)) {
    rep("all", length(units))
  } else {
    data[[a$blocks]][units]
  }
  allocation <- split(units, block)
  sizes <- lengths(allocation)
  paired <- !is.null(a$blocks) && all(sizes == 2L)
  .need(
    is.null(a$blocks) || !any(sizes == 2L) || paired,
    "Mixed paired and larger blocks require an explicit variance adapter.",
    code = "mixed_block_sizes_unsupported",
    category = "unsupported",
    path = a$id,
    sources = .strings(a$evidence)
  )
  .need(
    !paired || length(allocation) >= 2L,
    "Matched-pair inference requires at least two independent pairs.",
    code = "assignment_replication_insufficient",
    path = a$id,
    sources = .strings(a$evidence)
  )
  for (rows in allocation) {
    .need(
      paired ||
        all(vapply(
          conditions,
          function(condition) sum(z[rows] == condition) >= 2L,
          logical(1)
        )),
      "Unpaired effect inference requires at least two assignment units per condition within every block.",
      code = "assignment_replication_insufficient",
      path = a$id,
      sources = .strings(a$evidence)
    )
    .need(
      abs(sum(z[rows] == conditions[[2]]) - length(rows) * a$prob) < 1e-8,
      "Realized allocation disagrees with the declared complete-randomization probability."
    )
  }
  formula <- stats::reformulate(
    paste0("`", a$treatment, "`"),
    response = paste0("`", outcome, "`")
  )
  args <- list(
    formula = formula,
    data = data,
    condition1 = conditions[[1]],
    condition2 = conditions[[2]]
  )
  if (length(units) < nrow(data)) {
    args$clusters <- as.name(a$unit)
  }
  if (!is.null(a$blocks)) {
    args$blocks <- as.name(a$blocks)
  }
  analysis <- list(
    dataset = ctx$dataset,
    assignment = assignment,
    outcome = outcome,
    target = "randomized roster",
    estimator = "difference_in_means",
    conditions = a$conditions,
    current_keys = .array(.keys(ref, ctx)),
    used_rows = nrow(ref),
    missing_rows = 0L,
    backend = "estimatr",
    backend_version = as.character(utils::packageVersion("estimatr"))
  )
  list(args = args, analysis = analysis)
}

#' @export
print.studyframe_result <- function(x, ...) {
  print(x$fit, ...)
  cat(
    "\nDataset:",
    x$analysis$dataset$id,
    "| backend:",
    x$analysis$backend,
    x$analysis$backend_version,
    "| observations used:",
    x$analysis$used_rows,
    "\n"
  )
  invisible(x)
}
