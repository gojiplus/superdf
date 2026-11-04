#' Declare how observations were collected
#'
#' Declarations use stable variable identifiers, initially equal to column names.
#' Unknown components remain unknown; constructors do not infer a design from data.
#' Sources describe the evidence for a declaration, not proof of its truth.
#'
#' @param id Stable identifier for a variable or declaration.
#' @param column Column containing a variable (defaults to its identifier).
#' @param label Human-readable definition.
#' @param units Measurement units, or NULL when not documented.
#' @param categories List of lists with `value` and `label` fields.
#' @param missing List of lists with `value` and `reason` fields. Codes are retained
#'   in storage; adapters convert declared codes to NA before analysis.
#' @param wave Measurement occasion, or NULL when not documented.
#' @param source Source or explanation for the declaration.
#' @param status One of documented, assumed, or unknown.
#' @param unit Variable identifying sampling or assignment units.
#' @param strata Variable identifying sampling strata, or NULL.
#' @param replacement With, without, or unknown replacement.
#' @param method Sampling method (srs, pps, unknown) or variance method.
#' @param probability Variable containing stage inclusion probabilities, or NULL.
#' @param fpc Variable containing finite-population information, or NULL.
#' @param fpc_kind Interpretation of fpc: population_size or sampling_fraction.
#' @param population Description of the population or frame.
#' @param stages Ordered list of sampling_stage declarations.
#' @param state known, unknown, or not_applicable.
#' @param variable Variable containing a weight.
#' @param sampling Identifier of the associated sampling declaration, or NULL.
#' @param role Weight role: base, final, or analysis.
#' @param waves Measurement occasions supported by the weight. Empty means
#'   applicability is undocumented, not that every occasion is supported.
#' @param applies List of rules, each with a `variable` and a list of `values`.
#'   Rules are conjoined and define the weight's reference sample.
#' @param adjustments Documented weight adjustments, as character strings.
#' @param variance A variance_spec declaration.
#' @param replicates Stable identifiers of replicate-weight columns.
#' @param combined Whether replicate columns already include full-sample weights.
#' @param scale Overall replicate variance scaling constant, or NULL.
#' @param rscales Per-replicate scaling constants.
#' @param mse Center replicate deviations at the full-sample estimate.
#' @param rho Fay's parameter, or NULL.
#' @param lonely_psu Explicit single-PSU handling policy (default fail).
#' @param treatment Variable containing realized assignment.
#' @param blocks Variable identifying assignment blocks, or NULL.
#' @param mechanism complete, bernoulli, or unknown assignment.
#' @param prob Probability of the second condition, constant across units.
#' @param conditions Two ordered assignment codes: control, treatment.
#' @return A plain list conforming to a component of the SuperDF schema.
#' @name declarations
#' @examples
#' stage <- sampling_stage("school", method = "srs", strata = "district", replacement = "with")
#' design <- sampling_spec("sample", "District pupils", list(stage), source = "Protocol")
#' weight <- weight_spec("main", "weight", sampling = "sample", source = "Release guide")
NULL

#' @rdname declarations
#' @export
variable_spec <- function(
  id,
  column = id,
  label = "",
  units = NULL,
  categories = list(),
  missing = list(),
  wave = NULL,
  source = "",
  status = "unknown"
) {
  list(
    id = id,
    column = column,
    label = label,
    units = units,
    categories = categories,
    missing = missing,
    wave = wave,
    source = source,
    status = status
  )
}

#' @rdname declarations
#' @export
sampling_stage <- function(
  unit,
  strata = NULL,
  replacement = "unknown",
  method = "unknown",
  probability = NULL,
  fpc = NULL,
  fpc_kind = "population_size"
) {
  list(
    unit = unit,
    strata = strata,
    replacement = replacement,
    method = method,
    probability = probability,
    fpc = if (is.null(fpc)) NULL else list(variable = fpc, kind = fpc_kind)
  )
}

#' @rdname declarations
#' @export
sampling_spec <- function(
  id,
  population = "",
  stages = list(),
  state = "known",
  source = "",
  status = "documented"
) {
  list(
    id = id,
    state = state,
    population = population,
    stages = stages,
    source = source,
    status = status
  )
}

#' @rdname declarations
#' @export
variance_spec <- function(
  method = "taylor",
  replicates = character(),
  combined = TRUE,
  scale = NULL,
  rscales = numeric(),
  mse = TRUE,
  rho = NULL,
  lonely_psu = "fail"
) {
  list(
    method = method,
    replicates = as.list(replicates),
    combined = combined,
    scale = scale,
    rscales = as.list(rscales),
    mse = mse,
    rho = rho,
    lonely_psu = lonely_psu
  )
}

#' @rdname declarations
#' @export
weight_spec <- function(
  id,
  variable,
  sampling = NULL,
  role = "base",
  waves = character(),
  applies = list(),
  adjustments = character(),
  variance = variance_spec(),
  source = "",
  status = "documented"
) {
  list(
    id = id,
    variable = variable,
    sampling = sampling,
    role = role,
    waves = as.list(waves),
    applies = applies,
    adjustments = as.list(adjustments),
    variance = variance,
    source = source,
    status = status
  )
}

#' @rdname declarations
#' @export
assignment_spec <- function(
  id,
  treatment,
  unit,
  blocks = NULL,
  mechanism = "unknown",
  prob = NULL,
  conditions = c(0, 1),
  state = "known",
  source = "",
  status = "documented"
) {
  list(
    id = id,
    state = state,
    treatment = treatment,
    unit = unit,
    blocks = blocks,
    mechanism = mechanism,
    prob = prob,
    conditions = as.list(conditions),
    source = source,
    status = status
  )
}
