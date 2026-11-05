test_that("response imports cannot acquire complete randomized-roster claims implicitly", {
  original <- reference_workflows()$experiment
  d <- as.data.frame(original)
  a <- context(original)$assignments
  received <- studyframe(
    d,
    "id",
    "pupil",
    "responses",
    assignments = a,
    roster = roster_spec("received_responses")
  )
  request <- analysis_request(
    "effect",
    "outcome",
    assignment = "offer_assignment"
  )
  checked <- check_analysis(received, request)
  expect_false(checked$ok)
  expect_equal(checked$issues[[1]]$code, "randomized_roster_required")
  condition <- tryCatch(
    run_analysis(received, request),
    studyframe_analysis_error = identity
  )
  expect_s3_class(condition, "studyframe_analysis_error")
  expect_equal(condition$issues, checked$issues)
  expect_true(check_analysis(original, request)$ok)
  expect_equal(
    run_analysis(original, request)$analysis$roster$completeness,
    "asserted_complete"
  )
  expect_error(
    studyframe(
      d,
      "id",
      "pupil",
      "unsupported_claim",
      assignments = a,
      roster = roster_spec(
        "randomized_units",
        completeness = "asserted_complete",
        scope = "offer_assignment"
      )
    ),
    "source, and evidence"
  )
})

test_that("evidence identities and variable instrument references are preserved", {
  d <- data.frame(id = 1:3, y = c(1, 2, 3))
  first <- evidence_spec(
    "instrument_a",
    "file:survey.qsf",
    sha256 = strrep("a", 64),
    producer = "Survey team",
    version = "v1"
  )
  second <- evidence_spec(
    "instrument_b",
    "file:survey.qsf",
    sha256 = strrep("b", 64),
    producer = "Survey team",
    version = "v1",
    description = "Changed coding without publishing"
  )
  x <- studyframe(
    d,
    "id",
    "person",
    "evidence",
    evidence = list(first),
    variables = list(variable_spec(
      "y",
      evidence = "instrument_a",
      instrument = instrument_ref("instrument_a", "QID1")
    ))
  )
  extended <- attach_evidence(x, list(second))
  expect_length(context(extended)$evidence, 2L)
  expect_identical(
    context(extended)$variables[[1]]$instrument$source,
    "instrument_a"
  )
  conflict <- first
  conflict$sha256 <- strrep("c", 64)
  expect_error(attach_evidence(x, list(conflict)), "Conflicting evidence")
  expect_error(
    studyframe(
      d,
      "id",
      "person",
      "duplicates",
      evidence = list(first, conflict)
    ),
    "Duplicate evidence"
  )
  expect_error(
    studyframe(
      d,
      "id",
      "person",
      "missing_source",
      variables = list(variable_spec(
        "y",
        instrument = instrument_ref("absent", "QID1")
      ))
    ),
    "unknown evidence"
  )
  issue <- analysis_issue(
    "blank_reason_unknown",
    "missing",
    "y",
    "Blank reason is unknown.",
    sources = "instrument_a",
    remedy = "Retain unknown until a source distinguishes missing reasons."
  )
  with_issue <- attach_evidence(x, list(first), list(issue))
  expect_equal(context(with_issue)$diagnostics, list(issue))
  expect_error(
    attach_evidence(
      x,
      list(),
      list(analysis_issue("bad", "missing", "y", "Bad", "absent"))
    ),
    "unknown evidence"
  )
})

test_that("requirements checks collect independent facts and never call estimators", {
  x <- reference_workflows()$experiment
  missing <- check_analysis(
    x,
    analysis_request("effect", "absent", assignment = "absent")
  )
  codes <- vapply(missing$issues, function(z) z$code, character(1))
  expect_setequal(
    codes,
    c("outcome_unknown", "assignment_required", "randomized_roster_required")
  )
  # A failing estimator binding makes accidental fitting during checks observable.
  local_mocked_bindings(
    svymean = function(...) stop("mean estimator was called"),
    .package = "survey"
  )
  local_mocked_bindings(
    difference_in_means = function(...) stop("effect estimator was called"),
    .package = "estimatr"
  )
  mean <- reference_workflows()$survey
  expect_true(
    check_analysis(mean, analysis_request("mean", "income", weight = "main"))$ok
  )
  expect_true(
    check_analysis(
      x,
      analysis_request("effect", "outcome", assignment = "offer_assignment")
    )$ok
  )
})

test_that("checks share preparation and identify changed revisions", {
  x <- reference_workflows()$survey
  request <- analysis_request("mean", "income", weight = "main")
  checked <- check_analysis(x, request)
  fit <- run_analysis(x, request)
  expect_true(checked$ok)
  expect_equal(fit$analysis$input_digest, checked$input_digest)
  expect_equal(fit$analysis$request, checked$request)
  expect_equal(fit$result_version, "0.3.0")
  expect_equal(fit$analysis$receipt_version, "0.3.0")
  expect_equal(fit$analysis$confidence_interval$df, fit$analysis$design_df)
  changed <- x[1:6, ]
  rejected <- check_analysis(changed, request)
  expect_false(rejected$ok)
  expect_equal(rejected$issues[[1]]$code, "domain_required")
  expect_false(identical(rejected$input_digest, checked$input_digest))
  condition <- tryCatch(
    run_analysis(changed, request),
    studyframe_analysis_error = identity
  )
  expect_equal(condition$issues, rejected$issues)
  planned <- context(x)$weights
  planned[[1]]$phase <- "planned"
  plan <- studyframe(
    as.data.frame(x),
    "id",
    "pupil",
    "planned",
    sampling = context(x)$sampling,
    weights = planned
  )
  expect_equal(
    check_analysis(plan, request)$issues[[1]]$code,
    "design_not_realized"
  )
  expect_equal(
    check_analysis(
      x,
      analysis_request("effect", "income", weight = "main")
    )$issues[[1]]$code,
    "effect_policy_unsupported"
  )
})

test_that("insufficient independent assignment replication is rejected before fitting", {
  make_study <- function(blocks = NULL, values = c(1L, 1L)) {
    original <- reference_workflows()$experiment
    ctx <- context(original)
    assignment <- ctx$assignments[[1]]
    assignment$unit <- "id"
    assignment["blocks"] <- list(blocks)
    studyframe(
      data.frame(
        id = seq_along(values),
        offer = rep(0:1, length.out = length(values)),
        outcome = seq_along(values) + 1,
        block = values
      ),
      "id",
      "person",
      "too_small",
      assignments = list(assignment),
      roster = ctx$roster,
      evidence = ctx$evidence
    )
  }
  request <- analysis_request(
    "effect",
    "outcome",
    assignment = "offer_assignment"
  )
  for (study in list(make_study(), make_study("block"))) {
    checked <- check_analysis(study, request)
    expect_false(checked$ok)
    expect_equal(
      checked$issues[[1]]$code,
      "assignment_replication_insufficient"
    )
    rejected <- tryCatch(
      run_analysis(study, request),
      studyframe_analysis_error = identity
    )
    expect_equal(rejected$issues, checked$issues)
  }
  expect_true(
    check_analysis(make_study("block", c(1L, 1L, 2L, 2L)), request)$ok
  )
  mixed <- make_study("block", c(1L, 1L, 2L, 2L, 2L, 2L))
  expect_equal(
    check_analysis(mixed, request)$issues[[1]]$code,
    "mixed_block_sizes_unsupported"
  )
})
test_that("request validation returns portable issues for malformed arguments", {
  x <- reference_workflows()$survey
  request <- analysis_request("mean", "income", weight = "main")
  invalid <- list(
    NULL,
    "mean",
    unname(request),
    as.data.frame(request[!vapply(request, is.null, logical(1))])
  )
  for (value in list(
    NULL,
    NA_character_,
    character(),
    c("a", "b"),
    1,
    "bad id"
  )) {
    candidate <- request
    candidate["outcome"] <- list(value)
    invalid[[length(invalid) + 1L]] <- candidate
  }
  for (value in list(NULL, NA, logical(), c(TRUE, FALSE), 1, "TRUE")) {
    candidate <- request
    candidate["domain"] <- list(value)
    invalid[[length(invalid) + 1L]] <- candidate
  }
  for (candidate in invalid) {
    checked <- check_analysis(x, candidate)
    expect_identical(checked$ok, FALSE)
    expect_identical(checked$issues[[1]]$code, "invalid_request")
    failed <- tryCatch(run_analysis(x, candidate), studyframe_error = identity)
    expect_identical(failed$issues, checked$issues)
    expect_identical(conditionCall(failed)[[1]], quote(run_analysis))
  }
  roundtrip <- jsonlite::fromJSON(.json(request), simplifyVector = FALSE)
  expect_identical(check_analysis(x, roundtrip)$ok, TRUE)
})

test_that("backend failures retain their original condition without leaking it into issue records", {
  x <- reference_workflows()$survey
  request <- analysis_request("mean", "income", weight = "main")
  cause <- rlang::error_cnd(
    "backend_test_error",
    message = "Backend {details}.",
    field = "income"
  )
  local_mocked_bindings(
    svymean = function(...) stop(cause),
    .package = "survey"
  )
  failed <- tryCatch(run_analysis(x, request), studyframe_error = identity)
  expect_s3_class(failed, "studyframe_analysis_error")
  expect_identical(failed$parent, cause)
  expect_identical(conditionCall(failed)[[1]], quote(run_analysis))
  expect_identical(failed$issues[[1]]$code, "backend_estimation_failed")
  expect_identical(failed$issues[[1]]$message, "Backend {details}.")
  expect_identical(
    jsonlite::fromJSON(.json(failed$issues), simplifyVector = FALSE),
    failed$issues
  )
})

test_that("context and design failures keep causes while checks remain plain data", {
  x <- reference_workflows()$survey
  request <- analysis_request("mean", "income", weight = "main")
  for (candidate in list(x[1:6, ], structure(x, seal = "changed"))) {
    checked <- check_analysis(candidate, request)
    failed <- tryCatch(
      run_analysis(candidate, request),
      studyframe_error = identity
    )
    expect_s3_class(failed$parent, "error")
    expect_identical(failed$issues, checked$issues)
    expect_identical(conditionCall(failed)[[1]], quote(run_analysis))
    expect_named(checked, c("ok", "issues", "request", "input_digest"))
    expect_identical(
      jsonlite::fromJSON(.json(checked$issues), simplifyVector = FALSE),
      checked$issues
    )
  }
})

test_that("analysis failures show collected requirements", {
  x <- reference_workflows()$survey
  expect_snapshot(
    error = TRUE,
    run_analysis(x, analysis_request("mean", "absent"))
  )
})

test_that("scalar request fields cannot change shape during JSON serialization", {
  x <- reference_workflows()$survey
  request <- analysis_request("mean", "income", weight = "main")
  for (field in c("kind", "outcome", "weight", "domain", "missing")) {
    for (value in list(matrix(request[[field]], 1L), I(request[[field]]))) {
      candidate <- request
      candidate[[field]] <- value
      checked <- check_analysis(x, candidate)
      expect_identical(checked$ok, FALSE)
      expect_identical(checked$issues[[1]]$code, "invalid_request")
    }
  }
})
