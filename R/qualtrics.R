.q_fail <- function(
  code,
  message,
  path = "import",
  call = rlang::caller_env()
) {
  issue <- analysis_issue(
    code,
    "invalid",
    path,
    message,
    remedy = "Review the export and its instrument snapshot."
  )
  cli::cli_abort(
    "{message}",
    class = c("studyframe_import_error", "studyframe_error"),
    issues = list(issue),
    call = call
  )
}

.q_text <- function(value) {
  if (is.null(value)) {
    return("")
  }
  trimws(gsub("<[^>]+>", "", as.character(value)))
}

.q_column <- function(id) {
  aliases <- c(
    `_recordId` = "response_id",
    ResponseId = "response_id",
    responseId = "response_id",
    Status = "status",
    Finished = "finished",
    DistributionChannel = "distribution_channel",
    distributionChannel = "distribution_channel"
  )
  if (id %in% names(aliases)) {
    return(unname(aliases[[id]]))
  }
  id <- sub("^embeddedData-", "", id)
  id <- gsub("[^A-Za-z0-9_]", "_", id)
  if (!grepl("^[A-Za-z]", id)) {
    id <- paste0("v_", id)
  }
  id
}

.q_csv <- function(file) {
  raw <- utils::read.csv(
    file,
    check.names = FALSE,
    colClasses = "character",
    na.strings = "",
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
  import_ids <- names(raw)
  labels <- names(raw)
  if (nrow(raw) >= 2L) {
    decoded <- lapply(raw[2, ], function(x) {
      if (is.na(x) || !grepl('"ImportId"', x, fixed = TRUE)) {
        return(NULL)
      }
      tryCatch(
        jsonlite::fromJSON(x, simplifyVector = FALSE)$ImportId,
        error = function(e) NULL
      )
    })
    known <- vapply(
      decoded,
      function(x) rlang::is_string(x) && nzchar(x),
      logical(1)
    )
    if (any(known)) {
      if (!all(known)) {
        .q_fail(
          "qualtrics_partial_import_ids",
          "Every export column needs a valid ImportId header."
        )
      }
      import_ids <- vapply(decoded, identity, character(1))
      labels <- vapply(
        raw[1, ],
        function(x) if (is.na(x)) "" else x,
        character(1)
      )
      raw <- raw[-c(1, 2), , drop = FALSE]
    }
  }
  if (anyDuplicated(import_ids)) {
    .q_fail("qualtrics_duplicate_import_ids", "ImportId values must be unique.")
  }
  columns <- vapply(import_ids, .q_column, character(1))
  if (anyDuplicated(columns)) {
    .q_fail(
      "qualtrics_column_collision",
      "Export identifiers collide after normalization."
    )
  }
  names(raw) <- columns
  list(data = raw, ids = import_ids, labels = labels)
}

.q_numeric <- function(x, column, integer = FALSE) {
  value <- suppressWarnings(as.numeric(x))
  bad <- !is.na(x) & (is.na(value) | !is.finite(value))
  if (integer) {
    outside_integer <- value != trunc(value) | abs(value) > .Machine$integer.max
    bad <- bad | (!is.na(value) & outside_integer)
  }
  if (any(bad)) {
    .q_fail(
      "qualtrics_invalid_numeric",
      paste("Invalid numeric export value in", column),
      column
    )
  }
  if (integer) as.integer(value) else value
}

.q_convert <- function(x, type, column) {
  switch(
    type,
    string = x,
    number = .q_numeric(x, column),
    integer = .q_numeric(x, column, integer = TRUE),
    boolean = {
      normalized <- tolower(x)
      if (any(!is.na(x) & !normalized %in% c("true", "false", "1", "0"))) {
        .q_fail(
          "qualtrics_invalid_boolean",
          paste("Invalid boolean export value in", column),
          column
        )
      }
      ifelse(is.na(x), NA, normalized %in% c("true", "1"))
    },
    .q_fail(
      "qualtrics_invalid_type",
      paste("Unsupported column type for", column),
      column
    )
  )
}

.q_question <- function(id, question, values, source, label) {
  qid <- question$QuestionID
  suffix <- substring(id, nchar(qid) + 1L)
  subquestion <- if (nzchar(suffix)) substring(suffix, 2L) else NULL
  categories <- list()
  type <- "string"
  text <- .q_text(question$QuestionText)
  if (!nzchar(text)) {
    text <- label
  }
  if (
    question$QuestionType == "TE" &&
      question$Selector %in% c("SL", "ML", "ESTB")
  ) {
    if (!suffix %in% c("", "_TEXT")) {
      .q_fail(
        "qualtrics_ambiguous_column",
        paste("Unsupported text field:", id),
        id
      )
    }
  } else if (
    question$QuestionType == "TE" && identical(question$Selector, "FORM")
  ) {
    if (is.null(subquestion) || !subquestion %in% names(question$Choices)) {
      .q_fail(
        "qualtrics_ambiguous_column",
        paste("Unknown form subquestion:", id),
        id
      )
    }
    text <- paste(
      text,
      .q_text(question$Choices[[subquestion]]$Display),
      sep = " \u2014 "
    )
  } else if (
    question$QuestionType == "MC" &&
      question$Selector %in% c("SAVR", "SAHR", "SACOL", "DL", "SB")
  ) {
    if (nzchar(suffix)) {
      choice <- sub("_TEXT$", "", subquestion)
      if (!endsWith(suffix, "_TEXT") || !choice %in% names(question$Choices)) {
        .q_fail(
          "qualtrics_ambiguous_column",
          paste("Unsupported multiple-choice field:", id),
          id
        )
      }
      text <- paste(
        text,
        .q_text(question$Choices[[choice]]$Display),
        "text",
        sep = " \u2014 "
      )
    } else {
      codes <- question$RecodeValues
      if (is.null(codes) || !setequal(names(codes), names(question$Choices))) {
        .q_fail(
          "qualtrics_recode_mapping_required",
          paste("Explicit complete QSF RecodeValues are required for", id),
          id
        )
      }
      numeric_codes <- .q_numeric(unlist(codes, use.names = FALSE), id)
      if (anyDuplicated(numeric_codes)) {
        .q_fail(
          "qualtrics_duplicate_recodes",
          paste("Recodes are ambiguous for", id),
          id
        )
      }
      categories <- lapply(seq_along(codes), function(i) {
        list(
          value = numeric_codes[[i]],
          label = .q_text(question$Choices[[names(codes)[[i]]]]$Display)
        )
      })
      type <- "number"
      converted <- .q_numeric(values, id)
      if (any(!is.na(converted) & !converted %in% numeric_codes)) {
        .q_fail(
          "qualtrics_recode_mismatch",
          paste("Response codes disagree with the QSF for", id),
          id
        )
      }
    }
  } else {
    .q_fail(
      "qualtrics_unsupported_question",
      paste(
        "Unsupported question type/selector for",
        id,
        ":",
        question$QuestionType,
        question$Selector
      ),
      id
    )
  }
  list(
    type = type,
    variable = variable_spec(
      .q_column(id),
      label = text,
      categories = categories,
      source = "Qualtrics instrument export",
      status = "documented",
      evidence = source,
      instrument = instrument_ref(source, qid, subquestion)
    )
  )
}

#' Import a bounded Qualtrics response export with its instrument evidence
#'
#' Reads a numeric-value CSV export and a QSF instrument snapshot. The supported
#' questions are single-answer multiple choice with explicit recodes, single or
#' multiline text, text forms, and text fields attached to supported choices.
#' Other question types and ambiguous mappings fail explicitly. Blanks remain
#' missing with unknown reason. No sampling or randomization mechanism is inferred.
#'
#' @param responses Path to a CSV response export. Standard three-row Qualtrics
#'   headers and single-row headers containing import IDs are supported.
#' @param qsf Path to the corresponding QSF instrument snapshot, or NULL for a
#'   response-only import. Without it, question codes remain uninterpreted strings
#'   unless converted explicitly; instrument identity and coding remain unknown.
#' @param dataset_id Dataset identifier; defaults to the QSF survey ID.
#' @param unit Observational unit for an import without an authoritative roster.
#' @param roster Optional authoritative studyframe. Outcomes are left-joined to its
#'   complete attached roster, never substituted for its sampling or assignment facts.
#' @param respondent_key Import ID (or normalized column name) containing the
#'   authoritative roster's single key. Required when roster is supplied.
#' @param column_types Named character vector of explicit storage conversions,
#'   using import IDs or normalized names: string, number, integer, boolean.
#' @param preview Policy for preview, test, offline-preview, and synthetic-panel
#'   records: error (default), exclude, or include. Incomplete genuine responses
#'   are retained under every policy.
#' @param expected_qsf_sha256 Optional QSF SHA-256 captured during collection.
#'   A mismatch fails. Without it, the instrument/export match remains unknown.
#' @param instrument_version Optional caller-supplied instrument version. It is
#'   attributed information, not proof that respondents saw this version.
#' @param expected_survey_id Optional expected survey ID, checked against the QSF.
#' @return A studyframe with source evidence, question mappings, and import diagnostics.
#' @export
read_qualtrics <- function(
  responses,
  qsf = NULL,
  dataset_id = NULL,
  unit = "response",
  roster = NULL,
  respondent_key = NULL,
  column_types = character(),
  preview = c("error", "exclude", "include"),
  expected_qsf_sha256 = NULL,
  instrument_version = NULL,
  expected_survey_id = NULL
) {
  preview <- rlang::arg_match(preview)
  instrument <- if (is.null(qsf)) {
    NULL
  } else {
    jsonlite::read_json(qsf, simplifyVector = FALSE)
  }
  survey_id <- if (is.null(qsf)) {
    "qualtrics_responses"
  } else {
    instrument$SurveyEntry$SurveyID
  }
  if (
    !is.null(qsf) &&
      (is.null(survey_id) || length(survey_id) != 1L || !nzchar(survey_id))
  ) {
    .q_fail("qualtrics_missing_survey_id", "QSF must identify its survey.")
  }
  if (
    !is.null(expected_survey_id) && !identical(survey_id, expected_survey_id)
  ) {
    .q_fail(
      "qualtrics_survey_mismatch",
      "QSF survey ID does not match the expected survey."
    )
  }
  qsf_hash <- if (is.null(qsf)) NULL else .file_hash(qsf)
  if (
    !is.null(expected_qsf_sha256) && !identical(qsf_hash, expected_qsf_sha256)
  ) {
    .q_fail(
      "qualtrics_instrument_mismatch",
      "QSF hash differs from the instrument snapshot recorded for collection."
    )
  }
  response_hash <- .file_hash(responses)
  qsf_id <- if (is.null(qsf)) NULL else paste0("qsf_", substr(qsf_hash, 1, 12))
  response_id <- paste0("responses_", substr(response_hash, 1, 12))
  captured <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  evidence <- list(
    evidence_spec(
      response_id,
      normalizePath(responses),
      response_hash,
      producer = "Qualtrics",
      captured_at = captured,
      description = "Response export with stable import identifiers; coding requires instrument evidence"
    )
  )
  if (!is.null(qsf)) {
    evidence <- c(
      list(
        evidence_spec(
          qsf_id,
          normalizePath(qsf),
          qsf_hash,
          producer = "Qualtrics",
          captured_at = captured,
          version = instrument_version,
          description = paste("Instrument snapshot for", survey_id)
        )
      ),
      evidence
    )
  }
  diagnostics <- list(analysis_issue(
    "qualtrics_blank_reason_unknown",
    "missing",
    "variables",
    "Blank responses retain an unknown missingness reason.",
    response_id,
    "Supply recorded exposure/disposition evidence before classifying blanks."
  ))
  if (is.null(qsf)) {
    diagnostics[[length(diagnostics) + 1L]] <- analysis_issue(
      "qualtrics_instrument_missing",
      "missing",
      "variables",
      "No instrument snapshot was supplied; question coding is unknown.",
      response_id,
      "Attach a collection-time QSF and verify its coding before interpreting responses."
    )
  }
  if (!is.null(qsf) && is.null(expected_qsf_sha256)) {
    diagnostics[[length(diagnostics) + 1L]] <- analysis_issue(
      "qualtrics_instrument_match_unknown",
      "missing",
      "evidence",
      "The QSF has not been matched to a collection-time snapshot.",
      qsf_id,
      "Compare the collection-time instrument hash and export coding before inference."
    )
  }
  if (grepl('"Randomizer"|"BlockRandomizer"', .json(instrument))) {
    diagnostics[[length(diagnostics) + 1L]] <- analysis_issue(
      "qualtrics_randomization_unmapped",
      "unsupported",
      "assignments",
      "Survey randomizer settings do not establish a supported assignment mechanism.",
      qsf_id,
      "Use a separately documented assignment roster or a validated mechanism-specific adapter."
    )
  }
  imported <- .q_csv(responses)
  data <- imported$data
  if (
    !"response_id" %in% names(data) ||
      anyNA(data$response_id) ||
      anyDuplicated(data$response_id)
  ) {
    .q_fail(
      "qualtrics_response_ids",
      "Response IDs must exist, be nonmissing, and be unique."
    )
  }
  nonfield <- rep(FALSE, nrow(data))
  if (!"status" %in% names(data)) {
    diagnostics[[length(diagnostics) + 1L]] <- analysis_issue(
      "qualtrics_status_unknown",
      "missing",
      "variables",
      "Response type was not exported; test and preview records cannot be identified.",
      response_id,
      "Export Status/Response Type and review collection records."
    )
  } else {
    nonfield <- tolower(data$status) %in%
      c(
        "1",
        "2",
        "17",
        "256",
        "survey preview",
        "survey test",
        "offline preview",
        "synthetic"
      )
    known <- nonfield |
      tolower(data$status) %in%
        c("0", "4", "16", "ip address", "imported", "offline")
    if (any(!known)) {
      diagnostics[[length(diagnostics) + 1L]] <- analysis_issue(
        "qualtrics_status_unrecognized",
        "missing",
        "variables/status",
        paste(
          sum(!known),
          "records have missing or unrecognized response types; these records are retained."
        ),
        response_id,
        "Review collection records before deciding whether these responses belong in analysis."
      )
    }
  }
  if ("distribution_channel" %in% names(data)) {
    nonfield <- nonfield |
      tolower(data$distribution_channel) %in% c("preview", "test")
  }
  if (any(nonfield) && preview == "error") {
    .q_fail(
      "qualtrics_nonfield_responses",
      "Preview, test, or synthetic responses require an explicit policy."
    )
  }
  if (any(nonfield)) {
    diagnostics[[length(diagnostics) + 1L]] <- analysis_issue(
      paste0("qualtrics_nonfield_", preview),
      "choice",
      "variables/status",
      paste(
        sum(nonfield),
        "preview/test/synthetic records; policy:",
        preview
      ),
      response_id,
      "Retain the policy with the delivered study."
    )
    if (preview == "exclude") data <- data[!nonfield, , drop = FALSE]
  }
  questions <- Filter(
    function(e) identical(e$Element, "SQ"),
    instrument$SurveyElements
  )
  payloads <- lapply(questions, function(e) e$Payload)
  question_ids <- vapply(payloads, function(q) q$QuestionID, character(1))
  if (anyDuplicated(question_ids)) {
    .q_fail("qualtrics_duplicate_questions", "QSF question IDs must be unique.")
  }
  names(payloads) <- question_ids
  if (
    length(column_types) &&
      (is.null(names(column_types)) || any(!nzchar(names(column_types))))
  ) {
    .q_fail("qualtrics_unnamed_types", "Column type conversions must be named.")
  }
  conversions <- stats::setNames(
    column_types,
    vapply(names(column_types), .q_column, character(1))
  )
  if (
    anyDuplicated(names(conversions)) ||
      !all(names(conversions) %in% names(data))
  ) {
    .q_fail(
      "qualtrics_unknown_conversion",
      "Type conversions must name distinct exported columns."
    )
  }
  variables <- vector("list", ncol(data))
  for (i in seq_along(data)) {
    id <- imported$ids[[i]]
    column <- names(data)[[i]]
    qid <- sub("_.*$", "", id)
    type <- if (column == "finished") "boolean" else "string"
    variable <- variable_spec(
      column,
      label = imported$labels[[i]],
      source = paste("Qualtrics response export; ImportId:", id),
      status = "documented",
      evidence = response_id
    )
    if (!is.null(qsf) && grepl("^QID", id)) {
      if (!qid %in% question_ids) {
        .q_fail(
          "qualtrics_question_mismatch",
          paste("Export question is missing from QSF:", id),
          id
        )
      }
      definition <- .q_question(
        id,
        payloads[[qid]],
        data[[i]],
        qsf_id,
        imported$labels[[i]]
      )
      type <- definition$type
      variable <- definition$variable
      variable$evidence <- as.list(c(qsf_id, response_id))
    }
    if (column %in% names(conversions)) {
      requested <- conversions[[column]]
      if (length(variable$categories) && requested != "number") {
        .q_fail(
          "qualtrics_coded_type",
          "Coded multiple-choice columns retain numeric storage.",
          column
        )
      }
      type <- requested
    }
    data[[i]] <- .q_convert(data[[i]], type, column)
    variables[[i]] <- variable
  }
  if (is.null(roster)) {
    return(studyframe(
      data,
      keys = "response_id",
      unit = unit,
      dataset_id = if (is.null(dataset_id)) survey_id else dataset_id,
      variables = variables,
      evidence = evidence,
      diagnostics = diagnostics,
      roster = roster_spec(
        "received_responses",
        "Qualtrics response export",
        evidence = response_id
      )
    ))
  }
  validate_studyframe(roster)
  keys <- .strings(context(roster)$units$keys)
  if (length(keys) != 1L || is.null(respondent_key)) {
    .q_fail(
      "qualtrics_roster_key",
      "Supply respondent_key for a roster with one stable observation key."
    )
  }
  received_key <- .q_column(respondent_key)
  if (
    !received_key %in% names(data) ||
      anyNA(data[[received_key]]) ||
      anyDuplicated(data[[received_key]])
  ) {
    .q_fail(
      "qualtrics_repeated_unit",
      "Response unit keys must be nonmissing and unique; resolve repeat submissions explicitly."
    )
  }
  roster_key <- .col(context(roster), keys[[1]])
  reference <- collection_data(roster)
  if (
    !identical(
      .storage(data[[received_key]]),
      .storage(reference[[roster_key]])
    )
  ) {
    .q_fail(
      "qualtrics_key_type",
      "Response and roster key types differ; declare column_types explicitly."
    )
  }
  if (!all(data[[received_key]] %in% reference[[roster_key]])) {
    .q_fail(
      "qualtrics_unknown_unit",
      "Responses include units absent from the authoritative roster."
    )
  }
  names(data)[match(received_key, names(data))] <- roster_key
  variables <- Filter(function(v) v$column != received_key, variables)
  overlap <- intersect(setdiff(names(data), roster_key), names(reference))
  if (length(overlap)) {
    .q_fail(
      "qualtrics_roster_collision",
      paste(
        "Response fields cannot overwrite authoritative roster columns:",
        paste(overlap, collapse = ", ")
      )
    )
  }
  roster <- attach_evidence(roster, evidence, diagnostics)
  join_studyframe(roster, data, roster_key, variables, source = response_id)
}
