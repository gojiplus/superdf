reproduce_recall <- function(repo, seed = 1L, include_legacy = FALSE) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(readr)
    library(readxl)
    library(scales)
  })
  manifest <- c(
    "scripts/00_survey_df_clean.R" = "ccbed22841411aba1fb47f59f630cd9d930ff77aa08245a953159932b0424fc5",
    "scripts/02_results_w2.R" = "e8dcd484cc4ad4392c318b0560e526b58d7b7c0a40da4f322e83f84c2f46c751",
    "data/lucid/er2_lucid_jan2023.csv" = "9b891b08fbff7c0f3cdf922e652122b6710b507b2a412b4aa93ec57fa56f28a5",
    "data/lucid/extreme_recall.docx" = "1c7843658ae80af179075d31ea95be5aba756de4a5f7dc6ec158f74bbb16d7cb",
    "data/politicians/names_open_to_id_coded.csv" = "f8e2bcfbe206ec2e9a401dcad632b2004b4e30efe606ab10c7dac39236574a9a",
    "data/politicians/names_open_to_id.csv" = "6eda188c0a0822f2ba067df6a7dd897d661c0cd74a04e414c19c380d4ed23b7d",
    "data/politicians/politicians_attributes.xlsx" = "db223ccdad1525bec679701a66a593590429fe2532c0ea5a058759da8e20a831",
    "data/politicians/partisanship.csv" = "27391173af790957a710641fbcee2a93b57602303aa6a15caddee051cbb93b9d",
    "data/cf_scores/dime_recipients_1979_2020.csv.gz" =
      "5fcd5634c88d8c3c3c8a7d6cbab680543871aba4fac024f14c4887800b97e717"
  )
  actual <- vapply(
    file.path(repo, names(manifest)),
    function(p) digest::digest(file = p, algo = "sha256"),
    character(1)
  )
  if (!identical(unname(actual), unname(manifest))) {
    stop("Source hashes changed; review the source expressions and manifest.")
  }
  oldwd <- setwd(repo)
  on.exit(setwd(oldwd), add = TRUE)
  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (has_seed) {
    oldseed <- get(".Random.seed", envir = .GlobalEnv)
  }
  on.exit(
    if (has_seed) {
      assign(".Random.seed", oldseed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    },
    add = TRUE
  )
  oldkind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(oldkind)), add = TRUE, after = FALSE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  env <- new.env(parent = globalenv())
  cleaning <- parse("scripts/00_survey_df_clean.R")
  wanted <- c(
    "list_bonica_open",
    "df_open_ids",
    "df_open_ids_complicated",
    "df_politicians",
    "df_missing_party",
    "df_dime_open_merge",
    "df_er_clean",
    "df_er_participants",
    "df_er_open_w2"
  )
  captured_warnings <- character()
  withCallingHandlers(
    suppressMessages({
      for (name in wanted) {
        idx <- which(vapply(
          cleaning,
          function(e) {
            is.call(e) &&
              identical(e[[1]], as.name("<-")) &&
              identical(e[[2]], as.name(name))
          },
          logical(1)
        ))
        stopifnot(length(idx) == 1L)
        eval(cleaning[[idx]], envir = env)
      }
    }),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  results <- parse("scripts/02_results_w2.R")
  stopifnot(identical(results[[10]][[2]], as.name("df_er_open_w2")))
  eval(results[[10]], envir = env)
  slots <- as.data.frame(dplyr::ungroup(env$df_er_open_w2))
  stopifnot(
    nrow(slots) == 6L * nrow(env$df_er_participants),
    !anyDuplicated(slots[c("ResponseId", "question_open")])
  )
  needed <- c(
    "ResponseId",
    "question_open",
    "party_open_q",
    "number",
    "number_inverse",
    "recipient.cfscore.dyn_scaled_prop_comb",
    "politicians_open_count",
    "pol_interest"
  )
  data <- slots[needed]
  baseline <- fixest::feols(
    recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse,
    data = data,
    se = "cluster",
    cluster = "ResponseId",
    notes = FALSE
  )
  complete <- complete.cases(data[c(
    "ResponseId",
    "number_inverse",
    "recipient.cfscore.dyn_scaled_prop_comb"
  )])
  stata <- estimatr::lm_robust(
    recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse,
    data = data[complete, ],
    clusters = data$ResponseId[complete],
    se_type = "stata"
  )
  sensitivity_data <- data[
    complete &
      !is.na(slots$party_name) &
      slots$party_name == slots$party_open_q,
  ]
  same_party_sensitivity <- fixest::feols(
    recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse,
    data = sensitivity_data,
    se = "cluster",
    cluster = "ResponseId",
    notes = FALSE
  )
  legacy <- if (include_legacy) {
    estimatr::lm_robust(
      politicians_open_count ~ pol_interest,
      data = data,
      clusters = data$ResponseId
    )
  } else {
    NULL
  }
  raw <- readr::read_csv(
    "data/lucid/er2_lucid_jan2023.csv",
    show_col_types = FALSE
  )
  flow <- list(
    raw_responses = nrow(raw),
    attention_pass = nrow(env$df_er_participants),
    attention_excluded = nrow(raw) - nrow(env$df_er_participants),
    slots = nrow(data),
    model_observations = sum(complete),
    model_clusters = length(unique(data$ResponseId[complete])),
    missing_outcome = sum(!complete),
    selected_assignment_counts = as.list(table(
      env$df_er_participants$experiment
    )),
    seed = seed,
    source_hashes = as.list(manifest)
  )
  flow$collection_start <- format(
    min(raw$StartDate),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  flow$collection_end <- format(
    max(raw$RecordedDate),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  flow$consent <- as.list(table(raw$Q1.1))
  flow$rng_kind <- as.list(RNGkind())
  joins <- list(
    raw_response_id = list(
      rows = nrow(raw),
      duplicate_keys = anyDuplicated(raw$ResponseId)
    ),
    typed_name_map = list(
      rows = nrow(env$df_open_ids_complicated),
      duplicate_keys = anyDuplicated(
        env$df_open_ids_complicated$politicians_open_lower
      )
    ),
    name_to_bonica = list(
      rows = nrow(env$df_open_ids),
      duplicate_keys = anyDuplicated(env$df_open_ids$politican_clean)
    ),
    politician_attributes = list(
      rows = nrow(env$df_politicians),
      duplicate_keys = anyDuplicated(env$df_politicians$bonica.rid)
    ),
    dime_latest = list(
      rows = nrow(env$df_dime_open_merge),
      duplicate_keys = anyDuplicated(env$df_dime_open_merge$bonica.rid)
    ),
    party_map = list(
      rows = nrow(env$df_missing_party),
      duplicate_keys = anyDuplicated(env$df_missing_party$politican_clean)
    ),
    participant_roster = list(
      rows = nrow(env$df_er_participants),
      duplicate_keys = anyDuplicated(env$df_er_participants$ResponseId)
    )
  )
  stopifnot(all(vapply(joins, function(j) j$duplicate_keys == 0L, logical(1))))
  diagnostics <- list(
    wrong_party_model_rows = sum(
      complete &
        !is.na(slots$party_name) &
        slots$party_name != slots$party_open_q
    ),
    invalid_count_model_rows = sum(
      complete & is.na(slots$politican_clean_count)
    ),
    missing_score_by_slot = as.list(table(slots$number[!complete])),
    model_rows_by_party = as.list(table(slots$party_open_q[complete])),
    participant_party_count_distribution = as.list(table(
      unique(slots[c(
        "ResponseId",
        "party_open_q",
        "politicians_open_count"
      )])$politicians_open_count
    ))
  )
  list(
    data = data,
    baseline = baseline,
    same_party_sensitivity = same_party_sensitivity,
    estimatr_stata = stata,
    legacy = legacy,
    flow = flow,
    joins = joins,
    diagnostics = diagnostics,
    warnings = unique(captured_warnings)
  )
}
