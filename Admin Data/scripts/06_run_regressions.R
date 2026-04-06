# Script: 06_run_regressions.R
# Purpose: Load all per-LA Parquet files into a single in-memory dataset and
#          run the full-sample OLS + PSM regressions with cluster-robust SEs.
#          This preserves the original Additive vs Interactive FE distinction.
#          Uses arrow::open_dataset() with column selection to minimise peak
#          memory (~50% reduction vs the old rbindlist approach).
#          Results are written to CSV incrementally after each treatment to
#          support crash-resume and avoid memory accumulation.
#
# Performance:
#   - Multi-outcome feols: all 13 outcomes run in a single feols() call per
#     treatment, sharing the expensive FE demeaning step (~5-10x OLS speedup).
#   - Outcome sd/mean pre-computed once per (spec x core) batch, not per treatment.
#   - Covariate complete-cases mask pre-computed once per batch.
#   - Per-treatment CSV writes: worst-case crash loss is 13 rows, not 182.
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 15, 2025. Last Updated: March 19, 2026.
rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Setup & Dependencies ----------------------------------------------------

start.time <- Sys.time()

# Run ID: shared across scripts 05 and 06 when launched from run_analysis.R.
# Falls back to a local timestamp when executed standalone.
run_id <- local({
  rid <- Sys.getenv("PIPELINE_RUN_ID", unset = "")
  if (nzchar(rid)) rid else format(start.time, "run_%Y%m%d_%H%M%S")
})

library(data.table)
library(fixest)
library(arrow)
library(dplyr)
library(here)
library(jsonlite)

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))

# Suppress fixest notes about removed singletons (they spam the console)
setFixest_notes(FALSE)
setFixest_nthreads(2)  # safe on 16GB with lean=TRUE; increase to 4 if 32GB+



message("===================================================================")
message("  06_run_regressions.R - Full-Sample Regressions")
message(sprintf("  Started: %s", format(start.time, "%Y-%m-%d %H:%M:%S")))
message(sprintf("  fixest version: %s", as.character(packageVersion("fixest"))))
message("===================================================================")

ccod_version   <- CCOD_VERSION
la_refined_dir <- EPC_LA_REFINED_DIR
matched_data_dir <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY)
output_dir     <- RESULTS_DIR
summary_dir    <- SUMMARY_TABLES_DIR

message(sprintf("  Matching geography: %s", MATCHING_GEOGRAPHY))
message(sprintf("  Matched data dir:   %s", matched_data_dir))
if (!dir.exists(matched_data_dir)) {
  stop("Matched data directory not found: ", matched_data_dir,
       "\n  Run 05_create_matched_pairs.R with MATCHING_GEOGRAPHY = \"", MATCHING_GEOGRAPHY, "\" first.")
}

# Write run manifest (once per geography level; skipped on crash-resume).
wc_suffix <- if (WITHIN_CORPORATE) "_wc" else ""
manifest_path <- file.path(SUMMARY_TABLES_DIR,
                           paste0("run_manifest_06_", MATCHING_GEOGRAPHY, wc_suffix, ".json"))
if (!file.exists(manifest_path)) {
  jsonlite::write_json(
    list(
      run_id             = run_id,
      script             = "06_run_regressions.R",
      matching_geography = MATCHING_GEOGRAPHY,
      ccod_version       = CCOD_VERSION,
      full_sample        = FULL_SAMPLE,
      r_version          = R.version$version.string,
      start_time         = format(start.time, "%Y-%m-%dT%H:%M:%S"),
      hostname           = Sys.info()[["nodename"]]
    ),
    manifest_path, auto_unbox = TRUE, pretty = TRUE
  )
  message(sprintf("  Run manifest written: %s", basename(manifest_path)))
} else {
  message(sprintf("  Run manifest exists (crash-resume): %s", basename(manifest_path)))
}

# Adapters ----------------------------------------------------------------
analysis_configs        <- treatment_metadata
regression_core_filters <- matching_core_filters

if (WITHIN_CORPORATE) {
  message(">>> WITHIN_CORPORATE mode: UK For-Profit as control, 3 treatments.")
  define_treatments <- define_within_corporate_treatments
  analysis_configs  <- within_corporate_metadata
}
active_control_label <- if (WITHIN_CORPORATE) within_corporate_control_label else standard_control_label

# Define which regression cores are valid given the matching core
valid_core_pairs <- list(
  base           = c("base", "council_tax", "ppd", "ppd_counciltax"),
  council_tax    = c("council_tax", "ppd_counciltax"),
  ppd            = c("ppd", "ppd_counciltax"),
  ppd_counciltax = c("ppd_counciltax")
)


# Outcome Variables -------------------------------------------------------
outcome_variables <- c(
  "bad_epc", "current_energy_efficiency",
  "energy_consumption_current", "energy_consumption_current_property",
  "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh", "energy_consumption_gap",
  "energy_consumption_gap_property", "energy_efficiency_potential_gap",
  "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap",
  "borderline_good_epc", "borderline_better_epc"
)


# Load Full Dataset (Memory-Efficient) ------------------------------------

message("\n--- Loading Data (column-selected via Arrow) ---")

# Define all columns needed across the regression pipeline
needed_cols <- unique(c(
  # Outcome variables
  outcome_variables,
  # Continuous covariates
  "number_habitable_rooms", "total_floor_area", "ppd_price_sqm",
  # Fixed effects / exact matching vars
  "lodgement_year", "property_type", "main_fuel", "construction_age_band",
  "built_form", "local_authority", "tax_band", "ppd_year_transfer",
  # Treatment definition inputs
  "source", "tenure_2", "coarse_proprietorship", "country_incorporated_1",
  "country_incorporated_tax_haven", "country_incorporated_british_haven",
  "country_incorporated_european_haven", "country_incorporated_caribbean_haven",
  "country_incorporated_other_haven",
  # PSM join key
  "uprn"
))

load_start <- proc.time()
all_parquet_files <- list.files(la_refined_dir, pattern = "\\.parquet$", full.names = TRUE)
unknown_mask <- grepl("unknown", basename(all_parquet_files), ignore.case = TRUE)
if (any(unknown_mask)) {
  message(sprintf("  Skipping %d unknown-LA file(s): %s",
                  sum(unknown_mask),
                  paste(basename(all_parquet_files[unknown_mask]), collapse = ", ")))
  all_parquet_files <- all_parquet_files[!unknown_mask]
}
ds <- arrow::open_dataset(all_parquet_files, format = "parquet")
all_parquet_cols <- names(ds)
available_cols   <- intersect(needed_cols, all_parquet_cols)
dropped_cols     <- setdiff(all_parquet_cols, available_cols)

message(sprintf("  Parquet schema: %d columns total", length(all_parquet_cols)))
message(sprintf("  Selecting %d columns needed for regressions (dropping %d unused)",
                length(available_cols), length(dropped_cols)))

missing_cols <- setdiff(needed_cols, all_parquet_cols)
if (length(missing_cols) > 0) {
  message(sprintf("  Note: %d expected columns not in parquet: %s",
                  length(missing_cols), paste(missing_cols, collapse = ", ")))
}

# Stream all parquet files into one Arrow Table, then convert
EPC_matched_combined <- ds |>
  select(all_of(available_cols)) |>
  collect() |>
  as.data.table()

rm(ds); gc()

load_elapsed <- (proc.time() - load_start)[["elapsed"]]
mem_mb <- as.numeric(object.size(EPC_matched_combined)) / 1024^2
message(sprintf("  Loaded: %s rows x %d cols (%.1f MB in memory, %.0f s)",
                format(nrow(EPC_matched_combined), big.mark = ","),
                ncol(EPC_matched_combined), mem_mb, load_elapsed))

# Re-apply treatment definitions
message("  Applying treatment definitions...")
EPC_matched_combined <- define_treatments(EPC_matched_combined)
setkey(EPC_matched_combined, uprn)
message(sprintf("  Defined %d treatment variables.", length(analysis_configs)))

# Drop treatment-definition input columns (no longer needed; saves ~300-600 MB)
treatment_input_cols <- c("source", "tenure_2", "coarse_proprietorship",
                          "country_incorporated_1", "country_incorporated_tax_haven",
                          "country_incorporated_british_haven",
                          "country_incorporated_european_haven",
                          "country_incorporated_caribbean_haven",
                          "country_incorporated_other_haven")
drop_cols <- intersect(treatment_input_cols, names(EPC_matched_combined))
if (length(drop_cols) > 0L) {
  EPC_matched_combined[, (drop_cols) := NULL]
  message(sprintf("  Dropped %d treatment-input columns (%.1f MB freed).",
                  length(drop_cols),
                  length(drop_cols) * nrow(EPC_matched_combined) * 8 / 1024^2))
}

# Pre-compute integer interaction FE columns for each spec -----------------
message("  Pre-computing interaction FE group columns for each spec...")
spec_fe_col <- character(length(spec_configs))
names(spec_fe_col) <- sapply(spec_configs, `[[`, "name")

for (sc in spec_configs) {
  col_name <- paste0("fe_interact_", gsub("[^a-z0-9]+", "_", tolower(sc$name)))
  spec_fe_col[sc$name] <- col_name
  grp_vars <- sc$exact_vars[sc$exact_vars != ""]
  EPC_matched_combined[, (col_name) := .GRP, by = grp_vars]
  message(sprintf("    %-40s %d unique groups", col_name,
                  uniqueN(EPC_matched_combined[[col_name]], na.rm = TRUE)))
}
message("  Done pre-computing FE groups.")


# Helper Functions --------------------------------------------------------

# Extract cluster count from a fixest model.
# model$G was removed in fixest >= 0.13; fitstat("G") is the stable API.
get_n_clusters <- function(model) {
  tryCatch(
    as.integer(fitstat(model, "G", simplify = TRUE)),
    error = function(e) NA_integer_
  )
}

build_formula <- function(outcomes, treatment_var, continuous_vars, exact_vars,
                          model_type, fe_group_col = NULL) {
  continuous_vars <- continuous_vars[continuous_vars != ""]
  exact_vars      <- exact_vars[exact_vars != ""]
  # Multi-outcome LHS for fixest: c(y1, y2, ...)
  if (length(outcomes) > 1L) {
    lhs <- paste0("c(", paste(outcomes, collapse = ", "), ")")
  } else {
    lhs <- outcomes
  }
  rhs         <- paste(c(treatment_var, continuous_vars), collapse = " + ")
  fe_additive <- paste(exact_vars, collapse = " + ")
  if (model_type == "OLS Additive FE") {
    if (fe_additive != "") as.formula(paste(lhs, "~", rhs, "|", fe_additive))
    else                   as.formula(paste(lhs, "~", rhs))
  } else if (model_type == "OLS Interactive FE") {
    if (!is.null(fe_group_col) && nzchar(fe_group_col)) {
      as.formula(paste(lhs, "~", rhs, "|", fe_group_col))
    } else if (fe_additive != "") {
      fe_interact <- paste(exact_vars, collapse = "^")
      as.formula(paste(lhs, "~", rhs, "|", fe_interact))
    } else {
      as.formula(paste(lhs, "~", rhs))
    }
  } else if (model_type %in% c("PSM (Matched)", "PSM (Matched) PS<=0.2", "PSM (Matched) PS<=0.1")) {
    as.formula(paste(lhs, "~", rhs, "|", fe_additive))
  } else if (model_type %in% c("PSM (Matched) + Subclass FE",
                               "PSM (Matched) PS<=0.2 + Subclass FE",
                               "PSM (Matched) PS<=0.1 + Subclass FE")) {
    as.formula(paste(lhs, "~", rhs, "| subclass"))
  } else {
    stop("Unknown model type: ", model_type)
  }
}

get_regression_core_filter <- function(regression_core) {
  if (regression_core %in% names(regression_core_filters))
    return(regression_core_filters[[regression_core]])
  stop("Unknown regression core: ", regression_core)
}

# Pre-compute the complete-cases mask for covariates + FE variables.
# Returns a logical vector of length nrow(data). Treatment- and outcome-independent.
precompute_covariate_mask <- function(data, continuous_vars, exact_vars) {
  cov_vars <- unique(c(continuous_vars, exact_vars))
  cov_vars <- cov_vars[cov_vars != ""]
  cov_vars <- intersect(cov_vars, names(data))
  if (length(cov_vars) == 0L) return(rep(TRUE, nrow(data)))
  Reduce(`&`, lapply(cov_vars, function(v) !is.na(data[[v]])))
}

# Fast counts using a pre-computed covariate mask.
get_simple_counts <- function(data, treat_var, outcome, cov_mask = NULL) {
  tryCatch({
    if (is.null(cov_mask)) {
      cc <- !is.na(data[[treat_var]]) & !is.na(data[[outcome]])
    } else {
      cc <- cov_mask & !is.na(data[[treat_var]]) & !is.na(data[[outcome]])
    }
    treat_values <- data[[treat_var]][cc]
    list(
      treated = as.integer(sum(treat_values == 1, na.rm = TRUE)),
      control = as.integer(sum(treat_values == 0, na.rm = TRUE))
    )
  }, error = function(e) list(treated = NA_integer_, control = NA_integer_))
}

# Load matched data for one (config, matching_core, spec_config) and join against
# the full dataset. No regression_core filter is applied — callers apply it per-rc.
load_matched_pairs <- function(config, matching_core, spec_config) {
  matched_file_path <- file.path(
    matched_data_dir,
    paste0("matched_pairs_", config$file_id,
           "_matching_core_", matching_core,
           "_", ccod_version, ".RData")
  )
  if (!file.exists(matched_file_path)) return(NULL)
  
  local_env <- new.env(parent = emptyenv())
  load_ok <- tryCatch({
    load(matched_file_path, envir = local_env)
    TRUE
  }, error = function(e) {
    message(sprintf("  WARN: failed to load %s: %s", basename(matched_file_path), conditionMessage(e)))
    FALSE
  })
  if (!load_ok) { rm(local_env); return(NULL) }
  
  matched_dt <- local_env$matched_results[[spec_config$name]]
  local_env$matched_results <- NULL
  local_env$matching_core_metadata <- NULL
  rm(local_env)
  
  if (is.null(matched_dt) || nrow(matched_dt) == 0) return(NULL)
  
  matched_joined <- EPC_matched_combined[matched_dt[!is.na(uprn)], on = "uprn", nomatch = 0]
  rm(matched_dt)
  
  if (nrow(matched_joined) == 0) return(NULL)
  matched_joined
}


# Hypothesis tag lookup ---------------------------------------------------
get_hypothesis_tag <- function(treatment_short_id, outcome) {
  if (outcome == "bad_epc")                                             return("H2")
  if (outcome %in% c("borderline_good_epc", "borderline_better_epc")) return("H3")
  if (treatment_short_id == "frfp")                                    return("H1a")
  if (treatment_short_id %in% c("np", "uknp", "frnp", "thnp"))        return("H1b")
  # Within-corporate treatments
  if (treatment_short_id == "wcff")                                    return("H1a_wc")
  if (treatment_short_id %in% c("wcnp", "wcps"))                      return("H1b_wc")
  "H1"
}

# Cluster count below this threshold triggers few_clusters_flag.
FEW_CLUSTERS_THRESHOLD <- 20L

# Error log ---------------------------------------------------------------
error_log_path <- file.path(summary_dir,
                            paste0("regression_errors_", MATCHING_GEOGRAPHY, wc_suffix, ".csv"))
error_log_schema <- data.table(
  run_id        = character(0),
  model         = character(0),
  spec          = character(0),
  matching_core = character(0),
  regression_core = character(0),
  treatment     = character(0),
  outcome       = character(0),
  status        = character(0),
  error_message = character(0)
)
if (!file.exists(error_log_path)) fwrite(error_log_schema, error_log_path)

# Output CSV Setup (with crash-resume support) ----------------------------
output_csv_path <- file.path(summary_dir, paste0("results_table_", MATCHING_GEOGRAPHY, wc_suffix, ".csv"))

results_schema <- data.table(
  coef               = numeric(0),
  se                 = numeric(0),
  p_value            = numeric(0),
  ci_lower           = numeric(0),
  ci_upper           = numeric(0),
  nobs               = integer(0),
  n_clusters         = integer(0),
  r2                 = numeric(0),
  standardised_coef  = numeric(0),
  pct_effect         = numeric(0),
  outcome            = character(0),
  outcome_sd         = numeric(0),
  outcome_mean       = numeric(0),
  treatment          = character(0),
  treatment_short_id = character(0),
  model              = character(0),
  spec               = character(0),
  matching_core      = character(0),
  regression_core    = character(0),
  matching_geography = character(0),
  treated_n          = integer(0),
  control_n          = integer(0),
  psm_n_eligible              = integer(0),
  psm_n_matched               = integer(0),
  psm_match_rate              = numeric(0),
  psm_match_rate_post_caliper = numeric(0),
  psm_caliper_attrition       = numeric(0),
  few_clusters_flag    = integer(0),
  convergence_warnings = integer(0),
  t_statistic          = numeric(0),
  degrees_of_freedom   = integer(0),
  n_fixed_effects      = integer(0),
  psm_n_matched_post_caliper = integer(0),
  outcome_n_valid      = integer(0),
  hypothesis_tag       = character(0),
  run_id               = character(0),
  status               = character(0),
  error_message        = character(0),
  control_definition   = character(0)
)

# Crash-resume: two tiers -------------------------------------------------
# 1. completed_batch_combos: old-style (model|spec|mc|rc) — for OLS copy and backward compat
# 2. completed_treat_combos: fine-grained (model|spec|mc|rc|treatment_short_id) — per-treatment skip
completed_batch_set <- new.env(hash = TRUE, parent = emptyenv(), size = 2000L)
completed_treat_set <- new.env(hash = TRUE, parent = emptyenv(), size = 10000L)
has_batch <- function(key) exists(key, envir = completed_batch_set, inherits = FALSE)
add_batch <- function(key) completed_batch_set[[key]] <- TRUE
has_treat <- function(key) exists(key, envir = completed_treat_set, inherits = FALSE)
add_treat <- function(key) completed_treat_set[[key]] <- TRUE
total_rows_written <- 0L
ols_error_counter  <- 0L
psm_error_counter  <- 0L

if (file.exists(output_csv_path)) {
  existing <- tryCatch(fread(output_csv_path), error = function(e) data.table())
  if (nrow(existing) > 0 &&
      all(c("model", "spec", "matching_core", "regression_core") %in% names(existing))) {

    # --- Pass 1: Remove retriable error rows (keep treatment_dropped) ----------
    # "error" rows are transient (may succeed on retry after code fix).
    # "treatment_dropped" rows are permanent (treatment variable absent from
    # coefficient table — deterministic, will never change on retry).
    # Keeping treatment_dropped prevents an infinite purge-retry cycle.
    coef_vals <- existing[["coef"]]
    is_retriable <- is.na(coef_vals)
    if ("status" %in% names(existing)) {
      status_vals <- as.character(existing[["status"]])
      status_vals[is.na(status_vals)] <- ""
      # Only strip rows that are both NA-coef AND not permanently dropped
      is_retriable <- is_retriable & !(status_vals %in% "treatment_dropped")
    }
    n_error_rows <- sum(is_retriable)
    if (n_error_rows > 0L) existing <- existing[!is_retriable]

    # --- Pass 2: Remove treatments with fewer outcomes than expected ---
    # A treatment that ran with 7/13 outcomes needs to re-run to pick up the
    # remaining 6. Compare outcome counts within the same model family
    # (OLS vs PSM) and matching_core, since PSM on matched data may
    # legitimately have fewer valid outcomes than OLS on the full sample.
    n_partial_rows <- 0L
    if (nrow(existing) > 0L &&
        all(c("treatment_short_id", "outcome") %in% names(existing))) {
      mc_tmp <- as.character(existing[["matching_core"]])
      mc_tmp[is.na(mc_tmp) | mc_tmp == ""] <- "NA_mc"
      model_family <- fifelse(grepl("^OLS", existing[["model"]]), "OLS", "PSM")
      # Data key scoped to model family + matching core (prevents OLS/PSM cross-contamination)
      existing[, .cr_data := paste(model_family, spec, mc_tmp, regression_core, treatment_short_id, sep = "|")]
      # Model-instance key = (model, spec, mc, rc, treatment) — unique to each run
      existing[, .cr_inst := paste(model, spec, mc_tmp, regression_core, treatment_short_id, sep = "|")]
      # Count outcomes per instance, then find the within-family max per data key
      inst_n   <- existing[, .(.n = .N), by = .cr_inst]
      data_max <- existing[, .(.n = .N), by = .(.cr_data, .cr_inst)
                           ][, .(.max = max(.n)), by = .cr_data]
      existing[inst_n,  .cr_n   := i..n,   on = ".cr_inst"]
      existing[data_max, .cr_max := i..max, on = ".cr_data"]
      is_partial <- existing$.cr_n < existing$.cr_max
      n_partial_rows <- sum(is_partial)
      if (n_partial_rows > 0L) {
        partial_insts <- unique(existing$.cr_inst[is_partial])
        n_partial_treats <- length(partial_insts)
        existing <- existing[!(.cr_inst %in% partial_insts)]
        message(sprintf("  Crash-resume: removing %d rows from %d treatments with partial outcomes.",
                        n_partial_rows, n_partial_treats))
      }
      existing[, c(".cr_data", ".cr_inst", ".cr_n", ".cr_max") := NULL]
      rm(mc_tmp, model_family, inst_n, data_max)
    }

    # Rewrite CSV if any rows were removed
    if (n_error_rows > 0L || n_partial_rows > 0L) {
      message(sprintf("  Crash-resume: purged %d error + %d partial-outcome rows for re-run.",
                      n_error_rows, n_partial_rows))
      fwrite(results_schema, output_csv_path)
      if (nrow(existing) > 0L) fwrite(existing, output_csv_path, append = TRUE)
    }

    if (nrow(existing) > 0L) {
      mc <- as.character(existing[["matching_core"]])
      mc[is.na(mc) | mc == ""] <- "NA_mc"
      # Fine-grained treatment-level keys (always populated first)
      if ("treatment_short_id" %in% names(existing)) {
        tsid <- as.character(existing[["treatment_short_id"]])
        tsid[is.na(tsid)] <- "NA_tsid"
        treat_keys <- unique(paste(existing[["model"]], existing[["spec"]],
                                   mc, existing[["regression_core"]],
                                   tsid, sep = "|"))
        for (tk in treat_keys) completed_treat_set[[tk]] <- TRUE
        rm(tsid, treat_keys)
      }
      # Batch keys: only register when ALL treatments are present for that
      # (model, spec, mc, rc) combo. Otherwise the coarse skip at the
      # regression-core level would mask per-treatment gaps.
      # We check for distinct treatment_short_id count rather than row count,
      # because different cores may have different numbers of available outcomes.
      all_treatment_ids <- vapply(analysis_configs, `[[`, character(1), "short_id")
      n_all_treatments  <- length(all_treatment_ids)
      if ("treatment_short_id" %in% names(existing)) {
        batch_keys_all <- paste(existing[["model"]], existing[["spec"]],
                                mc, existing[["regression_core"]], sep = "|")
        tsid_all <- as.character(existing[["treatment_short_id"]])
        # For each batch key, count distinct treatments present
        batch_treat_dt <- data.table(bk = batch_keys_all, tsid = tsid_all)
        batch_n_treats <- batch_treat_dt[, .(n_treats = uniqueN(tsid)), by = bk]
        for (i in seq_len(nrow(batch_n_treats))) {
          if (batch_n_treats$n_treats[i] >= n_all_treatments)
            completed_batch_set[[ batch_n_treats$bk[i] ]] <- TRUE
        }
        rm(batch_keys_all, tsid_all, batch_treat_dt, batch_n_treats)
      }
      total_rows_written <- nrow(existing)
      rm(mc)
    }

    message(sprintf("  Crash-resume: found %d valid rows (%d batch keys, %d treatment keys).",
                    total_rows_written, length(ls(completed_batch_set)),
                    length(ls(completed_treat_set))))

    rm(existing)
  } else {
    fwrite(results_schema, output_csv_path)
  }
} else {
  fwrite(results_schema, output_csv_path)
}


# Progress counters -------------------------------------------------------
last_update_time  <- Sys.time()
update_interval   <- 10   # seconds between progress messages

# Pre-compute expected OLS treatment count for progress reporting
n_valid_spec_core <- sum(sapply(spec_configs, function(sc) {
  length(intersect(spec_core_pairs[[sc$name]], names(regression_core_filters)))
}))
ols_total_treatments <- n_valid_spec_core * length(analysis_configs)


# OLS Copy-from-Other-Geography -------------------------------------------
message("\n--- Checking other geography CSVs for reusable OLS rows ---")

other_geos <- setdiff(c("LA", "ITL2", "ITL3"), MATCHING_GEOGRAPHY)
ols_model_names <- c("OLS Additive FE", "OLS Interactive FE")

for (other_geo in other_geos) {
  other_csv <- file.path(summary_dir, paste0("results_table_", other_geo, wc_suffix, ".csv"))
  if (!file.exists(other_csv)) next

  other_dt <- tryCatch(fread(other_csv), error = function(e) NULL)
  if (is.null(other_dt) || nrow(other_dt) == 0) next
  if (!all(c("model", "spec", "matching_core", "regression_core",
             "treatment_short_id") %in% names(other_dt))) next

  ols_rows <- other_dt[model %in% ols_model_names]
  if (nrow(ols_rows) == 0) next

  # Only copy rows with actual results (non-NA coef); skip error skeletons
  ols_rows <- ols_rows[!is.na(coef)]
  if (nrow(ols_rows) == 0) next

  # Also filter partial-outcome treatments from source (same logic as Pass 2).
  # Without this, a source geography with 7/13 outcomes would set treatment keys
  # and prevent the OLS loop from running the full 13.
  if ("treatment_short_id" %in% names(ols_rows) && "outcome" %in% names(ols_rows)) {
    ols_rows[, .src_data := paste(spec, regression_core, treatment_short_id, sep = "|")]
    ols_rows[, .src_inst := paste(model, spec, "NA_mc", regression_core, treatment_short_id, sep = "|")]
    src_inst_n <- ols_rows[, .(.n = .N), by = .src_inst]
    src_data_max <- ols_rows[, .(.n = .N), by = .(.src_data, .src_inst)
                             ][, .(.max = max(.n)), by = .src_data]
    ols_rows[src_inst_n,  .src_n   := i..n,   on = ".src_inst"]
    ols_rows[src_data_max, .src_max := i..max, on = ".src_data"]
    n_src_partial <- sum(ols_rows$.src_n < ols_rows$.src_max)
    if (n_src_partial > 0L) {
      partial_src_insts <- unique(ols_rows$.src_inst[ols_rows$.src_n < ols_rows$.src_max])
      ols_rows <- ols_rows[!(.src_inst %in% partial_src_insts)]
      message(sprintf("  [OLS copy] %s: skipped %d partial-outcome rows from source.",
                      other_geo, n_src_partial))
    }
    ols_rows[, c(".src_data", ".src_inst", ".src_n", ".src_max") := NULL]
    rm(src_inst_n, src_data_max)
    if (nrow(ols_rows) == 0) { rm(other_dt); next }
  }

  # Use treatment-level keys so we can fill individual missing treatments
  mc_src <- as.character(ols_rows[["matching_core"]])
  mc_src[is.na(mc_src) | mc_src == ""] <- "NA_mc"
  tsid_src <- as.character(ols_rows[["treatment_short_id"]])
  tsid_src[is.na(tsid_src)] <- "NA_tsid"
  src_treat_keys <- paste(ols_rows[["model"]], ols_rows[["spec"]],
                          mc_src, ols_rows[["regression_core"]],
                          tsid_src, sep = "|")
  is_new <- !vapply(src_treat_keys, has_treat, logical(1))
  copy_rows <- ols_rows[is_new]
  if (nrow(copy_rows) == 0) {
    message(sprintf("  [OLS copy] %s: all treatments already present, skipping.", other_geo))
    rm(other_dt, ols_rows); next
  }

  copy_rows[, matching_geography := MATCHING_GEOGRAPHY]
  for (missing_col in setdiff(names(results_schema), names(copy_rows))) {
    col_type <- class(results_schema[[missing_col]])
    copy_rows[, (missing_col) := switch(col_type,
                                        numeric   = NA_real_,
                                        integer   = NA_integer_,
                                        character = NA_character_,
                                        NA
    )]
  }
  setcolorder(copy_rows, names(results_schema))
  fwrite(copy_rows, output_csv_path, append = TRUE)
  total_rows_written <- total_rows_written + nrow(copy_rows)

  # Register treatment-level keys from copied rows
  new_treat_keys <- unique(src_treat_keys[is_new])
  for (tk in new_treat_keys) add_treat(tk)

  # Register batch keys only for batches that are now fully complete
  mc_copy <- as.character(copy_rows[["matching_core"]])
  mc_copy[is.na(mc_copy) | mc_copy == ""] <- "NA_mc"
  copied_batch_keys <- unique(paste(copy_rows[["model"]], copy_rows[["spec"]],
                                    mc_copy, copy_rows[["regression_core"]], sep = "|"))
  n_outcomes_exp <- length(outcome_variables)
  n_treats_exp   <- length(analysis_configs)
  for (bk in copied_batch_keys) {
    # Count all treatment-level keys for this batch (from all sources)
    n_present <- sum(vapply(
      paste(bk, sapply(analysis_configs, `[[`, "short_id"), sep = "|"),
      has_treat, logical(1)
    ))
    if (n_present >= n_treats_exp) add_batch(bk)
  }

  message(sprintf("  [OLS copy] Copied %d rows (%d treatments) from %s -> %s",
                  nrow(copy_rows), length(new_treat_keys), other_geo, MATCHING_GEOGRAPHY))
  rm(other_dt, ols_rows, copy_rows)
  break  # one source is enough
}


# Helper: extract results from one fixest model ----------------------------
# Used by both OLS and PSM sections to avoid duplicating the extraction logic.
extract_one_model <- function(model, config, current_outcome, current_model_name,
                              spec_name, matching_core_val, regression_core,
                              out_sd, out_mean, hyp_tag, treated_n, control_n,
                              n_warnings,
                              psm_n_eligible       = NA_integer_,
                              psm_n_matched        = NA_integer_,
                              psm_mr               = NA_real_,
                              psm_mr_post_cal      = NA_real_,
                              psm_cal_attr         = NA_real_,
                              psm_n_matched_post_cal = NA_integer_,
                              outcome_n_valid      = NA_integer_) {
  ct      <- coeftable(model)
  n_obs   <- nobs(model)
  r2_val  <- r2(model, "r2")
  n_clust <- get_n_clusters(model)
  ci      <- confint(model, level = 0.95)
  
  if (!(config$var %in% rownames(ct))) {
    fwrite(data.table(run_id = run_id, model = current_model_name, spec = spec_name,
                      matching_core = matching_core_val, regression_core = regression_core,
                      treatment = config$short_id, outcome = current_outcome,
                      status = "treatment_dropped",
                      error_message = "treatment variable absent from coefficient table"),
           error_log_path, append = TRUE)
    list(
      result = data.table(
        coef = NA_real_, se = NA_real_, p_value = NA_real_,
        ci_lower = NA_real_, ci_upper = NA_real_,
        nobs = n_obs, n_clusters = n_clust, r2 = r2_val,
        standardised_coef = NA_real_, pct_effect = NA_real_,
        outcome = current_outcome, outcome_sd = out_sd, outcome_mean = out_mean,
        treatment = config$title, treatment_short_id = config$short_id,
        model = current_model_name, spec = spec_name,
        matching_core = matching_core_val, regression_core = regression_core,
        matching_geography = MATCHING_GEOGRAPHY,
        treated_n = treated_n, control_n = control_n,
        psm_n_eligible              = as.integer(psm_n_eligible),
        psm_n_matched               = as.integer(psm_n_matched),
        psm_match_rate              = psm_mr,
        psm_match_rate_post_caliper = psm_mr_post_cal,
        psm_caliper_attrition       = psm_cal_attr,
        few_clusters_flag    = as.integer(!is.na(n_clust) && n_clust < FEW_CLUSTERS_THRESHOLD),
        convergence_warnings = n_warnings,
        t_statistic          = NA_real_,
        degrees_of_freedom   = NA_integer_,
        n_fixed_effects      = NA_integer_,
        psm_n_matched_post_caliper = as.integer(psm_n_matched_post_cal),
        outcome_n_valid      = as.integer(outcome_n_valid),
        hypothesis_tag = hyp_tag,
        run_id = run_id, status = "treatment_dropped",
        error_message = "treatment variable absent from coefficient table",
        control_definition = active_control_label
      ),
      archive = NULL
    )
  } else {
    row      <- ct[config$var, ]
    coef_val <- row["Estimate"]
    t_stat   <- row["t value"]
    df_val   <- tryCatch(as.integer(degrees_freedom(model, type = "t")),
                         error = function(e) NA_integer_)
    n_fe     <- tryCatch({
      fs <- model$fixef_sizes
      if (is.null(fs)) fs <- summary(model)$fixef_sizes
      if (!is.null(fs)) as.integer(sum(fs)) else NA_integer_
    }, error = function(e) NA_integer_)
    
    list(
      result = data.table(
        coef               = coef_val,
        se                 = row["Std. Error"],
        p_value            = row["Pr(>|t|)"],
        ci_lower           = ci[config$var, 1L],
        ci_upper           = ci[config$var, 2L],
        nobs               = n_obs,
        n_clusters         = n_clust,
        r2                 = r2_val,
        standardised_coef  = coef_val / out_sd,
        pct_effect         = fifelse(out_mean > 0, coef_val / out_mean, NA_real_),
        outcome            = current_outcome,
        outcome_sd         = out_sd,
        outcome_mean       = out_mean,
        treatment          = config$title,
        treatment_short_id = config$short_id,
        model              = current_model_name,
        spec               = spec_name,
        matching_core      = matching_core_val,
        regression_core    = regression_core,
        matching_geography = MATCHING_GEOGRAPHY,
        treated_n          = treated_n,
        control_n          = control_n,
        psm_n_eligible              = as.integer(psm_n_eligible),
        psm_n_matched               = as.integer(psm_n_matched),
        psm_match_rate              = psm_mr,
        psm_match_rate_post_caliper = psm_mr_post_cal,
        psm_caliper_attrition       = psm_cal_attr,
        few_clusters_flag    = as.integer(!is.na(n_clust) && n_clust < FEW_CLUSTERS_THRESHOLD),
        convergence_warnings = n_warnings,
        t_statistic          = t_stat,
        degrees_of_freedom   = df_val,
        n_fixed_effects      = n_fe,
        psm_n_matched_post_caliper = as.integer(psm_n_matched_post_cal),
        outcome_n_valid      = as.integer(outcome_n_valid),
        hypothesis_tag       = hyp_tag,
        run_id               = run_id,
        status               = "ok",
        error_message        = NA_character_,
        control_definition   = active_control_label
      ),
      archive = extract_archive_row(
        outcome            = current_outcome,
        treatment_short_id = config$short_id,
        model_name         = current_model_name,
        spec_name          = spec_name,
        matching_core      = matching_core_val,
        regression_core    = regression_core,
        matching_geography = MATCHING_GEOGRAPHY,
        coef               = coef_val,
        se                 = row["Std. Error"],
        vcov_treatment     = tryCatch(vcov(model)[config$var, config$var], error = function(e) NA_real_),
        t_statistic        = t_stat,
        p_value            = row["Pr(>|t|)"],
        ci_lower           = ci[config$var, 1L],
        ci_upper           = ci[config$var, 2L],
        nobs               = n_obs,
        n_clusters         = n_clust,
        r2                 = r2_val,
        n_fixed_effects    = n_fe,
        degrees_of_freedom = df_val,
        run_id             = run_id
      )
    )
  }
}

# Helper: build one row for the model archive Parquet file ----------------
extract_archive_row <- function(outcome, treatment_short_id, model_name, spec_name,
                                matching_core, regression_core, matching_geography,
                                coef, se, vcov_treatment, t_statistic, p_value,
                                ci_lower, ci_upper, nobs, n_clusters, r2,
                                n_fixed_effects, degrees_of_freedom, run_id) {
  data.table(
    outcome            = outcome,
    treatment_short_id = treatment_short_id,
    model              = model_name,
    spec               = spec_name,
    matching_core      = matching_core,
    regression_core    = regression_core,
    matching_geography = matching_geography,
    coef               = coef,
    se                 = se,
    vcov_treatment     = vcov_treatment,
    t_statistic        = t_statistic,
    p_value            = p_value,
    ci_lower           = ci_lower,
    ci_upper           = ci_upper,
    nobs               = nobs,
    n_clusters         = n_clusters,
    r2                 = r2,
    n_fixed_effects    = n_fixed_effects,
    degrees_of_freedom = degrees_of_freedom,
    run_id             = run_id
  )
}

# Helper: build a skeleton error row for a single outcome -----------------
make_error_row <- function(current_outcome, config, current_model_name, spec_name,
                           matching_core_val, regression_core,
                           out_sd, out_mean, hyp_tag, treated_n, control_n,
                           n_warnings, error_msg,
                           psm_n_eligible         = NA_integer_,
                           psm_n_matched          = NA_integer_,
                           psm_mr                 = NA_real_,
                           psm_mr_post_cal        = NA_real_,
                           psm_cal_attr           = NA_real_,
                           psm_n_matched_post_cal = NA_integer_,
                           outcome_n_valid        = NA_integer_) {
  data.table(
    coef = NA_real_, se = NA_real_, p_value = NA_real_,
    ci_lower = NA_real_, ci_upper = NA_real_,
    nobs = NA_integer_, n_clusters = NA_integer_, r2 = NA_real_,
    standardised_coef = NA_real_, pct_effect = NA_real_,
    outcome = current_outcome, outcome_sd = out_sd, outcome_mean = out_mean,
    treatment = config$title, treatment_short_id = config$short_id,
    model = current_model_name, spec = spec_name,
    matching_core = matching_core_val, regression_core = regression_core,
    matching_geography = MATCHING_GEOGRAPHY,
    treated_n = treated_n, control_n = control_n,
    psm_n_eligible              = as.integer(psm_n_eligible),
    psm_n_matched               = as.integer(psm_n_matched),
    psm_match_rate              = psm_mr,
    psm_match_rate_post_caliper = psm_mr_post_cal,
    psm_caliper_attrition       = psm_cal_attr,
    few_clusters_flag    = NA_integer_,
    convergence_warnings = n_warnings,
    t_statistic          = NA_real_,
    degrees_of_freedom   = NA_integer_,
    n_fixed_effects      = NA_integer_,
    psm_n_matched_post_caliper = as.integer(psm_n_matched_post_cal),
    outcome_n_valid      = as.integer(outcome_n_valid),
    hypothesis_tag = hyp_tag,
    run_id = run_id, status = "error", error_message = error_msg,
    control_definition = active_control_label
  )
}


# OLS Models ---------------------------------------------------------------
# Multi-outcome feols: all 13 outcomes in one call per treatment.
# Shares the expensive FE demeaning step across outcomes (~5-10x speedup).
# Per-treatment CSV writes: crash loses at most 13 rows, not 182.

message("\n=== Running OLS Models ===")

for (current_model_name in c("OLS Interactive FE", "OLS Additive FE")) {
  message(sprintf("\n--- %s ---", current_model_name))
  ols_treat_counter <- 0L
  last_update_time  <- Sys.time()
  archive_buffer    <- list()
  
  for (spec_config in spec_configs) {
    spec_name     <- spec_config$name
    allowed_cores <- spec_core_pairs[[spec_name]]
    fe_col <- if (current_model_name == "OLS Interactive FE") spec_fe_col[spec_name] else NULL
    
    for (regression_core in names(regression_core_filters)) {
      if (!(regression_core %in% allowed_cores)) next
      
      # Coarse crash-resume: skip if the entire old-style batch is complete
      batch_key <- paste(current_model_name, spec_name, "NA_mc", regression_core, sep = "|")
      if (has_batch(batch_key)) {
        message(sprintf("  [SKIP] Already complete: %s | %s | %s",
                        current_model_name, spec_name, regression_core))
        next
      }
      
      # Compute subset index for this regression core
      if (regression_core == "base") {
        ols_subset_idx <- NULL
      } else {
        ols_subset_idx <- which(EPC_matched_combined[, eval(get_regression_core_filter(regression_core))])
        if (length(ols_subset_idx) == 0L) next
      }
      
      # Helper: extract a column vector respecting the subset index
      ols_col <- function(col) {
        v <- EPC_matched_combined[[col]]
        if (!is.null(ols_subset_idx)) v[ols_subset_idx] else v
      }
      
      # Pre-compute outcome stats ONCE for this (spec x core) — not per treatment
      available_outcomes <- intersect(outcome_variables, names(EPC_matched_combined))
      available_outcomes <- available_outcomes[
        vapply(available_outcomes, function(ov) any(!is.na(ols_col(ov))), logical(1))
      ]
      if (length(available_outcomes) == 0L) next
      
      ols_sd_cache   <- vapply(available_outcomes, function(ov)
        sd(ols_col(ov), na.rm = TRUE), numeric(1))
      ols_mean_cache <- vapply(available_outcomes, function(ov)
        mean(ols_col(ov), na.rm = TRUE), numeric(1))
      
      # Pre-compute covariate complete-cases mask (once per batch)
      cov_vars <- unique(c(spec_config$continuous_vars, spec_config$exact_vars))
      cov_vars <- cov_vars[cov_vars != ""]
      cov_vars <- intersect(cov_vars, names(EPC_matched_combined))
      if (length(cov_vars) == 0L) {
        cov_mask <- rep(TRUE, if (is.null(ols_subset_idx)) nrow(EPC_matched_combined) else length(ols_subset_idx))
      } else {
        cov_mask <- Reduce(`&`, lapply(cov_vars, function(v) !is.na(ols_col(v))))
      }
      
      for (config in analysis_configs) {
        ols_treat_counter <- ols_treat_counter + 1L
        
        # Fine-grained crash-resume: skip if this treatment already done
        treat_key <- paste(current_model_name, spec_name, "NA_mc", regression_core,
                           config$short_id, sep = "|")
        if (has_treat(treat_key)) {
          next
        }
        
        # Progress
        current_time <- Sys.time()
        if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
          elapsed <- as.numeric(difftime(current_time, start.time, units = "secs"))
          pct     <- ols_treat_counter / ols_total_treatments * 100
          message(sprintf("  [%3.0f%%] Treatment %d/%d | %.0f s | %s | %s | %s | %s",
                          pct, ols_treat_counter, ols_total_treatments, elapsed,
                          current_model_name, spec_name, regression_core, config$var))
          last_update_time <- current_time
        }
        
        # Build multi-outcome formula
        fml <- build_formula(
          outcomes        = available_outcomes,
          treatment_var   = config$var,
          continuous_vars = spec_config$continuous_vars,
          exact_vars      = spec_config$exact_vars,
          model_type      = current_model_name,
          fe_group_col    = fe_col
        )
        
        # Treatment-specific mask (for counts)
        treat_mask <- cov_mask & !is.na(ols_col(config$var))
        
        # Run all outcomes at once
        ols_warnings <- character(0)
        batch_error_msg <- NA_character_
        multi_result <- tryCatch({
          withCallingHandlers(
            { feols(fml, data = EPC_matched_combined, subset = ols_subset_idx, cluster = ~local_authority, lean = TRUE) },
            warning = function(w) {
              ols_warnings <<- c(ols_warnings, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          )
        }, error = function(e) {
          batch_error_msg <<- conditionMessage(e)
          message(sprintf("  WARN [OLS multi %s|%s|%s|%s]: %s",
                          current_model_name, spec_name, config$var, regression_core,
                          conditionMessage(e)))
          NULL
        })
        
        # Normalise: fixest_multi → named list keyed by outcome variable name.
        # At full scale, fixest may silently drop outcomes from multi-LHS calls;
        # named access lets us detect this and fall back to single-outcome feols.
        if (!is.null(multi_result) && inherits(multi_result, "fixest_multi")) {
          multi_result <- as.list(multi_result)
          names(multi_result) <- sub("^lhs: ", "", names(multi_result))
        } else if (!is.null(multi_result) && inherits(multi_result, "fixest")) {
          multi_result <- setNames(list(multi_result), available_outcomes[1L])
        }

        # Extract results for each outcome
        treat_rows <- vector("list", length(available_outcomes))
        local_errors <- 0L

        for (i in seq_along(available_outcomes)) {
          current_outcome <- available_outcomes[i]
          out_sd   <- ols_sd_cache[current_outcome]
          out_mean <- ols_mean_cache[current_outcome]
          hyp_tag  <- get_hypothesis_tag(config$short_id, current_outcome)

          # Fast per-outcome counts
          out_cc <- treat_mask & !is.na(ols_col(current_outcome))
          tv <- ols_col(config$var)[out_cc]
          ols_treated_n <- as.integer(sum(tv == 1L, na.rm = TRUE))
          ols_control_n <- as.integer(sum(tv == 0L, na.rm = TRUE))

          # Resolve model for this outcome: multi-result → single-outcome fallback
          model_for_outcome <- NULL
          fallback_used <- FALSE
          if (is.null(multi_result)) {
            # Entire multi-outcome call failed — will use error path below
          } else if (current_outcome %in% names(multi_result)) {
            model_for_outcome <- multi_result[[current_outcome]]
          } else {
            # Outcome dropped by fixest at scale — single-outcome fallback
            fallback_used <- TRUE
            single_fml <- build_formula(
              current_outcome, config$var, spec_config$continuous_vars,
              spec_config$exact_vars, current_model_name, fe_col
            )
            fb_warnings <- character(0)
            model_for_outcome <- tryCatch(
              withCallingHandlers(
                feols(single_fml, data = EPC_matched_combined,
                      subset = ols_subset_idx, cluster = ~local_authority, lean = TRUE),
                warning = function(w) { fb_warnings <<- c(fb_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
              ),
              error = function(e) {
                message(sprintf("  WARN [OLS fallback %s|%s|%s|%s|%s]: %s",
                                current_model_name, spec_name, config$var,
                                regression_core, current_outcome, conditionMessage(e)))
                NULL
              }
            )
            if (length(fb_warnings) > 0L)
              ols_warnings <- c(ols_warnings, fb_warnings)
          }

          if (is.null(multi_result) && !fallback_used) {
            # Entire multi-outcome call failed → error skeleton
            local_errors <- local_errors + 1L
            fwrite(data.table(run_id = run_id, model = current_model_name, spec = spec_name,
                              matching_core = NA_character_, regression_core = regression_core,
                              treatment = config$short_id, outcome = current_outcome,
                              status = "error",
                              error_message = if (is.na(batch_error_msg)) "multi-outcome feols failed" else batch_error_msg),
                   error_log_path, append = TRUE)
            treat_rows[[i]] <- make_error_row(
              current_outcome, config, current_model_name, spec_name,
              NA_character_, regression_core, out_sd, out_mean, hyp_tag,
              ols_treated_n, ols_control_n, length(ols_warnings),
              if (is.na(batch_error_msg)) "multi-outcome feols failed" else batch_error_msg
            )
          } else if (!is.null(model_for_outcome)) {
            treat_rows[[i]] <- tryCatch({
              eom <- extract_one_model(
                model = model_for_outcome, config = config,
                current_outcome = current_outcome,
                current_model_name = current_model_name,
                spec_name = spec_name, matching_core_val = NA_character_,
                regression_core = regression_core,
                out_sd = out_sd, out_mean = out_mean, hyp_tag = hyp_tag,
                treated_n = ols_treated_n, control_n = ols_control_n,
                n_warnings = length(ols_warnings),
                outcome_n_valid = as.integer(sum(out_cc))
              )
              if (!is.null(eom$archive)) archive_buffer[[length(archive_buffer) + 1L]] <- eom$archive
              eom$result
            }, error = function(e) {
              local_errors <<- local_errors + 1L
              fwrite(data.table(run_id = run_id, model = current_model_name, spec = spec_name,
                                matching_core = NA_character_, regression_core = regression_core,
                                treatment = config$short_id, outcome = current_outcome,
                                status = "error", error_message = conditionMessage(e)),
                     error_log_path, append = TRUE)
              make_error_row(
                current_outcome, config, current_model_name, spec_name,
                NA_character_, regression_core, out_sd, out_mean, hyp_tag,
                ols_treated_n, ols_control_n, length(ols_warnings),
                conditionMessage(e)
              )
            })
          } else {
            # Both multi and single-outcome feols failed — permanent failure
            treat_rows[[i]] <- make_error_row(
              current_outcome, config, current_model_name, spec_name,
              NA_character_, regression_core, out_sd, out_mean, hyp_tag,
              ols_treated_n, ols_control_n, length(ols_warnings),
              "outcome unavailable (multi + single-outcome feols both failed)"
            )
            treat_rows[[i]][, status := "treatment_dropped"]
          }
          # Release model objects — prevents C-level finalizers from accumulating
          if (!is.null(multi_result) && current_outcome %in% names(multi_result))
            multi_result[[current_outcome]] <- NULL
          if (exists("model_for_outcome", inherits = FALSE)) rm(model_for_outcome)
        }  # end outcome extraction
        
        # Write this treatment's rows immediately (per-treatment crash-resume)
        if (length(treat_rows) > 0L) {
          treat_dt <- rbindlist(treat_rows)
          if (nrow(treat_dt) > 0L) {
            fwrite(treat_dt, output_csv_path, append = TRUE)
            total_rows_written <- total_rows_written + nrow(treat_dt)
            add_treat(treat_key)
            message(sprintf("  [OLS] Wrote %d rows (total: %d) | %s | %s | %s | %s",
                            nrow(treat_dt), total_rows_written,
                            current_model_name, spec_name, regression_core, config$short_id))
          }
          rm(treat_dt)
        }
        
        if (local_errors > 0L) ols_error_counter <- ols_error_counter + local_errors
        
        rm(multi_result, treat_rows)
        
        # Safety-net gc: drain residual C-level finalizers every 5 treatments
        if (ols_treat_counter %% 5L == 0L) gc()
        
      }  # end config (treatment) loop
      
      # Flush OLS archive for this regression_core batch
      if (length(archive_buffer) > 0L) {
        archive_dt    <- rbindlist(archive_buffer)
        archive_fname <- paste0("archive_",
                                gsub("[^a-z0-9]+", "_", tolower(current_model_name)), "_",
                                spec_config$short_id, "_",
                                "NA_mc", "_",
                                regression_core, ".parquet")
        archive_path  <- file.path(MODEL_ARCHIVE_DIR, archive_fname)
        # Append-safe: combine with existing rows from a prior crash-resume
        if (file.exists(archive_path)) {
          existing_archive <- tryCatch(as.data.table(arrow::read_parquet(archive_path)),
                                       error = function(e) NULL)
          if (!is.null(existing_archive) && nrow(existing_archive) > 0L) {
            existing_keys <- paste(existing_archive$outcome, existing_archive$treatment_short_id, sep = "|")
            new_keys      <- paste(archive_dt$outcome, archive_dt$treatment_short_id, sep = "|")
            archive_dt    <- rbindlist(list(existing_archive, archive_dt[!new_keys %in% existing_keys]),
                                       use.names = TRUE, fill = TRUE)
          }
          rm(existing_archive); gc()
        }
        # Write to temp then rename to avoid Windows memory-mapped file lock
        archive_tmp <- paste0(archive_path, ".tmp")
        arrow::write_parquet(archive_dt, archive_tmp)
        file.rename(archive_tmp, archive_path)
        message(sprintf("  Archive flushed: %s (%d rows)", archive_fname, nrow(archive_dt)))
        rm(archive_dt)
      }
      archive_buffer <- list()
      
      rm(ols_subset_idx, cov_mask, ols_sd_cache, ols_mean_cache, ols_col)
      gc()   # drain finalizer queue between regression cores
    }  # end regression_core loop
  }  # end spec_config loop
  
  if (length(archive_buffer) > 0L) {
    message(sprintf("  WARNING: %d orphaned archive rows at end of %s (should be 0)",
                    length(archive_buffer), current_model_name))
    archive_buffer <- list()
  }
  
  message(sprintf("  Completed %s: %d treatment batches processed.", current_model_name, ols_treat_counter))
}


# PSM Models ---------------------------------------------------------------
# Multi-outcome feols: all outcomes in one call per (treatment x regression_core).
# Per-treatment CSV writes for crash resilience.

# Pre-compute psm_n_eligible per (treatment, regression_core).
message("\n--- Pre-computing psm_n_eligible per (treatment, regression_core) ---")
psm_eligible_cache <- list()
for (rc in names(regression_core_filters)) {
  if (rc == "base") {
    rc_idx <- NULL
  } else {
    rc_idx <- which(EPC_matched_combined[, eval(get_regression_core_filter(rc))])
  }
  for (cfg in analysis_configs) {
    v <- if (is.null(rc_idx)) EPC_matched_combined[[cfg$var]] else EPC_matched_combined[[cfg$var]][rc_idx]
    psm_eligible_cache[[paste0(cfg$short_id, "|", rc)]] <-
      as.integer(sum(v == 1L, na.rm = TRUE))
  }
  rm(rc_idx)
}
gc()

message("\n=== Running PSM Models ===")

psm_models_counter <- 0L

for (current_model_name in c("PSM (Matched)", "PSM (Matched) + Subclass FE",
                             "PSM (Matched) PS<=0.2", "PSM (Matched) PS<=0.2 + Subclass FE",
                             "PSM (Matched) PS<=0.1", "PSM (Matched) PS<=0.1 + Subclass FE")) {
  message(sprintf("\n--- %s ---", current_model_name))
  last_update_time <- Sys.time()
  archive_buffer   <- list()
  
  for (spec_config in spec_configs) {
    spec_name     <- spec_config$name
    allowed_cores <- spec_core_pairs[[spec_name]]
    
    for (matching_core in names(matching_core_filters)) {
      valid_regression_cores <- valid_core_pairs[[matching_core]]
      model_cores <- intersect(allowed_cores, valid_regression_cores)
      if (length(model_cores) == 0L) next
      
      for (config in analysis_configs) {
        
        # Skip load if every (rc × this_treatment) is already done
        all_rc_done <- all(vapply(model_cores, function(rc) {
          tk <- paste(current_model_name, spec_name, matching_core, rc, config$short_id, sep = "|")
          bk <- paste(current_model_name, spec_name, matching_core, rc, sep = "|")
          has_treat(tk) || has_batch(bk)
        }, logical(1L)))
        if (all_rc_done) next
        
        # Load matched data once for this (treatment × matching_core × spec)
        matched_joined <- load_matched_pairs(config, matching_core, spec_config)
        if (is.null(matched_joined) || nrow(matched_joined) == 0L) next
        
        for (regression_core in model_cores) {
          
          # Crash-resume: check both fine-grained and old-style keys
          treat_key <- paste(current_model_name, spec_name, matching_core, regression_core,
                             config$short_id, sep = "|")
          batch_key <- paste(current_model_name, spec_name, matching_core, regression_core, sep = "|")
          if (has_treat(treat_key) || has_batch(batch_key)) next
          
          # Compute regression_core subset index (avoids copy when mc != rc)
          if (matching_core != regression_core) {
            rc_filter <- get_regression_core_filter(regression_core)
            psm_rc_subset <- which(matched_joined[, eval(rc_filter)])
            if (length(psm_rc_subset) == 0L) next
            psm_data_ref <- matched_joined  # reference, not copy
          } else {
            psm_rc_subset <- NULL
            psm_data_ref <- matched_joined
            if (nrow(psm_data_ref) == 0L) next
          }
          
          # Helper: subsettable column accessor (redefined after caliper update)
          psm_col <- function(col) {
            v <- psm_data_ref[[col]]
            if (!is.null(psm_rc_subset)) v[psm_rc_subset] else v
          }
          
          # Pre-caliper counts
          psm_n_matched  <- sum(psm_col(config$var) == 1L, na.rm = TRUE)
          psm_n_eligible <- psm_eligible_cache[[paste0(config$short_id, "|", regression_core)]]
          
          # PS caliper — further restrict the subset
          if (grepl("PS<=0.1", current_model_name, fixed = TRUE)) {
            if (!"distance" %in% names(psm_data_ref)) next
            dist_vals <- psm_col("distance")
            caliper_keep <- which(dist_vals <= 0.1)
            if (length(caliper_keep) == 0L) next
            if (is.null(psm_rc_subset)) {
              psm_rc_subset <- caliper_keep
            } else {
              psm_rc_subset <- psm_rc_subset[caliper_keep]
            }
          } else if (grepl("PS<=0.2", current_model_name, fixed = TRUE)) {
            if (!"distance" %in% names(psm_data_ref)) next
            dist_vals <- psm_col("distance")
            caliper_keep <- which(dist_vals <= 0.2)
            if (length(caliper_keep) == 0L) next
            if (is.null(psm_rc_subset)) {
              psm_rc_subset <- caliper_keep
            } else {
              psm_rc_subset <- psm_rc_subset[caliper_keep]
            }
          }
          # Redefine helper after caliper update
          psm_col <- function(col) {
            v <- psm_data_ref[[col]]
            if (!is.null(psm_rc_subset)) v[psm_rc_subset] else v
          }
          
          # Post-caliper counts
          psm_n_matched_post_cal <- as.integer(sum(psm_col(config$var) == 1L, na.rm = TRUE))
          psm_mr        <- if (psm_n_eligible > 0L) psm_n_matched         / psm_n_eligible else NA_real_
          psm_mr_post_cal <- if (psm_n_eligible > 0L) psm_n_matched_post_cal / psm_n_eligible else NA_real_
          psm_cal_attr  <- if (psm_n_matched > 0L)
            (psm_n_matched - psm_n_matched_post_cal) / psm_n_matched
          else NA_real_
          
          # Determine available outcomes for this matched dataset
          psm_avail_outcomes <- intersect(outcome_variables, names(psm_data_ref))
          psm_avail_outcomes <- psm_avail_outcomes[
            vapply(psm_avail_outcomes, function(ov) any(!is.na(psm_col(ov))), logical(1))
          ]
          if (length(psm_avail_outcomes) == 0L) next
          
          psm_models_counter <- psm_models_counter + length(psm_avail_outcomes)
          
          # Progress
          current_time <- Sys.time()
          if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
            elapsed <- as.numeric(difftime(current_time, start.time, units = "secs"))
            message(sprintf("  PSM model %d | %.0f s | %s | %s | %s->%s | %s",
                            psm_models_counter, elapsed, current_model_name,
                            spec_name, matching_core, regression_core, config$var))
            last_update_time <- current_time
          }
          
          # Pre-compute outcome stats for this matched dataset
          psm_sd_cache   <- vapply(psm_avail_outcomes, function(ov)
            sd(psm_col(ov), na.rm = TRUE), numeric(1))
          psm_mean_cache <- vapply(psm_avail_outcomes, function(ov)
            mean(psm_col(ov), na.rm = TRUE), numeric(1))
          
          # Pre-compute covariate mask for this matched dataset
          psm_cov_vars <- unique(c(spec_config$continuous_vars, spec_config$exact_vars))
          psm_cov_vars <- psm_cov_vars[psm_cov_vars != ""]
          psm_cov_vars <- intersect(psm_cov_vars, names(psm_data_ref))
          if (length(psm_cov_vars) == 0L) {
            psm_cov_mask <- rep(TRUE, if (is.null(psm_rc_subset)) nrow(psm_data_ref) else length(psm_rc_subset))
          } else {
            psm_cov_mask <- Reduce(`&`, lapply(psm_cov_vars, function(v) !is.na(psm_col(v))))
          }
          psm_treat_mask <- psm_cov_mask & !is.na(psm_col(config$var))
          
          # Build multi-outcome formula
          fml <- build_formula(
            outcomes        = psm_avail_outcomes,
            treatment_var   = config$var,
            continuous_vars = spec_config$continuous_vars,
            exact_vars      = spec_config$exact_vars,
            model_type      = current_model_name
          )
          
          # Run multi-outcome feols
          psm_warnings <- character(0)
          psm_error_msg <- NA_character_
          psm_multi <- tryCatch({
            withCallingHandlers(
              { feols(fml, data = psm_data_ref, subset = psm_rc_subset, cluster = ~local_authority, lean = TRUE) },
              warning = function(w) {
                psm_warnings <<- c(psm_warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
              }
            )
          }, error = function(e) {
            psm_error_msg <<- conditionMessage(e)
            message(sprintf("  WARN [PSM multi %s|%s|%s|%s]: %s",
                            current_model_name, spec_name, config$var, regression_core,
                            conditionMessage(e)))
            NULL
          })
          
          # Normalise: fixest_multi → named list keyed by outcome variable name.
          if (!is.null(psm_multi) && inherits(psm_multi, "fixest_multi")) {
            psm_multi <- as.list(psm_multi)
            names(psm_multi) <- sub("^lhs: ", "", names(psm_multi))
          } else if (!is.null(psm_multi) && inherits(psm_multi, "fixest")) {
            psm_multi <- setNames(list(psm_multi), psm_avail_outcomes[1L])
          }

          # Extract results
          psm_rows <- vector("list", length(psm_avail_outcomes))
          local_psm_errors <- 0L

          for (i in seq_along(psm_avail_outcomes)) {
            current_outcome <- psm_avail_outcomes[i]
            out_sd   <- psm_sd_cache[current_outcome]
            out_mean <- psm_mean_cache[current_outcome]
            hyp_tag  <- get_hypothesis_tag(config$short_id, current_outcome)

            # Per-outcome counts
            out_cc <- psm_treat_mask & !is.na(psm_col(current_outcome))
            tv <- psm_col(config$var)[out_cc]
            psm_treated_n <- as.integer(sum(tv == 1L, na.rm = TRUE))
            psm_control_n <- as.integer(sum(tv == 0L, na.rm = TRUE))

            # Resolve model: named lookup in multi-result → single-outcome fallback
            psm_model_for_outcome <- NULL
            psm_fallback_used <- FALSE
            if (is.null(psm_multi)) {
              # Entire multi-outcome call failed — will use error path below
            } else if (current_outcome %in% names(psm_multi)) {
              psm_model_for_outcome <- psm_multi[[current_outcome]]
            } else {
              # Outcome dropped by fixest at scale — single-outcome fallback
              psm_fallback_used <- TRUE
              single_fml <- build_formula(
                current_outcome, config$var, spec_config$continuous_vars,
                spec_config$exact_vars, current_model_name
              )
              fb_warnings <- character(0)
              psm_model_for_outcome <- tryCatch(
                withCallingHandlers(
                  feols(single_fml, data = psm_data_ref, subset = psm_rc_subset,
                        cluster = ~local_authority, lean = TRUE),
                  warning = function(w) { fb_warnings <<- c(fb_warnings, conditionMessage(w)); invokeRestart("muffleWarning") }
                ),
                error = function(e) {
                  message(sprintf("  WARN [PSM fallback %s|%s|%s|%s|%s]: %s",
                                  current_model_name, spec_name, config$var,
                                  regression_core, current_outcome, conditionMessage(e)))
                  NULL
                }
              )
              if (length(fb_warnings) > 0L)
                psm_warnings <- c(psm_warnings, fb_warnings)
            }

            if (is.null(psm_multi) && !psm_fallback_used) {
              local_psm_errors <- local_psm_errors + 1L
              fwrite(data.table(run_id = run_id, model = current_model_name, spec = spec_name,
                                matching_core = matching_core, regression_core = regression_core,
                                treatment = config$short_id, outcome = current_outcome,
                                status = "error",
                                error_message = if (is.na(psm_error_msg)) "multi-outcome feols failed" else psm_error_msg),
                     error_log_path, append = TRUE)
              psm_rows[[i]] <- make_error_row(
                current_outcome, config, current_model_name, spec_name,
                matching_core, regression_core, out_sd, out_mean, hyp_tag,
                psm_treated_n, psm_control_n, length(psm_warnings),
                if (is.na(psm_error_msg)) "multi-outcome feols failed" else psm_error_msg,
                psm_n_eligible, psm_n_matched, psm_mr, psm_mr_post_cal, psm_cal_attr,
                psm_n_matched_post_cal = psm_n_matched_post_cal,
                outcome_n_valid = as.integer(sum(out_cc))
              )
            } else if (!is.null(psm_model_for_outcome)) {
              psm_rows[[i]] <- tryCatch({
                eom <- extract_one_model(
                  model = psm_model_for_outcome, config = config,
                  current_outcome = current_outcome,
                  current_model_name = current_model_name,
                  spec_name = spec_name, matching_core_val = matching_core,
                  regression_core = regression_core,
                  out_sd = out_sd, out_mean = out_mean, hyp_tag = hyp_tag,
                  treated_n = psm_treated_n, control_n = psm_control_n,
                  n_warnings = length(psm_warnings),
                  psm_n_eligible       = psm_n_eligible,
                  psm_n_matched        = psm_n_matched,
                  psm_mr               = psm_mr,
                  psm_mr_post_cal      = psm_mr_post_cal,
                  psm_cal_attr         = psm_cal_attr,
                  psm_n_matched_post_cal = psm_n_matched_post_cal,
                  outcome_n_valid      = as.integer(sum(out_cc))
                )
                if (!is.null(eom$archive)) archive_buffer[[length(archive_buffer) + 1L]] <- eom$archive
                eom$result
              }, error = function(e) {
                local_psm_errors <<- local_psm_errors + 1L
                fwrite(data.table(run_id = run_id, model = current_model_name, spec = spec_name,
                                  matching_core = matching_core, regression_core = regression_core,
                                  treatment = config$short_id, outcome = current_outcome,
                                  status = "error", error_message = conditionMessage(e)),
                       error_log_path, append = TRUE)
                make_error_row(
                  current_outcome, config, current_model_name, spec_name,
                  matching_core, regression_core, out_sd, out_mean, hyp_tag,
                  psm_treated_n, psm_control_n, length(psm_warnings),
                  conditionMessage(e),
                  psm_n_eligible, psm_n_matched, psm_mr, psm_mr_post_cal, psm_cal_attr,
                  psm_n_matched_post_cal = psm_n_matched_post_cal,
                  outcome_n_valid = as.integer(sum(out_cc))
                )
              })
            } else {
              # Both multi and single-outcome feols failed — permanent failure
              psm_rows[[i]] <- make_error_row(
                current_outcome, config, current_model_name, spec_name,
                matching_core, regression_core, out_sd, out_mean, hyp_tag,
                psm_treated_n, psm_control_n, length(psm_warnings),
                "outcome unavailable (multi + single-outcome feols both failed)",
                psm_n_eligible, psm_n_matched, psm_mr, psm_mr_post_cal, psm_cal_attr,
                psm_n_matched_post_cal = psm_n_matched_post_cal,
                outcome_n_valid = as.integer(sum(out_cc))
              )
              psm_rows[[i]][, status := "treatment_dropped"]
            }
            # Release model objects
            if (!is.null(psm_multi) && current_outcome %in% names(psm_multi))
              psm_multi[[current_outcome]] <- NULL
            if (exists("psm_model_for_outcome", inherits = FALSE)) rm(psm_model_for_outcome)
          }  # end outcome extraction
          
          # Write immediately (per-treatment per-rc)
          if (length(psm_rows) > 0L) {
            psm_dt <- rbindlist(psm_rows)
            if (nrow(psm_dt) > 0L) {
              fwrite(psm_dt, output_csv_path, append = TRUE)
              total_rows_written <- total_rows_written + nrow(psm_dt)
              add_treat(treat_key)
              message(sprintf("  [PSM] Wrote %d rows (total: %d) | %s | %s | %s->%s | %s",
                              nrow(psm_dt), total_rows_written,
                              current_model_name, spec_name, matching_core, regression_core,
                              config$short_id))
            }
            rm(psm_dt)
          }
          
          if (local_psm_errors > 0L) psm_error_counter <- psm_error_counter + local_psm_errors
          
          rm(psm_multi, psm_rows, psm_cov_mask, psm_treat_mask,
             psm_sd_cache, psm_mean_cache, psm_rc_subset, psm_col)
          gc()
        }  # end regression_core loop
        
        rm(matched_joined)
        
        # Safety-net gc: drain residual finalizers periodically
        if (psm_models_counter %% 50L == 0L) gc()
        
      }  # end config (treatment) loop
      
      # Flush PSM archive once per (spec, matching_core) batch — mirrors OLS pattern
      if (length(archive_buffer) > 0L) {
        archive_dt    <- rbindlist(archive_buffer)
        archive_fname <- paste0("archive_",
                                gsub("[^a-z0-9]+", "_", tolower(current_model_name)), "_",
                                spec_config$short_id, "_",
                                matching_core, "_",
                                MATCHING_GEOGRAPHY, ".parquet")
        archive_path  <- file.path(MODEL_ARCHIVE_DIR, archive_fname)
        # Append-safe: combine with existing rows from a prior crash-resume
        if (file.exists(archive_path)) {
          existing_archive <- tryCatch(as.data.table(arrow::read_parquet(archive_path)),
                                       error = function(e) NULL)
          if (!is.null(existing_archive) && nrow(existing_archive) > 0L) {
            existing_keys <- paste(existing_archive$outcome, existing_archive$treatment_short_id,
                                   existing_archive$regression_core, sep = "|")
            new_keys      <- paste(archive_dt$outcome, archive_dt$treatment_short_id,
                                   archive_dt$regression_core, sep = "|")
            archive_dt    <- rbindlist(list(existing_archive, archive_dt[!new_keys %in% existing_keys]),
                                       use.names = TRUE, fill = TRUE)
          }
          rm(existing_archive); gc()
        }
        # Write to temp then rename to avoid Windows memory-mapped file lock
        archive_tmp <- paste0(archive_path, ".tmp")
        arrow::write_parquet(archive_dt, archive_tmp)
        file.rename(archive_tmp, archive_path)
        message(sprintf("  Archive flushed: %s (%d rows)", archive_fname, nrow(archive_dt)))
        rm(archive_dt)
      }
      archive_buffer <- list()
      
    }  # end matching_core loop
  }  # end spec_config loop
  
  if (length(archive_buffer) > 0L) {
    message(sprintf("  WARNING: %d orphaned archive rows at end of %s (should be 0)",
                    length(archive_buffer), current_model_name))
    archive_buffer <- list()
  }
  
  message(sprintf("  Completed %s: %d total PSM models run.", current_model_name, psm_models_counter))
}


# Final Summary -----------------------------------------------------------
end.time   <- Sys.time()
time.taken <- end.time - start.time

message("\n===================================================================")
message("  Script Complete")
message("===================================================================")
message(sprintf("  Matching geography: %s", MATCHING_GEOGRAPHY))
message(sprintf("  OLS errors:        %d", ols_error_counter))
message(sprintf("  PSM errors:        %d", psm_error_counter))
message(sprintf("  Total rows in CSV: %d", total_rows_written))
message(sprintf("  Results saved to:  %s", output_csv_path))
message(sprintf("  Runtime: %.2f %s", as.numeric(time.taken), units(time.taken)))
message("===================================================================")