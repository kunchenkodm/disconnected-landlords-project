# Script: 06_run_regressions.R
# Purpose: Run per-LA disaggregated OLS and PSM regressions, then combine
#          coefficients using observation-count weighting. Per-LA regressions
#          with hetero-robust SEs are equivalent to the fully-interacted
#          (property FE + year FE) * LA FE specification.
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 15, 2025. Last Updated: Febuary 20, 2026.
rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Setup & Dependencies ----------------------------------------------------

start.time <- Sys.time()

library(data.table)
library(fixest)
library(arrow)
library(here)

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))
message("Sourced setup, specifications, and treatment metadata.")

ccod_version <- CCOD_VERSION
la_refined_dir <- EPC_LA_REFINED_DIR
matched_data_dir <- MATCHED_DATA_DIR
output_dir <- RESULTS_DIR
summary_dir <- SUMMARY_TABLES_DIR

# ADAPTERS
analysis_configs <- treatment_metadata
regression_core_filters <- matching_core_filters

# Define which regression cores are valid given the matching core
valid_core_pairs <- list(
  base          = c("base", "council_tax", "ppd", "ppd_counciltax"),
  council_tax   = c("council_tax", "ppd_counciltax"),
  ppd           = c("ppd", "ppd_counciltax"),
  ppd_counciltax = c("ppd_counciltax")
)


## Define Outcome Variables -----------------------------------------------
outcome_variables <- c(
  "bad_epc", "energy_consumption_current", "energy_consumption_current_property",
  "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh", "energy_consumption_gap",
  "energy_consumption_gap_property", "energy_efficiency_potential_gap",
  "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap",
  "borderline_good_epc", "borderline_better_epc"
)


# Helper Functions --------------------------------------------------------

# Build formula for per-LA regression (local_authority excluded from FE).
# For both "OLS Additive FE" and "OLS Interactive FE", the per-LA formula
# is identical (additive FE without LA).
build_formula_per_la <- function(outcome, treatment_var, continuous_vars, exact_vars) {
  continuous_vars <- continuous_vars[continuous_vars != ""]
  exact_vars <- setdiff(exact_vars[exact_vars != ""], "local_authority")
  rhs <- paste(c(treatment_var, continuous_vars), collapse = " + ")
  if (length(exact_vars) > 0) {
    fe <- paste(exact_vars, collapse = " + ")
    as.formula(paste(outcome, "~", rhs, "|", fe))
  } else {
    as.formula(paste(outcome, "~", rhs))
  }
}

# Build formula for PSM subclass FE models
build_formula_psm_subclass <- function(outcome, treatment_var, continuous_vars) {
  continuous_vars <- continuous_vars[continuous_vars != ""]
  rhs <- paste(c(treatment_var, continuous_vars), collapse = " + ")
  as.formula(paste(outcome, "~", rhs, "| subclass"))
}

# Retrieve a core filter expression
get_core_filter <- function(core_name) {
  if (core_name %in% names(regression_core_filters)) {
    return(regression_core_filters[[core_name]])
  }
  stop("Unknown core: ", core_name)
}

# Count treated / control among complete cases
get_simple_counts <- function(data, treat_var, outcome, continuous_vars, exact_vars) {
  all_vars <- unique(c(outcome, treat_var, continuous_vars, exact_vars))
  all_vars <- all_vars[all_vars != "" & all_vars != "local_authority"]
  # Only include vars that exist in the data
  all_vars <- intersect(all_vars, names(data))
  cc <- complete.cases(data[, ..all_vars])
  valid_data <- data[cc]
  list(
    treated = as.integer(sum(valid_data[[treat_var]] == 1, na.rm = TRUE)),
    control = as.integer(sum(valid_data[[treat_var]] == 0, na.rm = TRUE))
  )
}

# Load a per-LA parquet, apply treatments and derived columns
load_la_parquet <- function(path) {
  dt <- as.data.table(arrow::read_parquet(path))
  dt <- define_treatments(dt)
  dt[, energy_cons_curr_per_floor_area :=
       fifelse(total_floor_area == 0, NA_real_,
               energy_consumption_current / total_floor_area)]
  dt
}


# Accumulator functions for per-LA regression combining --------------------
init_accumulator <- function() {
  list(sum_n_beta = 0, sum_n = 0, sum_n2_se2 = 0,
       sum_n_mean = 0, sum_treated = 0L, sum_control = 0L,
       la_count = 0L)
}

accumulate_result <- function(acc, n_la, beta_la, se_la, mean_la, treated_la, control_la) {
  acc$sum_n_beta  <- acc$sum_n_beta  + n_la * beta_la
  acc$sum_n       <- acc$sum_n       + n_la
  acc$sum_n2_se2  <- acc$sum_n2_se2  + (n_la^2) * (se_la^2)
  acc$sum_n_mean  <- acc$sum_n_mean  + n_la * mean_la
  acc$sum_treated <- acc$sum_treated + treated_la
  acc$sum_control <- acc$sum_control + control_la
  acc$la_count    <- acc$la_count    + 1L
  acc
}

combine_accumulator <- function(acc, model_name, spec_name, regression_core,
                                treatment_title, outcome_name,
                                matching_core = NA_character_) {
  if (acc$sum_n == 0) return(NULL)
  data.table(
    coef          = acc$sum_n_beta / acc$sum_n,
    se            = sqrt(acc$sum_n2_se2) / acc$sum_n,
    nobs          = as.integer(acc$sum_n),
    r2            = NA_real_,
    outcome       = outcome_name,
    treatment     = treatment_title,
    model         = model_name,
    spec          = spec_name,
    matching_core = matching_core,
    regression_core = regression_core,
    outcome_mean  = acc$sum_n_mean / acc$sum_n,
    treated_n     = acc$sum_treated,
    control_n     = acc$sum_control
  )
}


# Discover per-LA parquet files and build LA-code -> file mapping ----------
la_files <- list.files(la_refined_dir, pattern = "\\.parquet$", full.names = TRUE)
n_la_files <- length(la_files)

if (n_la_files == 0L) {
  stop("No per-LA Parquet files found in: ", la_refined_dir)
}
message(sprintf("Found %d per-LA Parquet files.", n_la_files))

# Build mapping: LA code -> parquet file path (needed for PSM later)
message("Building LA-code-to-parquet mapping...")
la_to_parquet <- list()
for (pf in la_files) {
  sample_dt <- as.data.table(arrow::read_parquet(pf, col_select = "local_authority"))
  if (nrow(sample_dt) > 0) {
    la_code <- as.character(sample_dt$local_authority[1])
    la_to_parquet[[la_code]] <- pf
  }
  rm(sample_dt)
}
message(sprintf("Mapped %d LA codes to parquet files.", length(la_to_parquet)))


# =========================================================================
# OLS MODELS: Per-LA disaggregated regressions
# =========================================================================
message("\n=== Running OLS models (per-LA disaggregated) ===")

# Build all valid (spec, regression_core) combos
ols_combos <- list()
for (spec_config in spec_configs) {
  allowed_cores <- spec_core_pairs[[spec_config$name]]
  for (rc in names(regression_core_filters)) {
    if (rc %in% allowed_cores) {
      ols_combos[[length(ols_combos) + 1]] <- list(spec = spec_config, core = rc)
    }
  }
}

# Pre-initialise all accumulators
ols_accumulators <- list()
for (combo in ols_combos) {
  for (cfg in analysis_configs) {
    for (outc in outcome_variables) {
      key <- paste(combo$spec$name, combo$core, cfg$var, outc, sep = "|")
      ols_accumulators[[key]] <- init_accumulator()
    }
  }
}

models_counter <- 0L
last_update_time <- Sys.time()
update_interval <- 10

# ---- LA-outer loop: load each parquet once, run all OLS combos ----------
for (i in seq_along(la_files)) {
  if (i == 1L || i %% 10L == 0L) {
    elapsed <- as.numeric(difftime(Sys.time(), start.time, units = "secs"))
    message(sprintf("OLS [%d/%d] Loading LA parquet (%.0f s elapsed)",
                    i, n_la_files, elapsed))
  }

  dt <- load_la_parquet(la_files[i])

  for (combo in ols_combos) {
    spec_config    <- combo$spec
    regression_core <- combo$core
    core_filter    <- get_core_filter(regression_core)
    dt_filtered    <- dt[eval(core_filter)]
    if (nrow(dt_filtered) == 0) next

    for (cfg in analysis_configs) {
      for (current_outcome in outcome_variables) {
        models_counter <- models_counter + 1L

        current_time <- Sys.time()
        if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
          message(sprintf("  OLS model %d | %.1f sec | %s | %s",
                          models_counter,
                          as.numeric(difftime(current_time, start.time, units = "secs")),
                          cfg$var, current_outcome))
          last_update_time <- current_time
        }

        fml <- build_formula_per_la(current_outcome, cfg$var,
                                     spec_config$continuous_vars,
                                     spec_config$exact_vars)

        tryCatch({
          counts <- get_simple_counts(dt_filtered, cfg$var, current_outcome,
                                       spec_config$continuous_vars,
                                       spec_config$exact_vars)
          model <- feols(fml, data = dt_filtered, vcov = "hetero")
          ct    <- coeftable(model)

          if (cfg$var %in% rownames(ct)) {
            key <- paste(spec_config$name, regression_core,
                         cfg$var, current_outcome, sep = "|")
            ols_accumulators[[key]] <- accumulate_result(
              ols_accumulators[[key]],
              n_la      = nobs(model),
              beta_la   = ct[cfg$var, "Estimate"],
              se_la     = ct[cfg$var, "Std. Error"],
              mean_la   = mean(dt_filtered[[current_outcome]], na.rm = TRUE),
              treated_la = counts$treated,
              control_la = counts$control
            )
          }
        }, error = function(e) { })
      }
    }
  }
  rm(dt); gc()
}

# ---- Combine OLS accumulators into result rows --------------------------
# Both OLS model labels get identical values (per-LA ≡ interactive FE)
ols_results <- list()
for (key in names(ols_accumulators)) {
  acc <- ols_accumulators[[key]]
  if (acc$sum_n == 0) next

  parts <- strsplit(key, "\\|")[[1]]
  spec_name       <- parts[1]
  regression_core <- parts[2]
  treat_var       <- parts[3]
  outcome_name    <- parts[4]

  treatment_title <- NA_character_
  for (cfg in analysis_configs) {
    if (cfg$var == treat_var) { treatment_title <- cfg$title; break }
  }

  for (model_label in c("OLS Additive FE", "OLS Interactive FE")) {
    row <- combine_accumulator(acc, model_label, spec_name, regression_core,
                               treatment_title, outcome_name)
    if (!is.null(row)) ols_results[[length(ols_results) + 1]] <- row
  }
}

message(sprintf("OLS complete: %d result rows from %d per-LA model runs.",
                length(ols_results), models_counter))


# =========================================================================
# PSM MODELS: Per-LA disaggregated regressions
# =========================================================================
message("\n=== Running PSM models (per-LA disaggregated) ===")

psm_model_types <- c(
  "PSM (Matched)", "PSM (Matched) + Subclass FE",
  "PSM (Matched) PS<=0.2", "PSM (Matched) PS<=0.2 + Subclass FE",
  "PSM (Matched) PS<=0.1", "PSM (Matched) PS<=0.1 + Subclass FE"
)

psm_results <- list()
psm_counter <- 0L

for (current_model_name in psm_model_types) {
  message(sprintf("\n--- Processing %s models ---", current_model_name))
  psm_accumulators <- list()

  for (spec_config in spec_configs) {
    spec_name     <- spec_config$name
    allowed_cores <- spec_core_pairs[[spec_name]]

    for (matching_core in names(matching_core_filters)) {
      valid_regression_cores <- valid_core_pairs[[matching_core]]
      model_cores <- intersect(allowed_cores, valid_regression_cores)

      for (regression_core in model_cores) {
        for (cfg in analysis_configs) {

          # Load combined matched-pairs file
          matched_file_path <- file.path(
            matched_data_dir,
            paste0("matched_pairs_", cfg$file_id,
                   "_matching_core_", matching_core,
                   "_", ccod_version, ".RData")
          )
          if (!file.exists(matched_file_path)) next

          load(matched_file_path)           # → matched_results, matching_core_metadata
          matched_dt <- matched_results[[spec_name]]
          rm(matched_results, matching_core_metadata)

          if (is.null(matched_dt) || nrow(matched_dt) == 0) next

          # Caliper filter
          if (grepl("PS<=0.1", current_model_name, fixed = TRUE)) {
            if (!"distance" %in% names(matched_dt)) next
            matched_dt <- matched_dt[distance <= 0.1]
          } else if (grepl("PS<=0.2", current_model_name, fixed = TRUE)) {
            if (!"distance" %in% names(matched_dt)) next
            matched_dt <- matched_dt[distance <= 0.2]
          }
          if (nrow(matched_dt) == 0) next

          # Split by LA and process each
          matched_dt[, la_char := as.character(local_authority)]
          la_groups <- split(matched_dt, by = "la_char")

          for (la_label in names(la_groups)) {
            la_matched <- la_groups[[la_label]]

            # Look up the parquet file for this LA
            la_pf <- la_to_parquet[[la_label]]
            if (is.null(la_pf) || !file.exists(la_pf)) next

            # Expand matched data with full covariates
            full_la <- load_la_parquet(la_pf)
            setkey(full_la, uprn)
            expanded <- full_la[la_matched[!is.na(uprn)], on = "uprn", nomatch = 0]
            rm(full_la)
            if (nrow(expanded) == 0) next

            # Regression core filter (if different from matching core)
            if (matching_core != regression_core) {
              expanded <- expanded[eval(get_core_filter(regression_core))]
              if (nrow(expanded) == 0) next
            }

            use_subclass <- grepl("Subclass FE", current_model_name, fixed = TRUE)

            for (current_outcome in outcome_variables) {
              psm_counter <- psm_counter + 1L
              if (psm_counter %% 500L == 0L) {
                message(sprintf("  PSM model %d | %.1f sec",
                                psm_counter,
                                as.numeric(difftime(Sys.time(), start.time, units = "secs"))))
              }

              fml <- if (use_subclass) {
                build_formula_psm_subclass(current_outcome, cfg$var,
                                           spec_config$continuous_vars)
              } else {
                build_formula_per_la(current_outcome, cfg$var,
                                     spec_config$continuous_vars,
                                     spec_config$exact_vars)
              }

              tryCatch({
                ev <- if (use_subclass) "subclass"
                      else setdiff(spec_config$exact_vars, "local_authority")
                counts <- get_simple_counts(expanded, cfg$var, current_outcome,
                                             spec_config$continuous_vars, ev)

                model <- feols(fml, data = expanded, vcov = "hetero")
                ct    <- coeftable(model)

                if (cfg$var %in% rownames(ct)) {
                  acc_key <- paste(current_model_name, spec_name,
                                   matching_core, regression_core,
                                   cfg$var, current_outcome, sep = "|")
                  if (is.null(psm_accumulators[[acc_key]])) {
                    psm_accumulators[[acc_key]] <- init_accumulator()
                  }
                  psm_accumulators[[acc_key]] <- accumulate_result(
                    psm_accumulators[[acc_key]],
                    n_la       = nobs(model),
                    beta_la    = ct[cfg$var, "Estimate"],
                    se_la      = ct[cfg$var, "Std. Error"],
                    mean_la    = mean(expanded[[current_outcome]], na.rm = TRUE),
                    treated_la = counts$treated,
                    control_la = counts$control
                  )
                }
              }, error = function(e) { })
            }
            rm(expanded)
          }

          rm(matched_dt, la_groups); gc()
        }
      }
    }
  }

  # Combine accumulators for this model type
  for (acc_key in names(psm_accumulators)) {
    acc <- psm_accumulators[[acc_key]]
    if (acc$sum_n == 0) next

    parts <- strsplit(acc_key, "\\|")[[1]]
    # parts: model_name | spec | matching_core | regression_core | treat_var | outcome
    treatment_title <- NA_character_
    for (cfg in analysis_configs) {
      if (cfg$var == parts[5]) { treatment_title <- cfg$title; break }
    }
    row <- combine_accumulator(acc, model_name = parts[1], spec_name = parts[2],
                               regression_core = parts[4],
                               treatment_title = treatment_title,
                               outcome_name = parts[6],
                               matching_core = parts[3])
    if (!is.null(row)) psm_results[[length(psm_results) + 1]] <- row
  }
}

message(sprintf("PSM complete: %d result rows from %d per-LA model runs.",
                length(psm_results), psm_counter))


# Combine all results and save to CSV -------------------------------------
message("\nCombining results...")
all_results <- c(ols_results, psm_results)
if (length(all_results) > 0) {
  results_table <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
} else {
  results_table <- data.table()
  warning("No regression results produced.")
}
output_csv_path <- file.path(summary_dir, "results_table.csv")
fwrite(results_table, output_csv_path)
message("Saved all results to: ", output_csv_path)

end.time <- Sys.time()
time.taken <- end.time - start.time
message(sprintf("\n=== Script Complete ==="))
message(sprintf("OLS per-LA models: %d | PSM per-LA models: %d", models_counter, psm_counter))
message(sprintf("Result rows: %d", nrow(results_table)))
message(sprintf("Runtime: %.2f %s", as.numeric(time.taken), units(time.taken)))
