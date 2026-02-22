# Script: 06_run_regressions_full.R
# Purpose: Load all per-LA Parquet files into a single in-memory dataset and
#          run the full-sample OLS + PSM regressions with cluster-robust SEs.
#          This preserves the original Additive vs Interactive FE distinction.
#          WARNING: requires enough RAM to hold the full refined datase t (~8-12 GB).
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


# Load Full Dataset from Per-LA Parquet Files ------------------------------
message("Loading all per-LA Parquet files into memory...")

la_files <- list.files(la_refined_dir, pattern = "\\.parquet$", full.names = TRUE)
if (length(la_files) == 0L) {
  stop("No per-LA Parquet files found in: ", la_refined_dir)
}
message(sprintf("Found %d per-LA Parquet files. Reading...", length(la_files)))

load_start <- proc.time()
la_list <- vector("list", length(la_files))
for (i in seq_along(la_files)) {
  la_list[[i]] <- as.data.table(arrow::read_parquet(la_files[i]))
  if (i %% 50L == 0L) {
    message(sprintf("  Read %d / %d files", i, length(la_files)))
  }
}

EPC_matched_combined <- rbindlist(la_list, use.names = TRUE, fill = TRUE)
rm(la_list); gc()

load_elapsed <- (proc.time() - load_start)[["elapsed"]]
message(sprintf("Loaded full dataset: %d rows, %d columns (%.0f s)",
                nrow(EPC_matched_combined), ncol(EPC_matched_combined), load_elapsed))

# Re-apply treatment definitions
EPC_matched_combined <- define_treatments(EPC_matched_combined)
EPC_matched_combined[, energy_cons_curr_per_floor_area :=
                       fifelse(total_floor_area == 0, NA_real_,
                               energy_consumption_current / total_floor_area)]
setkey(EPC_matched_combined, uprn)


## Define Outcome Variables -----------------------------------------------
outcome_variables <- c(
  "bad_epc", "energy_consumption_current", "energy_consumption_current_property",
  "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh", "energy_consumption_gap",
  "energy_consumption_gap_property", "energy_efficiency_potential_gap",
  "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap",
  "borderline_good_epc", "borderline_better_epc"
)


# Helper Functions --------------------------------------------------------

expand_matched_data <- function(matched_list, full_data) {
  lapply(matched_list, function(matched_dt) {
    if (is.null(matched_dt) || nrow(matched_dt) == 0) return(NULL)
    full_data[matched_dt[!is.na(uprn)], on = "uprn", nomatch = 0]
  })
}

build_formula <- function(outcome, treatment_var, continuous_vars, exact_vars, model_type) {
  continuous_vars <- continuous_vars[continuous_vars != ""]
  exact_vars <- exact_vars[exact_vars != ""]
  rhs <- paste(c(treatment_var, continuous_vars), collapse = " + ")
  fe <- paste(exact_vars, collapse = " + ")
  fe_interact <- paste(exact_vars, collapse = "^")
  if (model_type == "OLS Additive FE") {
    if (fe != "")
      return(as.formula(paste(outcome, "~", rhs, "|", fe)))
    else
      return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type == "OLS Interactive FE") {
    if (fe_interact != "")
      return(as.formula(paste(outcome, "~", rhs, "|", fe_interact)))
    else
      return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type %in% c("PSM (Matched)", "PSM (Matched) PS<=0.2", "PSM (Matched) PS<=0.1")) {
    return(as.formula(paste(outcome, "~", rhs, "|", fe)))
  } else if (model_type %in% c("PSM (Matched) + Subclass FE",
                                "PSM (Matched) PS<=0.2 + Subclass FE",
                                "PSM (Matched) PS<=0.1 + Subclass FE")) {
    return(as.formula(paste(outcome, "~", rhs, "| subclass")))
  }
  stop("Unknown model type: ", model_type)
}

get_matching_core_filter <- function(matching_core) {
  if (matching_core %in% names(matching_core_filters)) {
    return(matching_core_filters[[matching_core]])
  }
  stop("Unknown matching core: ", matching_core)
}

get_regression_core_filter <- function(regression_core) {
  if (regression_core %in% names(regression_core_filters)) {
    return(regression_core_filters[[regression_core]])
  }
  stop("Unknown regression core: ", regression_core)
}

get_simple_counts <- function(data, treat_var, outcome, continuous_vars, exact_vars) {
  all_vars <- unique(c(outcome, treat_var, continuous_vars, exact_vars))
  all_vars <- all_vars[all_vars != ""]
  all_vars <- intersect(all_vars, names(data))
  cc <- complete.cases(data[, ..all_vars])
  valid_data <- data[cc]
  list(
    treated = as.integer(sum(valid_data[[treat_var]] == 1, na.rm = TRUE)),
    control = as.integer(sum(valid_data[[treat_var]] == 0, na.rm = TRUE))
  )
}


# OLS Models ---------------------------------------------------------------
run_ols_models_single_core <- function(current_model_name) {
  ols_results <- vector("list")
  local_result_counter <- 0

  for (spec_config in spec_configs) {
    spec_name <- spec_config$name
    allowed_cores <- spec_core_pairs[[spec_name]]

    for (regression_core in names(regression_core_filters)) {
      if (!(regression_core %in% allowed_cores)) next
      regression_core_filter <- get_regression_core_filter(regression_core)
      ols_data_filtered <- EPC_matched_combined[eval(regression_core_filter)]

      if (nrow(ols_data_filtered) == 0) {
        message(sprintf(" Skipping regression core '%s' - no data after filtering", regression_core))
        next
      }

      for (config in analysis_configs) {
        for (current_outcome in outcome_variables) {
          models_counter <<- models_counter + 1
          current_time <- Sys.time()
          if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
            elapsed_time <- as.numeric(difftime(current_time, start.time, units = "secs"))
            message(sprintf(
              "Model %d | Time since start: %.1f sec | Current model: %s",
              models_counter, elapsed_time, current_outcome
            ))
            last_update_time <<- current_time
          }

          fml <- build_formula(
            outcome = current_outcome,
            treatment_var = config$var,
            continuous_vars = spec_config$continuous_vars,
            exact_vars = spec_config$exact_vars,
            model_type = current_model_name
          )

          result <- tryCatch({
            counts <- get_simple_counts(
              ols_data_filtered, config$var, current_outcome,
              spec_config$continuous_vars, spec_config$exact_vars
            )

            model <- feols(fml, data = ols_data_filtered, cluster = ~local_authority)
            ct <- coeftable(model)
            if (!(config$var %in% rownames(ct))) {
              NULL
            } else {
              row <- ct[config$var, ]
              outcome_mean <- mean(ols_data_filtered[[current_outcome]], na.rm = TRUE)

              data.table(
                coef = row["Estimate"],
                se = row["Std. Error"],
                nobs = nobs(model),
                r2 = r2(model, "r2"),
                outcome = current_outcome,
                treatment = config$title,
                model = current_model_name,
                spec = spec_name,
                matching_core = NA_character_,
                regression_core = regression_core,
                outcome_mean = outcome_mean,
                treated_n = counts$treated,
                control_n = counts$control
              )
            }
          }, error = function(e) {
            warning(sprintf("Error in OLS: %s", e$message), call. = FALSE)
            NULL
          })
          if (!is.null(result)) {
            local_result_counter <- local_result_counter + 1
            ols_results[[local_result_counter]] <- result
          }
        }
      }
    }
  }

  if (local_result_counter > 0) {
    return(ols_results[1:local_result_counter])
  } else {
    return(list())
  }
}


# PSM Models ---------------------------------------------------------------
load_and_filter_matched_data <- function(config, matching_core, regression_core, spec_config) {
  matched_file_path <- file.path(
    matched_data_dir,
    paste0("matched_pairs_", config$file_id,
           "_matching_core_", matching_core,
           "_", ccod_version, ".RData")
  )

  if (!file.exists(matched_file_path)) return(NULL)

  load(matched_file_path)
  matched_expanded <- expand_matched_data(matched_results, EPC_matched_combined)
  matched_data_for_spec <- matched_expanded[[spec_config$name]]

  rm(matched_results, matched_expanded)

  if (is.null(matched_data_for_spec) || nrow(matched_data_for_spec) == 0) return(NULL)

  # Apply regression core filter if different from matching core
  if (matching_core != regression_core) {
    regression_core_filter <- get_regression_core_filter(regression_core)
    matched_data_for_spec <- matched_data_for_spec[eval(regression_core_filter)]
  }

  return(matched_data_for_spec)
}

run_psm_models_full_combinations <- function(current_model_name) {
  psm_results <- vector("list")
  local_result_counter <- 0

  for (spec_config in spec_configs) {
    spec_name <- spec_config$name
    allowed_cores <- spec_core_pairs[[spec_name]]

    for (matching_core in names(matching_core_filters)) {
      valid_regression_cores <- valid_core_pairs[[matching_core]]
      model_cores <- intersect(allowed_cores, valid_regression_cores)

      for (regression_core in model_cores) {
        for (config in analysis_configs) {
          matched_data_for_spec <- load_and_filter_matched_data(
            config, matching_core, regression_core, spec_config
          )

          if (is.null(matched_data_for_spec) || nrow(matched_data_for_spec) == 0) next

          # Apply propensity score restriction if needed
          if (grepl("PS<=0.1", current_model_name, fixed = TRUE)) {
            if (!"distance" %in% names(matched_data_for_spec)) next
            matched_data_for_spec <- matched_data_for_spec[distance <= 0.1]
            if (nrow(matched_data_for_spec) == 0) next
          } else if (grepl("PS<=0.2", current_model_name, fixed = TRUE)) {
            if (!"distance" %in% names(matched_data_for_spec)) next
            matched_data_for_spec <- matched_data_for_spec[distance <= 0.2]
            if (nrow(matched_data_for_spec) == 0) next
          }

          for (current_outcome in outcome_variables) {
            models_counter <<- models_counter + 1
            current_time <- Sys.time()
            if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
              elapsed_time <- as.numeric(difftime(current_time, start.time, units = "secs"))
              message(sprintf(
                "Model %d | Time since start: %.1f sec | Current model: %s",
                models_counter, elapsed_time, current_outcome
              ))
              last_update_time <<- current_time
            }

            fml <- build_formula(
              outcome = current_outcome,
              treatment_var = config$var,
              continuous_vars = spec_config$continuous_vars,
              exact_vars = spec_config$exact_vars,
              model_type = current_model_name
            )

            result <- tryCatch({
              counts <- get_simple_counts(
                matched_data_for_spec, config$var, current_outcome,
                spec_config$continuous_vars, spec_config$exact_vars
              )

              model <- feols(fml, data = matched_data_for_spec, cluster = ~local_authority)
              ct <- coeftable(model)
              if (!(config$var %in% rownames(ct))) {
                NULL
              } else {
                row <- ct[config$var, ]
                outcome_mean <- mean(matched_data_for_spec[[current_outcome]], na.rm = TRUE)

                data.table(
                  coef = row["Estimate"],
                  se = row["Std. Error"],
                  nobs = nobs(model),
                  r2 = r2(model, "r2"),
                  outcome = current_outcome,
                  treatment = config$title,
                  model = current_model_name,
                  spec = spec_name,
                  matching_core = matching_core,
                  regression_core = regression_core,
                  outcome_mean = outcome_mean,
                  treated_n = counts$treated,
                  control_n = counts$control
                )
              }
            }, error = function(e) {
              warning(sprintf("Error in PSM: %s", e$message), call. = FALSE)
              NULL
            })

            if (!is.null(result)) {
              local_result_counter <- local_result_counter + 1
              psm_results[[local_result_counter]] <- result
            }
          }
        }
      }
    }
  }

  if (local_result_counter > 0) {
    return(psm_results[1:local_result_counter])
  } else {
    return(list())
  }
}


# Process Models ----------------------------------------------------------
ols_results_temp <- vector("list")
psm_results <- vector("list")
result_counter <- 0
models_counter <- 0
last_update_time <- Sys.time()
update_interval <- 5

# OLS models
for (current_model_name in c("OLS Additive FE", "OLS Interactive FE")) {
  message(sprintf("\n--- Processing %s models ---", current_model_name))
  ols_results_for_model <- run_ols_models_single_core(current_model_name)
  ols_results_temp <- append(ols_results_temp, ols_results_for_model)
}

# PSM models
for (current_model_name in c("PSM (Matched)", "PSM (Matched) + Subclass FE",
                             "PSM (Matched) PS<=0.2", "PSM (Matched) PS<=0.2 + Subclass FE",
                             "PSM (Matched) PS<=0.1", "PSM (Matched) PS<=0.1 + Subclass FE")) {
  message(sprintf("\n--- Processing %s models ---", current_model_name))
  psm_results_for_model <- run_psm_models_full_combinations(current_model_name)
  psm_results <- append(psm_results, psm_results_for_model)
}

# Combine and save --------------------------------------------------------
message("\nCombining results...")
all_results <- c(ols_results_temp, psm_results)
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
message(sprintf("Models attempted: %d", models_counter))
message(sprintf("Rows written: %d", nrow(results_table)))
message(sprintf("Runtime: %.2f %s (%.1f models/sec)",
                as.numeric(time.taken), units(time.taken),
                models_counter / as.numeric(time.taken, units = "secs")))
