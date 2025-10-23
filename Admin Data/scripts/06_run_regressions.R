# Script: 06_run_regressions.R
# Purpose: Generate CSV summary from regression results.
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 15, 2025. Last Updated: October 8, 2025.

rm(list = ls())
gc()
start.time <- Sys.time()
source(here::here("scripts", "00_setup.R"))
library(data.table)
library(fixest)

ccod_version <- CCOD_VERSION
processed_data_dir <- PROCESSED_DATA_DIR
matched_data_dir <- MATCHED_DATA_DIR
summary_dir <- file.path(getwd(), "output", "summary_tables")
if (!dir.exists(summary_dir)) dir.create(summary_dir, recursive = TRUE)

# Define core filters with hierarchy levels
matching_core_filters <- list(
  base = list(filter = quote(rep(TRUE, .N)), level = 1),
  council_tax = list(filter = quote(!is.na(tax_band)), level = 2),
  ppd = list(filter = quote(!is.na(ppd_price_sqm)), level = 2),
  ppd_counciltax = list(filter = quote(!is.na(tax_band) & !is.na(ppd_price_sqm)), level = 3)
)

regression_core_filters <- list(
  base = list(filter = quote(rep(TRUE, .N)), level = 1),
  council_tax = list(filter = quote(!is.na(tax_band)), level = 2),
  ppd = list(filter = quote(!is.na(ppd_price_sqm)), level = 2),
  ppd_counciltax = list(filter = quote(!is.na(tax_band) & !is.na(ppd_price_sqm)), level = 3)
)

# Function to check if regression core is compatible with matching core
is_valid_core_combination <- function(matching_core, regression_core) {
  matching_level <- matching_core_filters[[matching_core]]$level
  regression_level <- regression_core_filters[[regression_core]]$level
  
  # Only allow regression core if it's at same level or narrower (higher level)
  return(regression_level >= matching_level)
}

# Define specification configurations
spec_configs <- list(
  list(
    name = "Baseline",
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
    exact_vars = c("property_type", "main_fuel", "construction_age_band", "built_form", "local_authority")
  ),
  list(
    name = "Council Tax", 
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
    exact_vars = c("property_type", "main_fuel", "tax_band", "construction_age_band", "built_form", "local_authority")
  ),
  list(
    name = "Council Tax + Price Paid",
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "lodgement_year", "ppd_price_sqm"),
    exact_vars = c("property_type", "main_fuel", "tax_band", "ppd_year_transfer", "construction_age_band", "built_form", "local_authority")
  )
)

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
  if (model_type == "OLS Additive FE") {
    fe <- paste(exact_vars, collapse = " + ")
    if (fe != "")
      return(as.formula(paste(outcome, "~", rhs, "|", fe)))
    else
      return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type == "OLS Interactive FE") {
    fe_interact <- paste(exact_vars, collapse = "^")
    if (fe_interact != "")
      return(as.formula(paste(outcome, "~", rhs, "|", fe_interact)))
    else
      return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type == "PSM (Matched)") {
    return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type == "PSM (Matched) + Subclass FE") {
    return(as.formula(paste(outcome, "~", rhs, "| subclass")))
  }
  stop("Unknown model type: ", model_type)
}

get_matching_core_filter <- function(matching_core) {
  filter_obj <- matching_core_filters[[matching_core]]
  if (is.null(filter_obj)) {
    stop("Unknown matching core: ", matching_core)
  }
  return(filter_obj$filter)
}

get_regression_core_filter <- function(regression_core) {
  filter_obj <- regression_core_filters[[regression_core]]
  if (is.null(filter_obj)) {
    stop("Unknown regression core: ", regression_core)
  }
  return(filter_obj$filter)
}

message("Loading data...")
input_file <- file.path(processed_data_dir, paste0("epc_matched_refined_", CCOD_VERSION, ".RData"))
load(input_file)
source(here::here("scripts", "treatment_definitions.R"))
EPC_matched_combined <- define_treatments(EPC_matched_combined)
EPC_matched_combined[, energy_cons_curr_per_floor_area :=
                       fifelse(total_floor_area == 0, NA_real_, energy_consumption_current / total_floor_area)]
setkey(EPC_matched_combined, uprn)

outcome_variables <- c(
  "bad_epc", "energy_consumption_current", "energy_consumption_current_property",
  "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh", "energy_consumption_gap",
  "energy_consumption_gap_property", "energy_efficiency_potential_gap",
  "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap",
  "borderline_good_epc", "borderline_better_epc"
)

analysis_configs <- list(
  list(var = "treat_for_profit", file_id = "for_profit_vs_private_rental", title = "Effect of For-Profit Ownership"),
  list(var = "treat_uk_for_profit", file_id = "uk_for_profit_vs_private_rental", title = "Effect of UK For-Profit Ownership"),
  list(var = "treat_foreign_for_profit", file_id = "foreign_for_profit_vs_private_rental", title = "Effect of Foreign For-Profit Ownership"),
  list(var = "treat_tax_haven_for_profit", file_id = "tax_haven_for_profit_vs_private_rental", title = "Effect of Tax Haven For-Profit Ownership"),
  list(var = "treat_non_profit", file_id = "non_profit_vs_private_rental", title = "Effect of Non-Profit Ownership"),
  list(var = "treat_uk_non_profit", file_id = "uk_non_profit_vs_private_rental", title = "Effect of UK Non-Profit Ownership"),
  list(var = "treat_foreign_non_profit", file_id = "foreign_non_profit_vs_private_rental", title = "Effect of Foreign Non-Profit Ownership"),
  list(var = "treat_tax_haven_non_profit", file_id = "tax_haven_non_profit_vs_private_rental", title = "Effect of Tax Haven Non-Profit Ownership"),
  list(var = "treat_public_sector", file_id = "public_sector_vs_private_rental", title = "Effect of Public Sector Ownership"),
  list(var = "treat_tax_haven", file_id = "tax_haven_vs_private_rental", title = "Effect of Tax Haven Ownership"),
  list(var = "treat_british_haven", file_id = "british_haven_vs_private_rental", title = "Effect of British Haven Ownership"),
  list(var = "treat_european_haven", file_id = "european_haven_vs_private_rental", title = "Effect of European Haven Ownership"),
  list(var = "treat_caribbean_haven", file_id = "caribbean_haven_vs_private_rental", title = "Effect of Caribbean Haven Ownership"),
  list(var = "treat_other_haven", file_id = "other_haven_vs_private_rental", title = "Effect of Other Haven Ownership")
)
model_types <- c("OLS Additive FE", "OLS Interactive FE", "PSM (Matched)", "PSM (Matched) + Subclass FE")

message("Calculating total number of models...")
matching_core_names <- names(matching_core_filters)
regression_core_names <- names(regression_core_filters)
n_specs <- length(spec_configs)

n_ols_computations <- length(analysis_configs) * length(outcome_variables) * 2 * n_specs * length(regression_core_names)
n_ols_output_rows <- n_ols_computations * length(matching_core_names)

# Count valid PSM combinations (only where regression_level >= matching_level)
n_valid_psm_combinations <- 0
for (matching_core in matching_core_names) {
  for (regression_core in regression_core_names) {
    if (is_valid_core_combination(matching_core, regression_core)) {
      n_valid_psm_combinations <- n_valid_psm_combinations + 1
    }
  }
}

n_psm_computations <- length(analysis_configs) * length(outcome_variables) * 2 * n_specs * n_valid_psm_combinations
total_computations <- n_ols_computations + n_psm_computations
total_output_rows <- n_ols_output_rows + n_psm_computations

message(sprintf(" OLS computations: %d (output rows: %d)", n_ols_computations, n_ols_output_rows))
message(sprintf(" PSM valid combinations: %d (out of %d possible)", n_valid_psm_combinations, 
                length(matching_core_names) * length(regression_core_names)))
message(sprintf(" PSM computations: %d", n_psm_computations))
message(sprintf(" Total computations: %d (output rows: %d)", total_computations, total_output_rows))

# Store results
ols_results_temp <- vector("list")
psm_results <- vector("list")
result_counter <- 0
models_counter <- 0
last_update_time <- Sys.time()
update_interval <- 5

# Helper functions
run_ols_models_single_core <- function(current_model_name) {
  ols_results <- vector("list")
  local_result_counter <- 0
  
  for (spec_config in spec_configs) {
    spec_name <- spec_config$name
    
    for (regression_core in names(regression_core_filters)) {
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
            pct <- round(100 * models_counter / total_computations, 1)
            elapsed <- as.numeric(difftime(current_time, start.time, units = "secs"))
            rate <- models_counter / elapsed
            eta_sec <- (total_computations - models_counter) / rate
            eta_min <- eta_sec / 60
            message(sprintf(" [%d/%d] %.1f%% | %.1f models/sec | ETA: %.1f min | OLS %s",
                            models_counter, total_computations, pct, rate, eta_min, current_outcome))
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
            model <- feols(fml, data = ols_data_filtered, cluster = ~local_authority)
            ct <- coeftable(model)
            if (!(config$var %in% rownames(ct))) {
              NULL
            } else {
              row <- ct[config$var, ]
              
              # Calculate additional statistics
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
  
  return(ols_results[1:local_result_counter])
}

replicate_ols_across_matching_cores <- function(ols_results_list) {
  if (length(ols_results_list) == 0) return(list())
  
  ols_base_results <- rbindlist(ols_results_list)
  matching_core_names <- names(matching_core_filters)
  
  replicated_results <- vector("list", length(matching_core_names))
  
  for (i in seq_along(matching_core_names)) {
    matching_core <- matching_core_names[i]
    replicated_dt <- copy(ols_base_results)
    replicated_dt[, matching_core := matching_core]
    replicated_results[[i]] <- replicated_dt
  }
  
  return(replicated_results)
}

load_and_filter_matched_data <- function(config, matching_core, regression_core, spec_config) {
  matched_file_path <- file.path(
    matched_data_dir,
    paste0("matched_pairs_", config$file_id, "_matching_core_", matching_core, "_", ccod_version, ".RData")
  )
  
  if (!file.exists(matched_file_path)) {
    return(NULL)
  }
  
  load(matched_file_path)
  
  # Get the spec index by matching the spec name
  spec_idx <- which(sapply(spec_configs, function(x) x$name == spec_config$name))
  
  matched_expanded <- expand_matched_data(matched_results, EPC_matched_combined)
  matched_data_for_spec <- matched_expanded[[spec_idx]]
  
  rm(matched_results, matched_expanded)
  
  if (is.null(matched_data_for_spec) || nrow(matched_data_for_spec) == 0) {
    return(NULL)
  }
  
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
    
    for (matching_core in names(matching_core_filters)) {
      for (regression_core in names(regression_core_filters)) {
        
        # Skip invalid combinations (wider regression core than matching core)
        if (!is_valid_core_combination(matching_core, regression_core)) {
          message(sprintf(" Skipping invalid combination: matching_core='%s', regression_core='%s' (regression core is wider than matching core)",
                          matching_core, regression_core))
          next
        }
        
        for (config in analysis_configs) {
          matched_data_for_spec <- load_and_filter_matched_data(
            config, matching_core, regression_core, spec_config
          )
          
          if (is.null(matched_data_for_spec) || nrow(matched_data_for_spec) == 0) {
            next
          }
          
          for (current_outcome in outcome_variables) {
            models_counter <<- models_counter + 1
            current_time <- Sys.time()
            if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
              pct <- round(100 * models_counter / total_computations, 1)
              elapsed <- as.numeric(difftime(current_time, start.time, units = "secs"))
              rate <- models_counter / elapsed
              eta_sec <- (total_computations - models_counter) / rate
              eta_min <- eta_sec / 60
              message(sprintf(" [%d/%d] %.1f%% | %.1f models/sec | ETA: %.1f min | PSM %s",
                              models_counter, total_computations, pct, rate, eta_min, current_outcome))
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
  
  return(psm_results[1:local_result_counter])
}

# Process OLS models (once per regression core, then replicate)
for (current_model_name in c("OLS Additive FE", "OLS Interactive FE")) {
  message(sprintf("\n--- Processing %s models ---", current_model_name))
  
  ols_results_for_model <- run_ols_models_single_core(current_model_name)
  ols_results_replicated <- replicate_ols_across_matching_cores(ols_results_for_model)
  ols_results_temp <- append(ols_results_temp, ols_results_replicated)
}

# Process PSM models (full matching_core × regression_core combinations)
for (current_model_name in c("PSM (Matched)", "PSM (Matched) + Subclass FE")) {
  message(sprintf("\n--- Processing %s models ---", current_model_name))
  
  psm_results_for_model <- run_psm_models_full_combinations(current_model_name)
  psm_results <- append(psm_results, psm_results_for_model)
}

# Combine all results
message("\nCombining results...")
master_results <- rbindlist(c(ols_results_temp, psm_results))
output_csv_path_master <- file.path(summary_dir, "master_results.csv")
fwrite(master_results, output_csv_path_master)
message(paste(" Saved all results to:", output_csv_path_master))

end.time <- Sys.time()
time.taken <- end.time - start.time
message(sprintf("\n=== Script Complete ==="))
message(sprintf("Models attempted: %d/%d", models_counter, total_computations))
message(sprintf("Rows written: %d ",
                nrow(master_results) ))
message(sprintf("Runtime: %.2f %s (%.1f models/sec)",
                as.numeric(time.taken), units(time.taken),
                models_counter / as.numeric(time.taken, units = "secs")))