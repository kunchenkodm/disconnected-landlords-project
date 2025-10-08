# Script: 06_run_regressions.R 
# Purpose: Generate CSV summary from regression results.
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 15, 2025. Last Updated: October 8, 2025.

rm(list = ls())
gc()

# DIAGNOSTICS: RUNTIME
start.time <- Sys.time()

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(fixest)

#### SETUP: INPUTS REQUIRED ####
ccod_version <- CCOD_VERSION
processed_data_dir <- PROCESSED_DATA_DIR
matched_data_dir <- MATCHED_DATA_DIR
summary_dir <- file.path(getwd(), "output", "summary_tables")

if (!dir.exists(summary_dir)) {
  dir.create(summary_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Helper function to expand matched data
# -----------------------------------------------------------------------------
expand_matched_data <- function(matched_list, full_data) {
  lapply(matched_list, function(matched_dt) {
    if (is.null(matched_dt) || nrow(matched_dt) == 0) return(NULL)
    full_data[matched_dt[!is.na(uprn)], on = "uprn", nomatch = 0]
  })
}

# -----------------------------------------------------------------------------
# Helper function to build formulas (centralized)
# -----------------------------------------------------------------------------
build_formula <- function(outcome, treatment_var, continuous_vars, exact_vars, 
                          model_type) {
  
  # Remove empty strings
  continuous_vars <- continuous_vars[continuous_vars != ""]
  exact_vars <- exact_vars[exact_vars != ""]
  
  # Build RHS
  rhs <- paste(c(treatment_var, continuous_vars), collapse = " + ")
  
  if (model_type == "OLS Additive FE") {
    fe <- paste(exact_vars, collapse = " + ")
    if (fe != "") {
      return(as.formula(paste(outcome, "~", rhs, "|", fe)))
    } else {
      return(as.formula(paste(outcome, "~", rhs)))
    }
    
  } else if (model_type == "OLS Interactive FE") {
    # Fixed: Use proper fixest syntax for interactions
    fe_interact <- paste(exact_vars, collapse = "^")
    if (fe_interact != "") {
      return(as.formula(paste(outcome, "~", rhs, "|", fe_interact)))
    } else {
      return(as.formula(paste(outcome, "~", rhs)))
    }
    
  } else if (model_type == "PSM (Matched)") {
    return(as.formula(paste(outcome, "~", rhs)))
    
  } else if (model_type == "PSM (Matched) + Subclass FE") {
    return(as.formula(paste(outcome, "~", rhs, "| subclass")))
  }
  
  stop("Unknown model type: ", model_type)
}

# -----------------------------------------------------------------------------
# Helper function to get subset filter
# -----------------------------------------------------------------------------
get_subset_filter <- function(subset_name) {
  switch(subset_name,
         base = quote(rep(TRUE, .N)),
         council_tax = quote(!is.na(tax_band)),
         ppd = quote(!is.na(ppd_price_sqm)),
         ppd_counciltax = quote(!is.na(tax_band) & !is.na(ppd_price_sqm)),
         stop("Unknown subset: ", subset_name)
  )
}

# -----------------------------------------------------------------------------
# Load and prepare data
# -----------------------------------------------------------------------------
message("Loading data...")
input_file <- file.path(processed_data_dir, 
                        paste0("epc_matched_refined_", CCOD_VERSION, ".RData"))
load(input_file)

source(here::here("scripts", "treatment_definitions.R"))
EPC_matched_combined <- define_treatments(EPC_matched_combined)
EPC_matched_combined[, energy_cons_curr_per_floor_area := 
                       fifelse(total_floor_area == 0, NA_real_, 
                               energy_consumption_current / total_floor_area)]

# Set key for faster subsetting
setkey(EPC_matched_combined, uprn)

# -----------------------------------------------------------------------------
# Define variable sets and analysis configurations
# -----------------------------------------------------------------------------
outcome_variables <- c(
  "bad_epc", "energy_consumption_current", "energy_consumption_current_property", 
  "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh", "energy_consumption_gap", 
  "energy_consumption_gap_property", "energy_efficiency_potential_gap", 
  "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap", 
  "borderline_good_epc", "borderline_better_epc"
)

continuous_controls <- list(
  c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
  c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
  c("number_habitable_rooms", "total_floor_area", "lodgement_year", "ppd_price_sqm")
)

exact_controls <- list(
  c("property_type", "main_fuel", "construction_age_band", "built_form", "local_authority"),
  c("property_type", "main_fuel", "tax_band", "construction_age_band", "built_form", "local_authority"),
  c("property_type", "main_fuel", "tax_band", "ppd_year_transfer", "construction_age_band", "built_form", "local_authority")
)

analysis_configs <- list(
  list(var = "treat_for_profit", file_id = "for_profit_vs_private_rental", 
       title = "Effect of For-Profit Ownership"),
  list(var = "treat_uk_for_profit", file_id = "uk_for_profit_vs_private_rental", 
       title = "Effect of UK For-Profit Ownership"),
  list(var = "treat_foreign_for_profit", file_id = "foreign_for_profit_vs_private_rental", 
       title = "Effect of Foreign For-Profit Ownership"),
  list(var = "treat_tax_haven_for_profit", file_id = "tax_haven_for_profit_vs_private_rental", 
       title = "Effect of Tax Haven For-Profit Ownership"),
  list(var = "treat_non_profit", file_id = "non_profit_vs_private_rental", 
       title = "Effect of Non-Profit Ownership"),
  list(var = "treat_uk_non_profit", file_id = "uk_non_profit_vs_private_rental", 
       title = "Effect of UK Non-Profit Ownership"),
  list(var = "treat_foreign_non_profit", file_id = "foreign_non_profit_vs_private_rental", 
       title = "Effect of Foreign Non-Profit Ownership"),
  list(var = "treat_tax_haven_non_profit", file_id = "tax_haven_non_profit_vs_private_rental", 
       title = "Effect of Tax Haven Non-Profit Ownership"),
  list(var = "treat_public_sector", file_id = "public_sector_vs_private_rental", 
       title = "Effect of Public Sector Ownership"),
  list(var = "treat_tax_haven", file_id = "tax_haven_vs_private_rental", 
       title = "Effect of Tax Haven Ownership"),
  list(var = "treat_british_haven", file_id = "british_haven_vs_private_rental", 
       title = "Effect of British Haven Ownership"),
  list(var = "treat_european_haven", file_id = "european_haven_vs_private_rental", 
       title = "Effect of European Haven Ownership"),
  list(var = "treat_caribbean_haven", file_id = "caribbean_haven_vs_private_rental", 
       title = "Effect of Caribbean Haven Ownership"),
  list(var = "treat_other_haven", file_id = "other_haven_vs_private_rental", 
       title = "Effect of Other Haven Ownership")
)

model_types <- c("OLS Additive FE", "OLS Interactive FE", 
                 "PSM (Matched)", "PSM (Matched) + Subclass FE")

spec_names <- c("Baseline", "Council Tax", "Council Tax + Price Paid")
subset_names <- c("base", "council_tax", "ppd", "ppd_counciltax")

# -----------------------------------------------------------------------------
# Index matched data files
# -----------------------------------------------------------------------------
message("Indexing matched data files...")
matched_data_files <- list()

for (config in analysis_configs) {
  matched_file_path <- file.path(matched_data_dir,
                                 paste0("matched_pairs_", config$file_id, 
                                        "_", CCOD_VERSION, ".RData"))
  if (file.exists(matched_file_path)) {
    matched_data_files[[config$file_id]] <- matched_file_path
  }
}

n_configs_with_matched <- length(matched_data_files)

# -----------------------------------------------------------------------------
# Calculate total number of models to estimate
# -----------------------------------------------------------------------------
message("Calculating total number of models...")

n_ols_models <- length(analysis_configs) * length(outcome_variables) * 
  2 * 3 * length(subset_names)  # 2 OLS types, 3 specs, 4 subsets

n_psm_models <- n_configs_with_matched * length(outcome_variables) * 
  2 * 3 * length(subset_names)  # 2 PSM types, 3 specs, 4 subsets

total_models <- n_ols_models + n_psm_models

message(sprintf("  OLS models to estimate: %d", n_ols_models))
message(sprintf("  PSM models to estimate: %d", n_psm_models))
message(sprintf("  Total models to estimate: %d", total_models))
message(sprintf("  Estimated time: %.1f-%.1f minutes (at 50-100 models/sec)\n", 
                total_models/100/60, total_models/50/60))

# -----------------------------------------------------------------------------
# Run regressions with optimized loop nesting (OPTION 1)
# -----------------------------------------------------------------------------
message("Running regressions with optimized loop structure...")

# Pre-allocate results list
master_results_list <- vector("list", total_models)
result_counter <- 0
models_counter <- 0

# Progress tracking
last_update_time <- Sys.time()
update_interval <- 5  # seconds between progress updates

# OUTER LOOP: Model type (determines data structure needed)
for (current_model_name in model_types) {
  
  is_psm_model <- grepl("PSM", current_model_name)
  message(sprintf("\n--- Processing %s models ---", current_model_name))
  
  # SECOND LOOP: Specification (determines controls and filtering)
  for (i in 1:3) {
    
    spec_name <- spec_names[i]
    message(sprintf("  Specification: %s", spec_name))
    
    # THIRD LOOP: Subset (determines data filtering)
    for (subset_name in subset_names) {
      
      # Pre-filter OLS data once for this (model_type, spec, subset) combination
      ols_data_filtered <- NULL
      if (!is_psm_model) {
        subset_filter <- get_subset_filter(subset_name)
        ols_data_filtered <- EPC_matched_combined[eval(subset_filter)]
        
        # Skip if no data
        if (nrow(ols_data_filtered) == 0) {
          message(sprintf("    Skipping subset '%s' - no data after filtering", subset_name))
          next
        }
      }
      
      # FOURTH LOOP: Treatment/Config (now we process each treatment on same data)
      for (config in analysis_configs) {
        
        # For PSM models, load matched data once per config
        matched_data_for_spec <- NULL
        if (is_psm_model) {
          
          # Skip if no matched data file for this config
          if (!(config$file_id %in% names(matched_data_files))) {
            next
          }
          
          # Load and expand matched data
          load(matched_data_files[[config$file_id]])
          matched_expanded <- expand_matched_data(matched_results, EPC_matched_combined)
          rm(matched_results)
          
          # Get data for current spec and apply subset filter
          if (!is.null(matched_expanded[[i]])) {
            subset_filter <- get_subset_filter(subset_name)
            matched_data_for_spec <- matched_expanded[[i]][eval(subset_filter)]
          }
          
          rm(matched_expanded)
          
          # Skip if no data after filtering
          if (is.null(matched_data_for_spec) || nrow(matched_data_for_spec) == 0) {
            next
          }
        }
        
        # Determine which data to use
        data_to_use <- if (is_psm_model) matched_data_for_spec else ols_data_filtered
        
        # INNERMOST LOOP: Outcomes (fastest varying, minimal overhead)
        for (current_outcome in outcome_variables) {
          
          models_counter <- models_counter + 1
          
          # Progress reporting (time-based)
          current_time <- Sys.time()
          if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
            pct <- round(100 * models_counter / total_models, 1)
            elapsed <- as.numeric(difftime(current_time, start.time, units = "secs"))
            rate <- models_counter / elapsed
            eta_sec <- (total_models - models_counter) / rate
            eta_min <- eta_sec / 60
            
            message(sprintf("    [%d/%d] %.1f%% | %.1f models/sec | ETA: %.1f min | %s",
                            models_counter, total_models, pct, rate, eta_min, current_outcome))
            last_update_time <- current_time
          }
          
          # Build formula (only once per outcome within this combination)
          fml <- build_formula(
            outcome = current_outcome,
            treatment_var = config$var,
            continuous_vars = continuous_controls[[i]],
            exact_vars = exact_controls[[i]],
            model_type = current_model_name
          )
          
          # Run regression with proper error handling
          result <- tryCatch({
            model <- feols(fml, data = data_to_use, cluster = ~local_authority)
            ct <- coeftable(model)
            
            if (!(config$var %in% rownames(ct))) {
              NULL
            } else {
              row <- ct[config$var, ]
              data.table(
                coef = row["Estimate"],
                se = row["Std. Error"],
                nobs = nobs(model),
                r2 = r2(model, "r2"),
                outcome = current_outcome,
                treatment = config$title,
                model = current_model_name,
                spec = spec_name,
                subset = subset_name
              )
            }
          }, error = function(e) {
            warning(sprintf("Error in %s | %s | %s | %s | %s: %s",
                            current_model_name, spec_name, subset_name,
                            config$title, current_outcome, e$message),
                    call. = FALSE)
            NULL
          })
          
          if (!is.null(result)) {
            result_counter <- result_counter + 1
            master_results_list[[result_counter]] <- result
          }
        } # End outcomes loop
        
        # Clean up matched data after processing all outcomes for this config
        if (is_psm_model) {
          rm(matched_data_for_spec)
          gc(verbose = FALSE)
        }
        
      } # End configs loop
      
      # Clean up filtered OLS data after processing all configs
      if (!is_psm_model) {
        rm(ols_data_filtered)
        gc(verbose = FALSE)
      }
      
    } # End subsets loop
  } # End specs loop
} # End model types loop

# Combine results efficiently
message("\nCombining results...")
master_results <- rbindlist(master_results_list[1:result_counter])

# # -----------------------------------------------------------------------------
# # Calculate and Report Mean Treatment Ordering
# # -----------------------------------------------------------------------------
# message("\n--- Calculating Mean Treatment Ordering ---")
# 
# # Calculate ranks within each specification (using data.table's efficient ranking)
# master_results[, rank := frank(-coef, ties.method = "average"), 
#                by = .(outcome, model, spec, subset)]
# 
# # Calculate mean rank and standard error
# mean_ranks <- master_results[, .(
#   mean_rank = mean(rank, na.rm = TRUE),
#   se_rank = sd(rank, na.rm = TRUE) / sqrt(.N),
#   n_specs = .N
# ), by = treatment][order(mean_rank)]
# 
# # Print results
# message("\nMean Treatment Rankings (lower rank = larger positive effect):")
# print(mean_ranks)

# -----------------------------------------------------------------------------
# Save outputs
# -----------------------------------------------------------------------------
message("\nSaving results...")

# output_csv_path_ranks <- file.path(summary_dir, "mean_treatment_rankings.csv")
# fwrite(mean_ranks, output_csv_path_ranks)
# message(paste("  Saved rankings to:", output_csv_path_ranks))

output_csv_path_master <- file.path(summary_dir, "master_results.csv")
fwrite(master_results, output_csv_path_master)
message(paste("  Saved all results to:", output_csv_path_master))

# -----------------------------------------------------------------------------
# Finalize Script
# -----------------------------------------------------------------------------
end.time <- Sys.time()
time.taken <- end.time - start.time

message(sprintf("\n=== Script Complete ==="))
message(sprintf("Models attempted: %d/%d", models_counter, total_models))
message(sprintf("Successful regressions: %d (%.1f%%)", 
                result_counter, 100 * result_counter / models_counter))
message(sprintf("Runtime: %.2f %s (%.1f models/sec)", 
                as.numeric(time.taken), units(time.taken),
                models_counter / as.numeric(time.taken, units = "secs")))