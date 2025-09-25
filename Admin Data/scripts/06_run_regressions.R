# Script: 06_run_regressions.R
# Purpose: Generate CSV summary from regression results.
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 15, 2025. Last Updated: September 25, 2025.

# This  script runs a series of regressions across different model 
# specifications and matching strategies, calculates the mean ordering of
# treatments, and saves the detailed results and the mean rankings to CSV files.

rm(list=ls())

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
    if (!is.null(matched_dt) && nrow(matched_dt) > 0) {
      full_data[matched_dt[!is.na(uprn)], on = 'uprn', nomatch = 0]
    } else { NULL }
  })
}

# -----------------------------------------------------------------------------
# Load and prepare data
# -----------------------------------------------------------------------------
input_file <- file.path(processed_data_dir, paste0("epc_matched_refined_", CCOD_VERSION, ".RData"))
load(input_file)
source(here::here("scripts", "treatment_definitions.R"))
EPC_matched_combined <- define_treatments(EPC_matched_combined)
EPC_matched_combined[, energy_cons_curr_per_floor_area := ifelse(total_floor_area == 0, NA_real_, energy_consumption_current / total_floor_area)]

# -----------------------------------------------------------------------------
# Define variable sets and analysis configurations
# -----------------------------------------------------------------------------
outcome_variables <- c("bad_epc", "energy_consumption_current", "energy_consumption_current_property", "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh", "energy_consumption_gap", "energy_consumption_gap_property", "energy_efficiency_potential_gap", "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap", "borderline_good_epc", "borderline_better_epc")
continuous_controls <- list(c("number_habitable_rooms", "total_floor_area", "lodgement_year"), c("number_habitable_rooms", "total_floor_area", "lodgement_year"), c("number_habitable_rooms", "total_floor_area", "lodgement_year", "ppd_price_sqm"))
exact_controls <- list(c("property_type", "main_fuel", "construction_age_band", "built_form", "local_authority"), c("property_type", "main_fuel", "tax_band", "construction_age_band", "built_form", "local_authority"), c("property_type", "main_fuel", "tax_band", "ppd_year_transfer", "construction_age_band", "built_form", "local_authority"))
analysis_configs <- list(
  list(var = "treat_for_profit", file_id = "for_profit_vs_private_rental", title = "Effect of For-Profit Ownership", coef_label = "For-Profit"),
  list(var = "treat_uk_for_profit", file_id = "uk_for_profit_vs_private_rental", title = "Effect of UK For-Profit Ownership", coef_label = "UK For-Profit"),
  list(var = "treat_foreign_for_profit", file_id = "foreign_for_profit_vs_private_rental", title = "Effect of Foreign For-Profit Ownership", coef_label = "Foreign For-Profit"),
  list(var = "treat_tax_haven_for_profit", file_id = "tax_haven_for_profit_vs_private_rental", title = "Effect of Tax Haven For-Profit Ownership", coef_label = "Tax Haven For-Profit"),
  list(var = "treat_non_profit", file_id = "non_profit_vs_private_rental", title = "Effect of Non-Profit Ownership", coef_label = "Non-Profit"),
  list(var = "treat_uk_non_profit", file_id = "uk_non_profit_vs_private_rental", title = "Effect of UK Non-Profit Ownership", coef_label = "UK Non-Profit"),
  list(var = "treat_foreign_non_profit", file_id = "foreign_non_profit_vs_private_rental", title = "Effect of Foreign Non-Profit Ownership", coef_label = "Foreign Non-Profit"),
  list(var = "treat_tax_haven_non_profit", file_id = "tax_haven_non_profit_vs_private_rental", title = "Effect of Tax Haven Non-Profit Ownership", coef_label = "Tax Haven Non-Profit"),
  list(var = "treat_public_sector", file_id = "public_sector_vs_private_rental", title = "Effect of Public Sector Ownership", coef_label = "Public Sector"),
  list(var = "treat_tax_haven", file_id = "tax_haven_vs_private_rental", title = "Effect of Tax Haven Ownership", coef_label = "Tax Haven"),
  list(var = "treat_british_haven", file_id = "british_haven_vs_private_rental", title = "Effect of British Haven Ownership", coef_label = "British Haven"),
  list(var = "treat_european_haven", file_id = "european_haven_vs_private_rental", title = "Effect of European Haven Ownership", coef_label = "European Haven"),
  list(var = "treat_caribbean_haven", file_id = "caribbean_haven_vs_private_rental", title = "Effect of Caribbean Haven Ownership", coef_label = "Caribbean Haven"),
  list(var = "treat_other_haven", file_id = "other_haven_vs_private_rental", title = "Effect of Other Haven Ownership", coef_label = "Other Haven")
)
model_types <- c("OLS Additive FE", "OLS Interactive FE", "PSM (Matched)", "PSM (Matched) + Subclass FE")

# --- 1. Pre-calculate all regression results ---
message("Pre-calculating all regression results...")
master_results_list <- list()
for (current_outcome in outcome_variables) {
  for (config in analysis_configs) {
    is_psm_needed <- any(grepl("PSM", model_types))
    matched_expanded_data <- NULL
    if(is_psm_needed) {
      matched_file_path <- file.path(matched_data_dir, paste0("matched_pairs_", config$file_id, "_", CCOD_VERSION, ".RData"))
      if (file.exists(matched_file_path)) {
        load(matched_file_path)
        matched_expanded_data <- expand_matched_data(matched_results, EPC_matched_combined)
        rm(matched_results)
      }
    }
    for (current_model_name in model_types) {
      for (i in 1:3) {
        spec_name <- c("Baseline", "Council Tax", "Council Tax + Price Paid")[i]
        fml <- NULL; data_to_use <- NULL
        if(current_model_name == "OLS Additive FE"){
          fml <- as.formula(paste0(current_outcome, " ~ ", config$var, " + ", paste(continuous_controls[[i]], collapse = " + "), " | ", paste(exact_controls[[i]], collapse = " + ")))
          data_to_use <- EPC_matched_combined
        } else if (current_model_name == "OLS Interactive FE"){
          fml <- as.formula(paste0(current_outcome, " ~ ", config$var, " + ", paste(continuous_controls[[i]], collapse = " + "), " | ", paste(exact_controls[[i]], collapse = "^")))
          data_to_use <- EPC_matched_combined
        } else if (current_model_name == "PSM (Matched)" && !is.null(matched_expanded_data) && !is.null(matched_expanded_data[[i]])){
          fml <- as.formula(paste(current_outcome, "~", config$var, "+", paste(continuous_controls[[i]], collapse = " + ")))
          data_to_use <- matched_expanded_data[[i]]
        } else if (current_model_name == "PSM (Matched) + Subclass FE" && !is.null(matched_expanded_data) && !is.null(matched_expanded_data[[i]])){
          base_fml <- as.formula(paste(current_outcome, "~", config$var, "+", paste(continuous_controls[[i]], collapse = " + ")))
          fml <- as.formula(paste(base_fml[2], "~", base_fml[3], "| subclass"))
          data_to_use <- matched_expanded_data[[i]]
        }
        if(!is.null(fml) && !is.null(data_to_use)){
          try({
            model <- feols(fml, data = data_to_use, cluster = ~local_authority)
            ct <- coeftable(model)
            if (config$var %in% rownames(ct)) {
              row <- ct[config$var, ]
              master_results_list[[length(master_results_list) + 1]] <- data.table(
                coef = row["Estimate"], se = row["Std. Error"],
                nobs = nobs(model), r2 = r2(model)["r2"],
                outcome = current_outcome,
                treatment = config$title, model = current_model_name, spec = spec_name
              )
            }
          }, silent = TRUE)
        }
      }
    }
  }
}
master_results <- rbindlist(master_results_list)

# --- 2. Calculate and Report Mean Treatment Ordering and Save CSVs ---
message("\n\n--- Calculating Mean Treatment Ordering Across All Specifications ---")

# Calculate ranks within each specification
master_results[, rank := frankv(coef, order = -1, ties.method = "average"), by = .(outcome, model, spec)]

# Calculate mean rank and standard error of the rank for each treatment
mean_ranks <- master_results[, .(
  mean_rank = mean(rank, na.rm = TRUE),
  se_rank = sd(rank, na.rm = TRUE) / sqrt(.N)
), by = .(treatment)]

# Sort by mean rank
mean_ranks <- mean_ranks[order(mean_rank)]

# Print the results to the console
message("\nMean Treatment Ranks (lower is better):")
print(mean_ranks)

# Save the mean ranks to a CSV file
output_csv_path_ranks <- file.path(summary_dir, "mean_treatment_rankings.csv")
fwrite(mean_ranks, output_csv_path_ranks)
message(paste("\nSaved mean ranking table to:", output_csv_path_ranks))

# Save the master results to a CSV file
output_csv_path_master <- file.path(summary_dir, "master_results.csv")
fwrite(master_results, output_csv_path_master)
message(paste("Saved all regression results to:", output_csv_path_master))

# --- 3. Finalize Script ---
message("\n\nAll CSV summaries have been generated.")
# DIAGNOSTICS: RUNTIME
end.time <- Sys.time()
time.taken <- end.time - start.time
message("CSV generation complete. \n Script 07 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")
