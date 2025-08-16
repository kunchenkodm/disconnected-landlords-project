# Script: 06_run_regressions.R
# Purpose: Run OLS regressions on matched data and equivalent unmatched specifications for all treatment definitions.
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 15, 2025. Last Updated: August 15, 2025.

rm(list=setdiff(ls(), "script"))

# DIAGNOSTICS: RUNTIME
start.time <- Sys.time()

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(fixest)
library(modelsummary)


#### SETUP: INPUTS REQUIRED ####
ccod_version <- CCOD_VERSION
# input_dir <- OUTPUT_DATA_DIR
# output_dir <- file.path(getwd(), "Tables")
processed_data_dir <- PROCESSED_DATA_DIR
matched_data_dir <- MATCHED_DATA_DIR
output_dir <- TABLES_DIR # Save tables to output/tables


# Helper function to expand matched data by joining with the main dataset
expand_matched_data <- function(matched_list, full_data) {
  expanded_list <- lapply(matched_list, function(matched_dt) {
    if (!is.null(matched_dt) && nrow(matched_dt) > 0) {
      matched_dt <- matched_dt[!is.na(uprn)]
      if (nrow(matched_dt) > 0) {
        full_data[matched_dt, on = 'uprn', nomatch = 0]
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  return(expanded_list)
}



# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load the EPC matched refined dataset
input_file <- file.path(processed_data_dir, paste0("epc_matched_refined_", CCOD_VERSION, ".RData"))
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}
message("Loading EPC matched refined dataset from ", input_file)
load(input_file)

##### TREATMENT DEFINITIONS (from script 05) #####
message("Defining all treatment variables...")
# Define the common control group condition
control_group_condition <- quote(source == "Unknown" & grepl("rental \\(private\\)|Rented \\(private\\)", tenure_2, ignore.case = TRUE))

# --- For-Profit Treatments ---
EPC_matched_combined[, treat_for_profit := fcase(grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE), 1L, eval(control_group_condition), 0L, default = NA_integer_)]
EPC_matched_combined[, treat_uk_for_profit := fcase(grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 1L, eval(control_group_condition), 0L, default = NA_integer_)]
EPC_matched_combined[, treat_foreign_for_profit := fcase(grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L, eval(control_group_condition), 0L, default = NA_integer_)]
EPC_matched_combined[, treat_tax_haven_for_profit := fcase(grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_tax_haven) & country_incorporated_tax_haven == 1, 1L, eval(control_group_condition), 0L, default = NA_integer_)]

# --- Non-Profit Treatments ---
EPC_matched_combined[, treat_non_profit := fcase(grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE), 1L, eval(control_group_condition), 0L, default = NA_integer_)]
EPC_matched_combined[, treat_uk_non_profit := fcase(grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 1L, eval(control_group_condition), 0L, default = NA_integer_)]
EPC_matched_combined[, treat_foreign_non_profit := fcase(grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L, eval(control_group_condition), 0L, default = NA_integer_)]
EPC_matched_combined[, treat_tax_haven_non_profit := fcase(grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_tax_haven) & country_incorporated_tax_haven == 1, 1L, eval(control_group_condition), 0L, default = NA_integer_)]

# --- Public Sector Treatment ---
EPC_matched_combined[, treat_public_sector := fcase(grepl("Public Sector", coarse_proprietorship, ignore.case = TRUE), 1L, eval(control_group_condition), 0L, default = NA_integer_)]

# --- Abroad vs. Domestic (For-Profits only) ---
EPC_matched_combined[, treat_abroad_domestic := fcase(!is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L, !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 0L, default = NA_integer_)]

# --- Tax Haven (various) treatments ---
EPC_matched_combined[, treat_tax_haven := fcase(country_incorporated_tax_haven == 1, 1L, eval(control_group_condition), 0L, default = NA_integer_)]
EPC_matched_combined[, treat_british_haven := fcase(country_incorporated_british_haven == 1, 1L, eval(control_group_condition), 0L,default = NA_integer_)]
EPC_matched_combined[, treat_european_haven := fcase(country_incorporated_european_haven == 1, 1L,eval(control_group_condition), 0L,default = NA_integer_)]
EPC_matched_combined[, treat_caribbean_haven := fcase(country_incorporated_caribbean_haven == 1, 1L,eval(control_group_condition), 0L,default = NA_integer_)]
EPC_matched_combined[, treat_other_haven := fcase(country_incorporated_other_haven == 1, 1L,eval(control_group_condition), 0L,default = NA_integer_)]






##### Variable Setup ####
outcome_variable <- "bad_epc"

# Lists of control variables for three specifications
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

##### REGRESSION ANALYSIS FUNCTION #####
#' @param treatment_variable A string with the name of the treatment variable.
#' @param matched_data_list A list of three matched data.tables, one for each specification.
#' @param full_data The complete data.table (`EPC_matched_combined`).
#' @param outcome_var The name of the outcome variable.
#' @param cont_vars A list of vectors of continuous control variables.
#' @param exact_vars A list of vectors of exact matching (categorical) variables.
#' @param coef_label A string for labeling the treatment coefficient in the output table.
#' @param table_title The title for the output table.
#' @param output_filename The filename for the output HTML table.
#' @return Invisibly returns the list of models.
run_regression_analysis <- function(treatment_variable,
                                    matched_data_list,
                                    full_data,
                                    outcome_var,
                                    cont_vars,
                                    exact_vars,
                                    coef_label,
                                    table_title,
                                    output_filename) {
  
  models_list <- list()
  
  # Loop through the 3 specifications
  for (i in 1:3) {
    # Define formulas dynamically for the current specification
    # OLS with additive FEs: continuous vars as covariates, exact vars as FEs
    fml_ols_add <- as.formula(paste0(outcome_var, " ~ ", treatment_variable, " + ", paste(cont_vars[[i]], collapse = " + "), " | ", paste(exact_vars[[i]], collapse = " + ")))
    
    # OLS with interactive FEs
    fml_ols_int <- as.formula(paste0(outcome_var, " ~ ", treatment_variable, " + ", paste(cont_vars[[i]], collapse = " + "), " | ", paste(exact_vars[[i]], collapse = "^")))
    
    # PSM models
    fml_psm <- as.formula(paste(outcome_var, "~", treatment_variable, "+", paste(cont_vars[[i]], collapse = " + ")))
    fml_psm_fe <- as.formula(paste(fml_psm[2], "~", fml_psm[3], "| subclass"))
    
    # Run models for specification i
    models_list[[paste0("ols_add", i)]] <- feols(fml_ols_add, data = full_data, cluster = ~local_authority)
    models_list[[paste0("ols_int", i)]] <- feols(fml_ols_int, data = full_data, cluster = ~local_authority)
    
    if (!is.null(matched_data_list[[i]])) {
      models_list[[paste0("psm", i)]] <- feols(fml_psm, data = matched_data_list[[i]], cluster = ~local_authority)
      models_list[[paste0("psm_fe", i)]] <- feols(fml_psm_fe, data = matched_data_list[[i]], cluster = ~local_authority)
    }
  }
  
  # Dynamically create the coefficient map
  coef_map_dynamic <- c("number_habitable_rooms" = "Habitable Rooms", "total_floor_area" = "Total Floor Area (sq. m)", "lodgement_year" = "Lodgement Year", "ppd_price_sqm" = "Price per sq. m")
  coef_map_dynamic[treatment_variable] <- coef_label
  
  # Generate and save the summary table
  modelsummary(
    models_list,
    output = output_filename,
    title = table_title,
    notes = "All models include controls for property characteristics. OLS models include fixed effects for all matching dimensions. Standard errors are clustered by local authority.",
    coef_map = coef_map_dynamic,
    gof_map = c("nobs", "r.squared"),
    add_header_above = c(" " = 1, "Baseline" = 4, "Council Tax" = 4, "Council Tax + Price Paid" = 4),
    stars = c('*' = .1, '**' = .05, '***' = .01)
  )
  
  message("Saved summary table to: ", output_filename)
  return(invisible(models_list))
}

##### ANALYSIS CONFIGURATION #####
# A list of configurations. Each element contains all necessary metadata to run one full analysis.
analysis_configs <- list(
  list(var = "treat_for_profit", file_id = "for_profit_vs_private_rental", title = "Effect of For-Profit Ownership on Likelihood of a Bad EPC", coef_label = "For-Profit"),
  list(var = "treat_uk_for_profit", file_id = "uk_for_profit_vs_private_rental", title = "Effect of UK For-Profit Ownership on Likelihood of a Bad EPC", coef_label = "UK For-Profit"),
  list(var = "treat_foreign_for_profit", file_id = "foreign_for_profit_vs_private_rental", title = "Effect of Foreign For-Profit Ownership on Likelihood of a Bad EPC", coef_label = "Foreign For-Profit"),
  list(var = "treat_tax_haven_for_profit", file_id = "tax_haven_for_profit_vs_private_rental", title = "Effect of Tax Haven For-Profit Ownership on Likelihood of a Bad EPC", coef_label = "Tax Haven For-Profit"),
  list(var = "treat_non_profit", file_id = "non_profit_vs_private_rental", title = "Effect of Non-Profit Ownership on Likelihood of a Bad EPC", coef_label = "Non-Profit"),
  list(var = "treat_uk_non_profit", file_id = "uk_non_profit_vs_private_rental", title = "Effect of UK Non-Profit Ownership on Likelihood of a Bad EPC", coef_label = "UK Non-Profit"),
  list(var = "treat_foreign_non_profit", file_id = "foreign_non_profit_vs_private_rental", title = "Effect of Foreign Non-Profit Ownership on Likelihood of a Bad EPC", coef_label = "Foreign Non-Profit"),
  list(var = "treat_tax_haven_non_profit", file_id = "tax_haven_non_profit_vs_private_rental", title = "Effect of Tax Haven Non-Profit Ownership on Likelihood of a Bad EPC", coef_label = "Tax Haven Non-Profit"),
  list(var = "treat_public_sector", file_id = "public_sector_vs_private_rental", title = "Effect of Public Sector Ownership on Likelihood of a Bad EPC", coef_label = "Public Sector"),
  list(var = "treat_abroad_domestic", file_id = "abroad_vs_domestic", title = "Effect of Abroad vs. Domestic For-Profit Ownership on Likelihood of a Bad EPC", coef_label = "Abroad vs. Domestic"),
  list(var = "treat_tax_haven", file_id = "tax_haven_vs_private_rental", title = "Effect of Tax Haven Ownership on Likelihood of a Bad EPC", coef_label = "Tax Haven"),
  list(var = "treat_british_haven", file_id = "british_haven_vs_private_rental", title = "Effect of British Haven Ownership on Likelihood of a Bad EPC", coef_label = "British Haven"),
  list(var = "treat_european_haven", file_id = "european_haven_vs_private_rental", title = "Effect of European Haven Ownership on Likelihood of a Bad EPC", coef_label = "European Haven"),
  list(var = "treat_caribbean_haven", file_id = "caribbean_haven_vs_private_rental", title = "Effect of Caribbean Haven Ownership on Likelihood of a Bad EPC", coef_label = "Caribbean Haven"),
  list(var = "treat_other_haven", file_id = "other_haven_vs_private_rental", title = "Effect of Other Haven Ownership on Likelihood of a Bad EPC", coef_label = "Other Haven")
)

##### EXECUTION LOOP #####
# Loop through each configuration and run the analysis
for (config in analysis_configs) {
  
  message(paste0("\n--- Running analysis for: '", config$var, "' ---"))
  
  # Using try() to ensure that an error in one analysis does not stop the entire script
  try({
    # 1. Construct file path for matched data
    matched_file_path <- file.path(matched_data_dir, paste0("matched_pairs_", config$file_id, "_", CCOD_VERSION, ".RData")) 
    if (file.exists(matched_file_path)) {
      # 2. Load and expand the matched data
      load(matched_file_path) # Loads 'matched_results'
      matched_expanded_data <- expand_matched_data(matched_results, EPC_matched_combined)
      rm(matched_results)
      
      # 3. Call the main analysis function
      run_regression_analysis(
        treatment_variable = config$var,
        matched_data_list = matched_expanded_data,
        full_data = EPC_matched_combined,
        outcome_var = outcome_variable,
        cont_vars = continuous_controls,
        exact_vars = exact_controls,
        coef_label = config$coef_label,
        table_title = config$title,
        output_filename = file.path(output_dir, paste0("table_", config$file_id, ".html"))
      )
    } else {
      warning("Matched file not found, skipping analysis for ", config$var, ". Path: ", matched_file_path)
    }
  })
}
message("Done")
# DIAGNOSTICS: RUNTIME
end.time <- Sys.time()
time.taken <- end.time - start.time
message("All analyses completed. \n Script 06 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")
