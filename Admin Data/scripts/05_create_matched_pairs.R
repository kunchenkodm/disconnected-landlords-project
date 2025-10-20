# Script: 05_create_matched_pairs.R
# Purpose: Implement matching logic to create sets of matched pairs based on two treatment definitions (For-Profit vs. Non-Profit and Abroad vs. Domestic) with exact matching by local_authority.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last Updated August 15, 2025

rm(list=setdiff(ls(), "script"))


# DIAGNOSTICS: RUNTIME
start.time <- Sys.time()


# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(MatchIt)

#### SETUP: INPUTS REQUIRED ####
# Configuration section using global variables from 00_setup.R
ccod_version <- CCOD_VERSION
input_dir <- PROCESSED_DATA_DIR
output_dir <- MATCHED_DATA_DIR # Save matched pairs to output/matched_data

# Input file from feature refinement script
input_file <- file.path(input_dir, paste0("epc_matched_refined_", ccod_version, ".RData"))

# Load the EPC matched refined dataset
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}
message("Loading EPC matched refined dataset from ", input_file)
load(input_file)

##### TREATMENT DEFINITIONS #####
message("Defining treatment variables...")
source(here::here("scripts", "treatment_definitions.R"))
EPC_matched_combined <- define_treatments(EPC_matched_combined) 
#### MATCHING PROTOCOL ####
message("Starting matching process for  treatment definitions...")
# Define matching core filters
matching_core_filters <- list(
  base = quote(rep(TRUE, .N)),
  council_tax = quote(!is.na(tax_band)),
  ppd = quote(!is.na(ppd_price_sqm)),
  ppd_counciltax = quote(!is.na(tax_band) & !is.na(ppd_price_sqm))
)

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



# Mapping of treatment variables to descriptive output identifiers
treatment_map <- c(
  treat_for_profit = "for_profit_vs_private_rental",
  treat_uk_for_profit = "uk_for_profit_vs_private_rental",
  treat_foreign_for_profit = "foreign_for_profit_vs_private_rental",
  treat_tax_haven_for_profit = "tax_haven_for_profit_vs_private_rental",
  treat_non_profit = "non_profit_vs_private_rental",
  treat_uk_non_profit = "uk_non_profit_vs_private_rental",
  treat_foreign_non_profit = "foreign_non_profit_vs_private_rental",
  treat_tax_haven_non_profit = "tax_haven_non_profit_vs_private_rental",
  treat_public_sector = "public_sector_vs_private_rental",
  treat_tax_haven = "tax_haven_vs_private_rental",
  treat_british_haven = "british_haven_vs_private_rental",
  treat_european_haven = "european_haven_vs_private_rental",
  treat_caribbean_haven = "caribbean_haven_vs_private_rental",
  treat_other_haven = "other_haven_vs_private_rental"
)

# Set seed for reproducibility
set.seed(20230703)

# Matching will be done with exact matching by local_authority
# Following the protocol in '03 create matched pairs.R', create three sets of matched pairs for each treatment
# with varying matching variables: property level features, council tax band, and property price data

# Function to perform matching and save results
perform_matching <- function(treatment_var, treatment_name, matching_core, core_data) {
  message("Performing matching for ", treatment_name, " in matching core: ", matching_core)
  dat <- core_data[!is.na(get(treatment_var))]
  base_matching_vars <- c(treatment_var, "number_habitable_rooms", "total_floor_area", "lodgement_year",
                          "property_type", "main_fuel", "construction_age_band",
                          "built_form", "local_authority")
  dat <- na.omit(dat, cols = base_matching_vars)
  dat[, rand_sort := runif(nrow(dat))]
  dat <- dat[order(rand_sort)]
  matched_results <- list()
  
  # Use spec_configs instead of hardcoded indices
  for (i in seq_along(spec_configs)) {
    spec_config <- spec_configs[[i]]
    
    # Apply spec-specific data filtering
    required_vars <- unique(c(base_matching_vars, spec_config$continuous_vars, spec_config$exact_vars))
    dat_spec <- na.omit(dat, cols = required_vars)
    
    # Remove infinite values if ppd_price_sqm is included
    if ("ppd_price_sqm" %in% spec_config$continuous_vars) {
      dat_spec <- dat_spec[!is.infinite(ppd_price_sqm)]
    }
    
    # Check minimum sample sizes
    min_treated <- if ("ppd_price_sqm" %in% spec_config$continuous_vars) 5 else 
      if ("tax_band" %in% spec_config$exact_vars) 5 else 10
    min_control <- if ("ppd_price_sqm" %in% spec_config$continuous_vars) 25 else
      if ("tax_band" %in% spec_config$exact_vars) 25 else 50
    
    if (nrow(dat_spec[get(treatment_var) == 1]) > min_treated & 
        nrow(dat_spec[get(treatment_var) == 0]) > min_control) {
      
      # Build formula dynamically
      continuous_formula <- paste(spec_config$continuous_vars, collapse = " + ")
      exact_formula <- paste(spec_config$exact_vars, collapse = " + ")
      
      matched_chars <- match.data(matchit(
        as.formula(paste0(treatment_var, " ~ ", continuous_formula)),
        data = dat_spec,
        exact = as.formula(paste0("~ ", exact_formula)),
        method = "nearest",
        distance = "glm"
      ))
      matched_results[[i]] <- matched_chars[, list(local_authority, uprn, distance, weights, subclass)]
    }
  }
  
  # Store matching core information in saved file
  matching_core_metadata <- list(
    matching_core_name = matching_core,
    matching_core_filter_expression = matching_core_filters[[matching_core]],
    timestamp = Sys.time()
  )
  
  # Output includes matching core name
  output_file <- file.path(output_dir, paste0("matched_pairs_", treatment_name, "_matching_core_", matching_core, "_", ccod_version, ".RData"))
  save(matched_results, matching_core_metadata, file = output_file)
  message("Matched pairs for ", treatment_name, " matching core ", matching_core, " saved to ", output_file)
  return(matched_results)
}

# Run matching for all treatments and all matching cores
for (matching_core in names(matching_core_filters)) {
  matching_core_filter <- matching_core_filters[[matching_core]]
  core_data <- EPC_matched_combined[eval(matching_core_filter)]
  message("Matching for matching core: ", matching_core, " (", nrow(core_data), " rows)")
  for (treat_var in names(treatment_map)) {
    treatment_name <- treatment_map[[treat_var]]
    perform_matching(treat_var, treatment_name, matching_core, core_data)
  }
}


message("Matching process completed for both treatment definitions.")


# DIAGNOSTICS: RUNTIME
end.time <- Sys.time()
time.taken <- end.time - start.time
message("\n Script 5 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")


