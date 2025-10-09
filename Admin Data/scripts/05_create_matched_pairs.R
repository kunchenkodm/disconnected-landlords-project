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
# Define core filters
core_filters <- list(
  base = quote(rep(TRUE, .N)),
  council_tax = quote(!is.na(tax_band)),
  ppd = quote(!is.na(ppd_price_sqm)),
  ppd_counciltax = quote(!is.na(tax_band) & !is.na(ppd_price_sqm))
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
perform_matching <- function(treatment_var, treatment_name, core_name, core_data) {
  message("Performing matching for ", treatment_name, " in core: ", core_name)
  dat <- core_data[!is.na(get(treatment_var))]
  base_matching_vars <- c(treatment_var, "number_habitable_rooms", "total_floor_area", "lodgement_year",
                          "property_type", "main_fuel", "construction_age_band",
                          "built_form", "local_authority")
  dat <- na.omit(dat, cols = base_matching_vars)
  dat[, rand_sort := runif(nrow(dat))]
  dat <- dat[order(rand_sort)]
  matched_results <- list()
  
  # First set: property features
  if (nrow(dat[get(treatment_var) == 1]) > 10 & nrow(dat[get(treatment_var) == 0]) > 50) {
    matched_chars1 <- match.data(matchit(
      as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year")),
      data = dat,
      exact = ~property_type + main_fuel + construction_age_band + built_form + local_authority,
      method = "nearest",
      distance = "glm"
    ))
    matched_results[[1]] <- matched_chars1[, list(local_authority, uprn, distance, weights, subclass)]
  }
  # Second set: add council tax
  dat_tax <- na.omit(dat, "tax_band")
  if (nrow(dat_tax[get(treatment_var) == 1]) > 5 & nrow(dat_tax[get(treatment_var) == 0]) > 25) {
    matched_chars2 <- match.data(matchit(
      as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year")),
      data = dat_tax,
      exact = ~property_type + main_fuel + tax_band + construction_age_band + built_form + local_authority,
      method = "nearest",
      distance = "glm"
    ))
    matched_results[[2]] <- matched_chars2[, list(local_authority, uprn, distance, weights, subclass)]
  }
  # Third set: add price data
  price_vars <- c("tax_band", "ppd_price_sqm", "ppd_year_transfer")
  dat_price <- na.omit(dat, cols = price_vars)
  dat_price <- dat_price[!is.infinite(ppd_price_sqm)]
  if (nrow(dat_price[get(treatment_var) == 1]) > 5) {
    matched_chars3 <- match.data(matchit(
      as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year + ppd_price_sqm")),
      data = dat_price,
      exact = ~property_type + main_fuel + tax_band + ppd_year_transfer + construction_age_band + built_form + local_authority,
      method = "nearest",
      distance = "glm"
    ))
    matched_results[[3]] <- matched_chars3[, list(local_authority, uprn, distance, weights, subclass)]
  }
  
  # Output includes core name
  output_file <- file.path(output_dir, paste0("matched_pairs_", treatment_name, "_", core_name, "_", ccod_version, ".RData"))
  save(matched_results, file = output_file)
  message("Matched pairs for ", treatment_name, " core ", core_name, " saved to ", output_file)
  return(matched_results)
}

# Run matching for all treatments and all cores
for (core_name in names(core_filters)) {
  core_filter <- core_filters[[core_name]]
  core_data <- EPC_matched_combined[eval(core_filter)]
  message("Matching for core: ", core_name, " (", nrow(core_data), " rows)")
  for (treat_var in names(treatment_map)) {
    treatment_name <- treatment_map[[treat_var]]
    perform_matching(treat_var, treatment_name, core_name, core_data)
  }
}


message("Matching process completed for both treatment definitions.")


# DIAGNOSTICS: RUNTIME
end.time <- Sys.time()
time.taken <- end.time - start.time
message("\n Script 5 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")


