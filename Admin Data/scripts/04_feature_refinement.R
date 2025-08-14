# Script: 04_feature_refinement.R
# Purpose: Refine the EPC_matched_enhanced dataset by adding necessary features for matching and analysis, and removing unused columns to keep the dataset lean.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025

rm(list=setdiff(ls(), "script"))

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(janitor)

#### SETUP: INPUTS REQUIRED ####
# Configuration section using global variables from 00_setup.R
ccod_version <- CCOD_VERSION
input_dir <- PROCESSED_DATA_DIR
output_dir <- PROCESSED_DATA_DIR

# Input file from feature enhancement script
input_file <- file.path(input_dir, paste0("epc_matched_enhanced_", ccod_version, ".RData"))

# Load the EPC matched enhanced dataset
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}
message("Loading EPC matched enhanced dataset from ", input_file)
load(input_file)

#### FEATURE CREATION ####
# Create derived features necessary for matching and analysis
message("Creating derived features for matching and analysis...")

# Energy Efficiency Dummies: Binary indicators for poor or very poor ratings
# These are used in analysis to identify properties with low energy efficiency
EPC_matched_combined[!is.na(roof_energy_eff), roof_energy_eff_dum := as.numeric(roof_energy_eff %in% c("Poor", "Very Poor"))]
EPC_matched_combined[!is.na(walls_energy_eff), walls_energy_eff_dum := as.numeric(walls_energy_eff %in% c("Poor", "Very Poor"))]
EPC_matched_combined[!is.na(hot_water_energy_eff), hot_water_energy_eff_dum := as.numeric(hot_water_energy_eff %in% c("Poor", "Very Poor"))]
EPC_matched_combined[!is.na(mainheat_energy_eff), mainheat_energy_eff_dum := as.numeric(mainheat_energy_eff %in% c("Poor", "Very Poor"))]
EPC_matched_combined[!is.na(windows_energy_eff), windows_energy_eff_dum := as.numeric(windows_energy_eff %in% c("Poor", "Very Poor"))]

# Normalize multi_glaze_proportion by dividing by 100 (as seen in analysis scripts)
EPC_matched_combined[, multi_glaze_proportion := multi_glaze_proportion / 100]

# Ensure matching variables are formatted correctly
# These variables are used in matching scripts like '03 create matched pairs.R'
# No additional transformation needed for most as they are already in the dataset
message("Ensuring matching variables are available: number_habitable_rooms, total_floor_area, etc.")

#### FEATURE REMOVAL ####
# Remove columns not used in analysis or matching to reduce dataset size and improve efficiency
# Rationale: Keep only variables used in 'analysis.R', '05 analysis.R', and matching scripts
# Unused columns include descriptive text fields, certain counts, and variables not appearing in models
message("Removing unused columns to streamline the dataset...")

# Identify columns to remove based on patterns and specific names
# Descriptive text fields (e.g., *_description) are not used in models
description_cols <- grep("_description", names(EPC_matched_combined), value = TRUE)
# Specific counts and other unused variables
unused_specific <- c("fixed_lighting_outlets_count", "low_energy_fixed_light_count")

# Combine all columns to remove
cols_to_remove <- c(description_cols, unused_specific)

# Remove the identified columns if they exist in the dataset
cols_to_remove <- cols_to_remove[cols_to_remove %in% names(EPC_matched_combined)]
if (length(cols_to_remove) > 0) {
  EPC_matched_combined[, (cols_to_remove) := NULL]
  message("Removed columns: ", paste(cols_to_remove, collapse = ", "))
} else {
  message("No columns identified for removal.")
}

#### SAVE REFINED DATASET ####
# Save the final refined dataset
output_file <- file.path(output_dir, paste0("epc_matched_refined_", ccod_version, ".RData"))
save(EPC_matched_combined, file = output_file)
message("Final refined dataset saved to ", output_file)
