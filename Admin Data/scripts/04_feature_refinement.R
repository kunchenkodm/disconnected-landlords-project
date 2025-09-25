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

# Generate (restore) variable for the current and potential energy consumption of the entire property
EPC_matched_combined[, energy_consumption_current_property   := energy_consumption_current   * total_floor_area]
EPC_matched_combined[, energy_consumption_potential_property := energy_consumption_potential * total_floor_area]

# Generate variable for the gap between potential and current energy consumption
EPC_matched_combined[, energy_consumption_gap   := energy_consumption_potential - energy_consumption_current]
EPC_matched_combined[, energy_consumption_gap_property   := energy_consumption_potential_property - energy_consumption_current_property]

# Generate variable for the gap between potential and current energy efficiency
# Memo: [energy_efficiency] = (£/m²/year where cost is derived from kWh)
EPC_matched_combined[, energy_efficiency_potential_gap  := potential_energy_efficiency - current_energy_efficiency]

# Generate variable for the gap between current energy efficiency and borderline for EPC < C (<=68 numeric)
EPC_matched_combined[, energy_efficiency_bad_epc_gap  := current_energy_efficiency - 68]

# Generate variable for the gap between current energy efficiency and borderline for next worst EPC rating
EPC_matched_combined[, energy_efficiency_worse_epc_gap := fcase(
  is.na(current_energy_efficiency), NA_real_, # Handle NA values first
  current_energy_efficiency >= 92, current_energy_efficiency - 91, # A -> B
  current_energy_efficiency >= 81, current_energy_efficiency - 80, # B -> C
  current_energy_efficiency >= 69, current_energy_efficiency - 68, # C -> D
  current_energy_efficiency >= 55, current_energy_efficiency - 54, # D -> E
  current_energy_efficiency >= 39, current_energy_efficiency - 38, # E -> F
  current_energy_efficiency >= 21, current_energy_efficiency - 20, # F -> G
  default = 0 # For band G, there is no worse band
)]

# Generate variable for having a borderline good EPC
# sd = 11.83, 0.5 sd caliper
global_sd <- sd(EPC_matched_combined$current_energy_efficiency, na.rm = TRUE)
global_half_sd <- 0.5 * global_sd   # caliper width

# Flag: borderline around cutoff at 69
EPC_matched_combined[, borderline_good_epc := fcase(
  current_energy_efficiency > 69 & 
    current_energy_efficiency <= 69 + global_half_sd, 1,
  
  is.na(current_energy_efficiency), NA_real_,
  default = 0
)]


# # Generate variable for having a borderline better EPC (0.5 band SD of lower + higher EPC above borderline)
# # Compute within-band SDs of the gap variable
# band_sds <- EPC_matched_combined[, .(
#   sd_gap = sd(energy_efficiency_worse_epc_gap, na.rm = TRUE)
# ), by = current_energy_rating]
# 
# # EPC cutoffs table
# epc_cutoffs <- data.table(
#   current_energy_rating = c("A", "B", "C", "D", "E", "F", "G"),
#   lower_cutoff = c(92, 81, 69, 55, 39, 21, 0),
#   upper_cutoff = c(100, 91, 80, 68, 54, 38, 20)
# )
# 
# # Merge SDs + cutoffs
# epc_info <- merge(epc_cutoffs, band_sds, by = "current_energy_rating", all.x = TRUE)
# 
# # Compute half-SDs (capped at 5 for stability)
# epc_info[, half_sd := pmin(0.5 * sd_gap, 5)]
# 
# # Merge into main dataset
# EPC_matched_combined <- merge(
#   EPC_matched_combined,
#   epc_info,
#   by = "current_energy_rating",
#   all.x = TRUE
# )
# 
# # Band-specific borderline flag
# EPC_matched_combined[, borderline_better_epc := fcase(
#   current_energy_efficiency > lower_cutoff & 
#     current_energy_efficiency <= lower_cutoff + half_sd, 1,
#   
#   is.na(current_energy_efficiency), NA_real_,
#   default = 0
# )]
# EPC_matched_combined[, c("lower_cutoff", "half_sd") := NULL]
# rm(global_sd, global_half_sd, band_sds, epc_cutoffs, epc_info)

band_sds <- EPC_matched_combined[, .(
  sd_gap = sd(energy_efficiency_worse_epc_gap, na.rm = TRUE),
  n = .N
), by = current_energy_rating]

# EPC cutoffs
epc_cutoffs <- data.table(
  current_energy_rating = c("A", "B", "C", "D", "E", "F", "G"),
  lower_cutoff = c(92, 81, 69, 55, 39, 21, 0),
  upper_cutoff = c(100, 91, 80, 68, 54, 38, 20)
)

# Merge SDs + cutoffs
epc_info <- merge(epc_cutoffs, band_sds, by = "current_energy_rating", all.x = TRUE)

# Compute pooled SDs for adjacent bands
# function for pooled SD
pooled_sd <- function(sd1, n1, sd2, n2) {
  sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
}

# create a column with pooled SD of current band + worse band
setorder(epc_info, -lower_cutoff)  # ensure A → G order
epc_info[, pooled_sd := fifelse(
  current_energy_rating != "G",
  pooled_sd(sd_gap, n, shift(sd_gap, type = "lead"), shift(n, type = "lead")),
  NA_real_
)]

# Half SD caliper (capped at the global half sd)
epc_info[, half_sd := pmin(0.5 * pooled_sd, global_half_sd)]

# Print diagnostics
print(epc_info[, .(current_energy_rating, pooled_sd, half_sd)])

# Merge into main dataset
EPC_matched_combined <- merge(
  EPC_matched_combined,
  epc_info[, .(current_energy_rating, lower_cutoff, half_sd)],
  by = "current_energy_rating",
  all.x = TRUE
)

# Create borderline dummy
EPC_matched_combined[, borderline_better_epc := fcase(
  current_energy_efficiency > lower_cutoff &
    current_energy_efficiency <= lower_cutoff + half_sd, 1,
  
  is.na(current_energy_efficiency), NA_real_,
  default = 0
)]

# Clean up helper columns and objects ---
EPC_matched_combined[, c("lower_cutoff", "half_sd") := NULL]
rm(band_sds, epc_cutoffs, epc_info, pooled_sd)





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
