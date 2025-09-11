# Script: 03_feature_enhancement.R
# Purpose: Enhance the EPC_matched_combined dataset by cleaning variable names and merging with PPD and VOA datasets.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last updated August 7, 2025.

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

la_reg_file <- file.path(RAW_LOOKUPS_DIR, "Local_Authority_District_(December_2018)_to_NUTS3_to_NUTS2_to_NUTS1_(January_2018)_Lookup_in_United_Kingdom.csv")
ppd_file <- file.path(RAW_DATA_DIR, "ppd_uprn.rdata")

electricity_file <- file.path(RAW_POSTCODE_DIR, "el_postcode.rdata")
gas_file <- file.path(RAW_POSTCODE_DIR, "gas_postcode.rdata")


# Input file from data merging pipeline script
input_file <- file.path(input_dir, paste0("epc_matched_combined_", CCOD_VERSION, ".RData"))

# Load the EPC matched combined dataset
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}
message("Loading EPC matched combined dataset from ", input_file)
load(input_file)

#### CLEAN VARIABLE NAMES ####
# Use janitor::make_clean_names to standardize variable names
message("Cleaning variable names in EPC_matched_combined...")
EPC_matched_combined <- as.data.table(EPC_matched_combined)
EPC_matched_combined <- clean_names(EPC_matched_combined)

# Again same thing with character-baseed UPRNS. 
EPC_matched_combined$uprn <- as.character(EPC_matched_combined$uprn)



#### LOAD AND MERGE PPD DATASET ####
# Load PPD dataset

if (!file.exists(ppd_file)) {
  stop("PPD dataset file does not exist: ", ppd_file)
}
message("Loading PPD dataset from ", ppd_file)
load(ppd_file)

# Assuming the dataset is loaded as 'ppd_uprn' or similar; adjust name if necessary
if (!exists("ppd_uprn")) {
  stop("PPD dataset not found in loaded RData file.")
}
setDT(ppd_uprn)


# Deduplicate by UPRN, keeping the most recent record by date_of_transfer
message("Deduplicating PPD dataset by UPRN, keeping most recent record...")
setorder(ppd_uprn, uprn, -date_of_transfer)
ppd_dedup <- ppd_uprn[,.SD[1], by = uprn][, .(uprn, price, date_of_transfer)]

ppd_dedup[, ppd_year_transfer := year(date_of_transfer)]
EPC_matched_combined[, lodgement_year := year(lodgement_date)]


# Merge with EPC_matched_combined
message("Merging PPD data with EPC_matched_combined...")
setkey(EPC_matched_combined, uprn)
# Again same thing with character-baseed UPRNS. 
ppd_dedup$uprn <- as.character(ppd_dedup$uprn)

EPC_matched_combined <- merge(EPC_matched_combined, ppd_dedup, all.x = TRUE, by = "uprn")

EPC_matched_combined[, ppd_price_sqm := price / total_floor_area]

# Clear some memory
rm(ppd_uprn, ppd_dedup)

#### LOAD AND MERGE VOA DATASET ####
# Load VOA dataset
voa_file <- file.path(RAW_DATA_DIR, "voa_uprn.rdata")

if (!file.exists(voa_file)) {
  stop("VOA dataset file does not exist: ", voa_file)
}
message("Loading VOA dataset from ", voa_file)
load(voa_file)

# Assuming the dataset is loaded as 'voa_uprn' or similar; adjust name if necessary
if (!exists("voa_uprn")) {
  stop("VOA dataset not found in loaded RData file.")
}
setDT(voa_uprn)

# Deduplicate by UPRN and select relevant columns
message("Deduplicating VOA dataset by UPRN and selecting relevant columns...")
voa_dedup <- unique(voa_uprn, by = "uprn")[, .(uprn, tax_band, property_id)]

# Merge with EPC_matched_combined
message("Merging VOA data with EPC_matched_combined...")
# Again same thing with character-baseed UPRNS. 
voa_dedup$uprn <- as.character(voa_dedup$uprn)
setkey(voa_dedup, uprn)

EPC_matched_combined <- merge(EPC_matched_combined, voa_dedup, all.x = TRUE, by = "uprn")

# Clear some memory
rm(voa_uprn, voa_dedup)

#### LOAD AND MERGE LA-Region lookup DATASET ####
# Load LA-Region lookup dataset

if (!file.exists(la_reg_file)) {
  stop("LA-Region lookup file does not exist: ", la_reg_file)
}
message("Loading LA-Region lookup dataset from ", la_reg_file)
la_region <- fread(la_reg_file)
la_region$local_authority <- la_region$LAD18CD

# Merge with EPC_matched_combined
EPC_matched_combined <- merge(EPC_matched_combined, la_region, all.x = TRUE, by = "local_authority")



#### LOAD AND MERGE POSTCODE-LEVEL ENERGY CONSUMPTION DATASET ####
# Load Electricity dataset
if (!file.exists(electricity_file)) {
  stop("Postcode level electricity dataset does not exist: ", electricity_file)
}
message("Loading postcode level electricity dataset from ", electricity_file)
load(electricity_file)

setDT(el_postcode)
setnames(el_postcode, "postcode", "postcode_2")

# "De-duplicate" the dataset by focusing on 2020 energy consumption. Could potentially convert to wide format w/ variables for consumption in 2020, 2017, etc.
el_postcode <- el_postcode[year == "2020"]
el_postcode <- el_postcode[,year := NULL]

setkey(EPC_matched_combined, postcode_2)

EPC_matched_combined <- merge(EPC_matched_combined, el_postcode)

rm(el_postcode)




#### LOAD AND MERGE POSTCODE-LEVEL ENERGY CONSUMPTION DATASET ####
# Load Gas dataset
if (!file.exists(gas_file)) {
  stop("Postcode level gas dataset does not exist: ", gas_file)
}
message("Loading postcode level electricity dataset from ", gas_file)
load(gas_file)

setDT(gas_postcode)
setnames(gas_postcode, "postcode", "postcode_2")

# "De-duplicate" the dataset by focusing on 2020 energy consumption. Could potentially convert to wide format w/ variables for consumption in 2020, 2017, etc.
gas_postcode <- gas_postcode[year == "2020"]
gas_postcode <- gas_postcode[,year := NULL]

setkey(EPC_matched_combined, postcode_2)

EPC_matched_combined <- merge(EPC_matched_combined,gas_postcode)

rm(gas_postcode)




#### SAVE ENHANCED DATASET ####
# Set key to UPRN
setkey(EPC_matched_combined, uprn)

# Save the final enhanced dataset
output_file <- file.path(output_dir, paste0("epc_matched_enhanced_", ccod_version, ".RData"))
save(EPC_matched_combined, file = output_file)
message("Final enhanced dataset saved to ", output_file)
