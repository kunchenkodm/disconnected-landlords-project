# Script: 03_feature_enhancement.R
# Purpose:  Enhance per-LA merged datasets by cleaning variable names
#           and merging with PPD, VOA, energy consumption, and geography lookups.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last updated Febuary 20, 2026.

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()
# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(janitor)

# SETUP: INPUTS REQUIRED  --------------------------------------------------
# Configuration section using global variables from 00_setup.R
ccod_version <- CCOD_VERSION
input_dir <- EPC_LA_MERGED_DIR
output_dir <- EPC_LA_ENHANCED_DIR

la_reg_file <- file.path(RAW_LOOKUPS_DIR, "Local_Authority_District_(December_2018)_to_NUTS3_to_NUTS2_to_NUTS1_(January_2018)_Lookup_in_United_Kingdom.csv")
ppd_file <- file.path(RAW_DATA_DIR, "ppd_uprn.rdata")

electricity_file <- file.path(RAW_POSTCODE_DIR, "el_postcode.rdata")
gas_file <- file.path(RAW_POSTCODE_DIR, "gas_postcode.rdata")


# Load Lookup Datasets (once, kept in memory) ------------------------------

## PPD (Price Paid Data) ---------------------------------------------------
if (!file.exists(ppd_file)) {
  stop("PPD dataset file does not exist: ", ppd_file)
}
message("Loading PPD dataset from ", ppd_file)
load(ppd_file)

if (!exists("ppd_uprn")) {
  stop("PPD dataset not found in loaded RData file.")
}
setDT(ppd_uprn)

# Deduplicate by UPRN, keeping the most recent record by date_of_transfer
message("Deduplicating PPD dataset by UPRN, keeping most recent record...")
setorder(ppd_uprn, uprn, -date_of_transfer)
ppd_dedup <- ppd_uprn[,.SD[1], by = uprn][, .(uprn, price, date_of_transfer)]
ppd_dedup[, ppd_year_transfer := year(date_of_transfer)]
ppd_dedup$uprn <- as.character(ppd_dedup$uprn)
setkey(ppd_dedup, uprn)
rm(ppd_uprn)


## VOA (Council Tax) -------------------------------------------------------
voa_file <- file.path(RAW_DATA_DIR, "voa_uprn.rdata")

if (!file.exists(voa_file)) {
  stop("VOA dataset file does not exist: ", voa_file)
}
message("Loading VOA dataset from ", voa_file)
load(voa_file)

if (!exists("voa_uprn")) {
  stop("VOA dataset not found in loaded RData file.")
}
setDT(voa_uprn)

# Deduplicate by UPRN and select relevant columns
message("Deduplicating VOA dataset by UPRN and selecting relevant columns...")
voa_dedup <- unique(voa_uprn, by = "uprn")[, .(uprn, tax_band, property_id)]
voa_dedup$uprn <- as.character(voa_dedup$uprn)
setkey(voa_dedup, uprn)
rm(voa_uprn)


## LA-Region Geography Lookup -----------------------------------------------
if (!file.exists(la_reg_file)) {
  stop("LA-Region lookup file does not exist: ", la_reg_file)
}
message("Loading LA-Region lookup dataset from ", la_reg_file)
la_region <- fread(la_reg_file)
la_region$local_authority <- la_region$LAD18CD


## Electricity Consumption --------------------------------------------------
if (!file.exists(electricity_file)) {
  stop("Postcode level electricity dataset does not exist: ", electricity_file)
}
message("Loading postcode level electricity dataset from ", electricity_file)
load(electricity_file)

setDT(el_postcode)
setnames(el_postcode, "postcode", "postcode_2")
el_postcode <- el_postcode[year == ENERGY_CONSUMPTION_REFERENCE_YEAR]
el_postcode <- el_postcode[, year := NULL]
setkey(el_postcode, postcode_2)


## Gas Consumption ----------------------------------------------------------
if (!file.exists(gas_file)) {
  stop("Postcode level gas dataset does not exist: ", gas_file)
}
message("Loading postcode level gas dataset from ", gas_file)
load(gas_file)

setDT(gas_postcode)
setnames(gas_postcode, "postcode", "postcode_2")
gas_postcode <- gas_postcode[year == ENERGY_CONSUMPTION_REFERENCE_YEAR]
gas_postcode <- gas_postcode[, year := NULL]
setkey(gas_postcode, postcode_2)


message("All lookup datasets loaded. Starting per-LA enhancement loop...")


# Per-LA Enhancement Loop --------------------------------------------------
la_files <- list.files(input_dir, pattern = "\\.rds$", full.names = TRUE)
n_files <- length(la_files)

if (n_files == 0L) {
  stop("No per-LA RDS files found in: ", input_dir)
}

process_start <- proc.time()

for (i in seq_along(la_files)) {
  la_name <- tools::file_path_sans_ext(basename(la_files[i]))
  out_path <- file.path(output_dir, paste0(la_name, ".rds"))

  # Crash-resume: skip if output already exists
  if (file.exists(out_path)) {
    if (i == 1L || i %% 50L == 0L) {
      message(sprintf("[%d/%d] exists, skipping (crash-resume): %s", i, n_files, la_name))
    }
    next
  }

  # Progress report
  if (i == 1L || i %% 10L == 0L) {
    elapsed <- (proc.time() - process_start)[["elapsed"]]
    message(sprintf("[%d/%d] Enhancing: %s  (%.0f s elapsed)", i, n_files, la_name, elapsed))
  }

  dt <- readRDS(la_files[i])
  setDT(dt)

  # Clean variable names
  dt <- clean_names(dt)

  # Ensure UPRN key is character type for merge consistency
  dt$uprn <- as.character(dt$uprn)

  # Merge PPD
  setkey(dt, uprn)
  dt <- merge(dt, ppd_dedup, all.x = TRUE, by = "uprn")
  dt[, ppd_price_sqm := price / total_floor_area]

  # Ensure lodgement_year exists (may have been created as lodgement_year in script 02)
  if (!"lodgement_year" %in% names(dt)) {
    dt[, lodgement_year := year(lodgement_date)]
  }

  # Merge VOA
  dt <- merge(dt, voa_dedup, all.x = TRUE, by = "uprn")

  # Merge geography
  dt <- merge(dt, la_region, all.x = TRUE, by = "local_authority")

  # Merge electricity (inner join — drops postcodes without energy data)
  setkey(dt, postcode_2)
  dt <- merge(dt, el_postcode, by = "postcode_2")

  # Merge gas (inner join)
  dt <- merge(dt, gas_postcode, by = "postcode_2")

  # Set key to UPRN
  setkey(dt, uprn)

  # Save enhanced per-LA dataset
  saveRDS(dt, out_path)
  rm(dt)
  gc()
}

elapsed_total <- (proc.time() - process_start)[["elapsed"]]
n_output <- length(list.files(output_dir, pattern = "\\.rds$"))
message(sprintf("Per-LA enhancement complete: %d files in %.0f s. Output: %s",
                n_output, elapsed_total, output_dir))
