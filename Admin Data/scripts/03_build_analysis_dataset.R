# Script: 03_build_analysis_dataset.R
# Purpose: Consolidated single-pass pipeline that replaces old scripts
#          02_merge_and_enhance.R and 03_refine_and_parquet.R.
#          Per-LA: reads EPC CSV, joins admin on UPRN, deduplicates, creates
#          key variables, explicit postcode renaming, merges PPD/VOA/ITL/energy
#          (left joins), creates derived features, removes unused columns, and
#          writes .parquet directly to EPC_LA_REFINED_DIR. No intermediate .rds.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: March 13, 2026.

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Set seed for reproducibility
set.seed(03072025)

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))
### Requirements ###
library(data.table)
library(janitor)
library(arrow)


# Inputs ------------------------------------------------------------------
ccod_version <- CCOD_VERSION
ocod_version <- OCOD_VERSION

input_dir <- PROCESSED_DATA_DIR
output_dir <- EPC_LA_REFINED_DIR
UPRN_dt_path <- file.path(RAW_LR_DIR, "LR_UPRN_FULL_JAN_2025.csv")
EPC_archive <- file.path(RAW_EPC_DIR, "all-domestic-certificates.zip")
EPC_path <- file.path(RAW_EPC_DIR, "domestic-EPC")

# Lookup dataset paths
ppd_file <- file.path(RAW_DATA_DIR, "ppd_uprn.rdata")
voa_file <- file.path(RAW_DATA_DIR, "voa_uprn.rdata")
la_itl_file <- file.path(RAW_LOOKUPS_DIR, "LAD_(December_2024)_to_LAU1_to_ITL3_to_ITL2_to_ITL1_(January_2025)_Lookup_in_the_UK.csv")
electricity_file <- file.path(RAW_POSTCODE_DIR, "el_postcode.rdata")
gas_file <- file.path(RAW_POSTCODE_DIR, "gas_postcode.rdata")

# Global stats (pre-computed by 02_compute_global_stats.R)
global_stats_path <- file.path(PROCESSED_DATA_DIR, "global_epc_stats.rds")

# Input file from data sourcing script
input_file <- file.path(input_dir, paste0("combined_admin_data_", ccod_version, ".RData"))


# Load Global Stats -------------------------------------------------------
if (!file.exists(global_stats_path)) {
  stop("Global stats file not found: ", global_stats_path,
       "\n  Run 02_compute_global_stats.R first.")
}
message("Loading pre-computed global EPC statistics from: ", global_stats_path)
global_stats <- readRDS(global_stats_path)
global_half_sd <- global_stats$global_half_sd
epc_lookup     <- global_stats$epc_lookup
message(sprintf("  Global energy efficiency: n=%d, mean=%.2f, sd=%.2f",
                global_stats$global_n, global_stats$global_mean, global_stats$global_sd))


# Load Admin + UPRN Data --------------------------------------------------
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}
message("Loading combined dataset from ", input_file)
load(input_file)

# Load the geovation Title Number-UPRN lookup dataset and set title_number as data.table key
message("Loading UPRN dataset...")
UPRN_dt <- fread(UPRN_dt_path)
setnames(UPRN_dt, c("V1", "V2"), c("title_number","UPRN"))
UPRN_dt[,V3:=NULL]

setkey(UPRN_dt,title_number)

setDT(combined)
setkey(combined,title_number)

# Assign UPRNs to the Admin dataset.
# Titles with multiple UPRNs: Each UPRN is added as a separate entity in the combined dataset.
message("Merging UPRN data with combined dataset...")
combined_expanded <- UPRN_dt[combined, nomatch = 0]

# Clear some memory
rm(combined, UPRN_dt)
tables()
# Convert UPRN to character for merging and set is as key for the data.table
combined_expanded$UPRN <- as.character(combined_expanded$UPRN)
setkey(combined_expanded, UPRN)


# Helper Functions --------------------------------------------------------

# Function to create a cross-sectional dataset with only the most recent EPCs kept.
create_xsection <- function(datatable){
  setDT(datatable)
  setorder(datatable, BUILDING_REFERENCE_NUMBER, LODGEMENT_DATETIME)
  temp_data <- datatable[,.SD[.N], by = BUILDING_REFERENCE_NUMBER]
  return(temp_data)
}


# Key Variable Creation Helper -------------------------------------------
# Category vectors for proprietorship classification
public_sector <- c("County Council", "Local Authority")
for_profit    <- c("Limited Company or Public Limited Company",
                   "Limited Liability Partnership",
                   "Unlimited Company")
non_profit <- c("Co-operative Society (Company)",
                "Co-operative Society (Corporate Body)",
                "Community Benefit Society (Company)",
                "Community Benefit Society (Corporate Body)",
                "Corporate Body",
                "Housing Association Co-operative Society (Company)",
                "Housing Association Co-operative Society (Corporate Body)",
                "Housing Association Community Benefit Society (Company)",
                "Housing Association Community Benefit Society (Corporate Body)",
                "Housing Association Registered Society (Company)",
                "Housing Association Registered Society (Corporate Body)",
                "Housing Association/Society (Company)",
                "Housing Association/Society (Corporate Body)",
                "Industrial and Provident Society (Company)",
                "Industrial and Provident Society (Corporate Body)",
                "Registered Society (Company)",
                "Registered Society (Corporate Body)")

# Tax Haven Lists (IMF)
tax_havens <- c(
  "ANGUILLA", "ANTIGUA AND BARBUDA", "BAHAMAS", "BAHRAIN", "BARBADOS",
  "BELIZE", "BERMUDA", "BRITISH VIRGIN ISLANDS", "CAYMAN ISLANDS", "CYPRUS",
  "GIBRALTAR", "GUERNSEY", "HONG KONG", "ISLE OF MAN", "JERSEY", "LEBANON",
  "LIECHTENSTEIN", "MACAU", "MACAO", "MALTA", "MARSHALL ISLANDS", "MAURITIUS",
  "MONACO", "DUTCH ANTILLES", "PANAMA", "SAMOA", "SEYCHELLES",
  "SINGAPORE", "ST KITTS AND NEVIS", "ST LUCIA", "ST VINCENT AND GRENADINES",
  "TURKS AND CAICOS ISLANDS"
)

british_havens <- c(
  "ANGUILLA", "BERMUDA", "BRITISH VIRGIN ISLANDS", "CAYMAN ISLANDS",
  "GIBRALTAR", "GUERNSEY", "ISLE OF MAN", "JERSEY", "TURKS AND CAICOS ISLANDS"
)

european_havens <- c(
  "CYPRUS", "GIBRALTAR", "GUERNSEY", "ISLE OF MAN", "JERSEY",
  "LIECHTENSTEIN", "MALTA", "MONACO"
)

caribbean_havens <- c(
  "ANGUILLA", "ANTIGUA AND BARBUDA", "BAHAMAS", "BARBADOS", "BELIZE",
  "BRITISH VIRGIN ISLANDS", "CAYMAN ISLANDS", "DUTCH ANTILLES", "PANAMA",
  "ST KITTS AND NEVIS", "ST LUCIA", "ST VINCENT AND GRENADINES",
  "TURKS AND CAICOS ISLANDS"
)

other_havens <- c(
  "BAHRAIN", "HONG KONG", "LEBANON", "MACAO", "MARSHALL ISLANDS",
  "MAURITIUS", "SAMOA", "SEYCHELLES", "SINGAPORE"
)

# Helper: apply all key variable creation to a data.table in-place
create_key_variables <- function(dt) {
  # Outcome variables
  dt[, bad_EPC := CURRENT_ENERGY_RATING %in% c("D", "E", "F", "G")]
  dt[, good_EPC := CURRENT_ENERGY_RATING %in% c("A", "B", "C")]

  dt[is.na(source), source := "Unknown"]
  dt[is.na(tenure), tenure := "Not in OCOD, CCOD"]

  # Concatenation
  dt[, concatenation := paste0(PROPERTY_TYPE, TENURE)]

  # Coarse Proprietorship
  dt[, coarse_proprietorship := fcase(
    proprietorship_category_1 %in% public_sector, "Public Sector",
    proprietorship_category_1 %in% for_profit,    "For-Profit",
    proprietorship_category_1 %in% non_profit,    "Non-Profit/Community Organisations",
    default = NA
  )]

  # Tax haven flags
  dt[, country_incorporated_tax_haven :=
       as.integer(
         country_incorporated_1 %in% tax_havens |
           country_incorporated_2 %in% tax_havens |
           country_incorporated_3 %in% tax_havens |
           country_incorporated_4 %in% tax_havens
       )
  ]

  dt[, country_incorporated_british_haven :=
       as.integer(
         country_incorporated_1 %in% british_havens |
           country_incorporated_2 %in% british_havens |
           country_incorporated_3 %in% british_havens |
           country_incorporated_4 %in% british_havens
       )
  ]

  dt[, country_incorporated_european_haven :=
       as.integer(
         country_incorporated_1 %in% european_havens |
           country_incorporated_2 %in% european_havens |
           country_incorporated_3 %in% european_havens |
           country_incorporated_4 %in% european_havens
       )
  ]

  dt[, country_incorporated_caribbean_haven :=
       as.integer(
         country_incorporated_1 %in% caribbean_havens |
           country_incorporated_2 %in% caribbean_havens |
           country_incorporated_3 %in% caribbean_havens |
           country_incorporated_4 %in% caribbean_havens
       )
  ]

  dt[, country_incorporated_other_haven :=
       as.integer(
         country_incorporated_1 %in% other_havens |
           country_incorporated_2 %in% other_havens |
           country_incorporated_3 %in% other_havens |
           country_incorporated_4 %in% other_havens
       )
  ]

  # Geography and Time
  dt[, postcode_area := sub(" .*", "", POSTCODE)]
  dt[, postcode_sector := sub("^([^ ]+ [A-Z0-9]).*", "\\1", POSTCODE)]
  dt[, lodgement_year := year(LODGEMENT_DATE)]

  invisible(dt)
}


# Extract EPC Certificates -----------------------------------------------
# We do LA sample randomisation here as folders of the EPC dataset
# correspond exactly to local authorities in England and Wales
extractRandomFolders <- function(zip_file,
                                 output_dir,
                                 folder_prefix = "domestic-",
                                 sample_size,
                                 enable_extraction) {
  if (!enable_extraction) {
    message("Extraction is disabled (enable_extraction is FALSE).")
    return(invisible(NULL))
  }

  # Check if the zip file exists
  if (!file.exists(zip_file)) {
    stop("Zip file does not exist: ", zip_file)
  }

  # Create the output directory if it does not exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # List the contents of the ZIP file
  zip_info <- unzip(zip_file, list = TRUE)
  zip_files <- zip_info$Name

  # Filter for entries that start with the given folder prefix
  domestic_entries <- grep(paste0("^", folder_prefix), zip_files, value = TRUE)

  # Extract the top-level folder names (assuming folders are delimited by "/")
  top_folders <- unique(sapply(domestic_entries, function(x) {
    strsplit(x, "/")[[1]][1]
  }))

  # Determine the actual number of folders to sample
  actual_sample_size <- min(sample_size, length(top_folders))
  if (actual_sample_size < sample_size) {
    warning("Only ", actual_sample_size, " folders available for extraction.")
  }

  # Randomly select the folders
  selected_folders <- sample(top_folders, actual_sample_size)
  message("Selected folders: ", paste(selected_folders, collapse = ", "))

  # Identify all files in the zip that belong to the selected folders
  selected_files <- domestic_entries[sapply(domestic_entries, function(x) {
    folder <- strsplit(x, "/")[[1]][1]
    folder %in% selected_folders
  })]

  # Extract the selected files into the output directory
  unzip(zip_file, files = selected_files, exdir = output_dir)
  message("Extraction complete.")
}

# Companion to extractRandomFolders(): extracts ALL domestic LA folders from the EPC zip archive
# when FULL_SAMPLE = TRUE. Uses batches of 20 folders to avoid OS argument-list limits.
extractAllFolders <- function(zip_file,
                              output_dir,
                              folder_prefix = "domestic-",
                              enable_extraction) {
  if (!enable_extraction) {
    message("Extraction is disabled (enable_extraction is FALSE).")
    return(invisible(NULL))
  }

  if (!file.exists(zip_file)) {
    stop("Zip file does not exist: ", zip_file)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  zip_info         <- unzip(zip_file, list = TRUE)
  domestic_entries <- grep(paste0("^", folder_prefix), zip_info$Name, value = TRUE)
  top_folders      <- unique(sapply(domestic_entries, function(x) strsplit(x, "/")[[1]][1]))
  message("Extracting ALL ", length(top_folders), " LA folders from archive...")

  # Batch extraction (20 folders per batch) to avoid OS arg-list limits
  batch_size    <- 20L
  folder_batches <- split(top_folders, ceiling(seq_along(top_folders) / batch_size))

  for (i in seq_along(folder_batches)) {
    batch          <- folder_batches[[i]]
    files_in_batch <- domestic_entries[sapply(domestic_entries, function(x)
      strsplit(x, "/")[[1]][1] %in% batch)]
    unzip(zip_file, files = files_in_batch, exdir = output_dir)
    message(sprintf("  Extracted batch %d / %d (%d folders)", i, length(folder_batches), length(batch)))
  }
  message("Full extraction complete: ", length(top_folders), " LA folders extracted.")
}

# To extract, make sure to set enable_extraction to TRUE, select an appropriate sample of LADs.
# In full-sample mode, all LA folders are extracted; in pilot mode a random sample is used.
if (isTRUE(FULL_SAMPLE)) {
  extractAllFolders(EPC_archive, EPC_path, enable_extraction = ENABLE_EXTRACTION)
} else {
  extractRandomFolders(EPC_archive, EPC_path, sample_size = LA_SAMPLE_SIZE, enable_extraction = ENABLE_EXTRACTION)
}


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


## LA -> ITL Geography Lookup ------------------------------------------------
# Builds a complete local_authority -> ITL1/ITL2/ITL3 mapping with names.
#   1. Dec 2024 LAD -> ITL 2025 (primary, with names)
#   2. Hardcoded predecessor -> successor for dissolved districts
#   3. Deduplication for LAs spanning multiple LAU1 units

if (!file.exists(la_itl_file)) stop("LA-ITL lookup not found: ", la_itl_file)
message("Building LA -> ITL geography lookup...")

dec24 <- fread(la_itl_file)
itl_map <- unique(dec24[, .(local_authority = LAD24CD,
                            ITL1 = ITL125CD, ITL1_name = ITL125NM,
                            ITL2 = ITL225CD, ITL2_name = ITL225NM,
                            ITL3 = ITL325CD, ITL3_name = ITL325NM)])
rm(dec24)

# Deduplicate: some LAs span multiple LAU1 units in different ITL regions
# (e.g., S12000021 North Ayrshire has island + mainland LAU1s).
# Keep first occurrence per LA code.
dup_las <- itl_map[duplicated(local_authority), local_authority]
if (length(dup_las) > 0) {
  message(sprintf("  Deduplicating %d LA codes with multiple ITL mappings: %s",
                  length(dup_las), paste(dup_las, collapse = ", ")))
}
itl_map <- itl_map[!duplicated(local_authority)]

# Predecessor -> successor for LA mergers where the ITL crosswalk is
# ambiguous (one-to-many ITL3) or the old code predates all lookup vintages.
predecessor_map <- data.table(
  old_la = c(
    # Buckinghamshire (merged Apr 2020 -> E06000060)
    "E07000004", "E07000005", "E07000006", "E07000007",
    # North Northamptonshire (merged Apr 2021 -> E06000061)
    "E07000150", "E07000152", "E07000153", "E07000156",
    # West Northamptonshire (merged Apr 2021 -> E06000062)
    "E07000151", "E07000154", "E07000155",
    # Cumberland (merged Apr 2023 -> E06000063): Allerdale, Carlisle, Copeland
    "E07000026", "E07000028", "E07000029",
    # Westmorland and Furness (merged Apr 2023 -> E06000064): Barrow, Eden, South Lakeland
    "E07000027", "E07000030", "E07000031",
    # North Yorkshire (merged Apr 2023 -> E06000065)
    "E07000163", "E07000164", "E07000165", "E07000166",
    "E07000167", "E07000168", "E07000169",
    # Somerset (merged Apr 2023 -> E06000066)
    "E07000187", "E07000188", "E07000189", "E07000246"
  ),
  successor = c(
    rep("E06000060", 4), rep("E06000061", 4), rep("E06000062", 3),
    rep("E06000063", 3), rep("E06000064", 3), rep("E06000065", 7),
    rep("E06000066", 4)
  )
)
predecessor_itl <- merge(predecessor_map,
  itl_map[, .(successor = local_authority, ITL1, ITL1_name, ITL2, ITL2_name, ITL3, ITL3_name)],
  by = "successor", all.x = TRUE)
predecessor_itl <- predecessor_itl[!is.na(ITL1) & !old_la %in% itl_map$local_authority,
  .(local_authority = old_la, ITL1, ITL1_name, ITL2, ITL2_name, ITL3, ITL3_name)]
if (nrow(predecessor_itl) > 0) {
  itl_map <- rbind(itl_map, predecessor_itl)
  message(sprintf("  Recovered %d LA codes via predecessor->successor mapping.", nrow(predecessor_itl)))
}
rm(predecessor_map, predecessor_itl)

setkey(itl_map, local_authority)
message(sprintf("  ITL lookup: %d LA codes -> %d ITL1, %d ITL2, %d ITL3 regions",
                nrow(itl_map), uniqueN(itl_map$ITL1), uniqueN(itl_map$ITL2), uniqueN(itl_map$ITL3)))


## Electricity Consumption --------------------------------------------------
if (!file.exists(electricity_file)) {
  stop("Postcode level electricity dataset does not exist: ", electricity_file)
}
message("Loading postcode level electricity dataset from ", electricity_file)
load(electricity_file)

setDT(el_postcode)
setnames(el_postcode, "postcode", "epc_postcode")
el_postcode <- el_postcode[year == ENERGY_CONSUMPTION_REFERENCE_YEAR]
el_postcode <- el_postcode[, year := NULL]
setkey(el_postcode, epc_postcode)


## Gas Consumption ----------------------------------------------------------
if (!file.exists(gas_file)) {
  stop("Postcode level gas dataset does not exist: ", gas_file)
}
message("Loading postcode level gas dataset from ", gas_file)
load(gas_file)

setDT(gas_postcode)
setnames(gas_postcode, "postcode", "epc_postcode")
gas_postcode <- gas_postcode[year == ENERGY_CONSUMPTION_REFERENCE_YEAR]
gas_postcode <- gas_postcode[, year := NULL]
setkey(gas_postcode, epc_postcode)

message("All lookup datasets loaded.")


# Per-LA Processing Loop ---------------------------------------------------
# Single-pass pipeline: EPC CSV -> join admin -> dedup -> key vars ->
# explicit postcode rename -> merge lookups (left joins) -> derived features ->
# remove unused columns -> write .parquet directly.

epc_folders <- list.files(path = EPC_path, pattern = "^domestic", full.names = TRUE)
n_folders   <- length(epc_folders)

if (n_folders == 0L) {
  stop("No domestic-* folders found in EPC_path: ", EPC_path)
}

if (!("data.table" %in% class(combined_expanded))) {
  stop("admin_dataset must be a data.table.")
}
if (is.null(key(combined_expanded)) || key(combined_expanded) != "UPRN") {
  stop("admin_dataset must have a single key 'UPRN'.")
}

message(sprintf("Starting per-LA processing of %d LA folders...", n_folders))
process_start <- proc.time()
parquet_files_written <- character(0)

for (i in seq_along(epc_folders)) {

  folder    <- epc_folders[[i]]
  la_name   <- basename(folder)
  cert_file <- file.path(folder, "certificates.csv")

  if (!file.exists(cert_file)) {
    warning(sprintf("[%d/%d] certificates.csv not found, skipping: %s", i, n_folders, folder))
    next
  }

  # Crash-resume: if .parquet already exists, skip
  out_path <- file.path(output_dir, paste0(la_name, ".parquet"))
  if (file.exists(out_path)) {
    if (i == 1L || i %% 50L == 0L) {
      message(sprintf("[%d/%d] exists, skipping (crash-resume): %s", i, n_folders, la_name))
    }
    parquet_files_written <- c(parquet_files_written, out_path)
    next
  }

  # Progress report every 10 LAs (and for the first)
  if (i == 1L || i %% 10L == 0L) {
    elapsed <- (proc.time() - process_start)[["elapsed"]]
    message(sprintf("[%d/%d] Processing: %s  (%.0f s elapsed)", i, n_folders, la_name, elapsed))
  }

  # Step 1: Read EPC data for this LA
  epc_data <- fread(cert_file, showProgress = FALSE)

  # Step 2: Convert UPRN to character and set key (matches admin_dataset key type)
  epc_data[, UPRN := as.character(UPRN)]
  setkey(epc_data, UPRN)

  # Step 3: Join - preserves all EPC rows
  joined_data <- combined_expanded[epc_data, on = "UPRN"]

  if (nrow(joined_data) == 0L) {
    message(sprintf("  [%d/%d] No rows after join for %s, skipping.", i, n_folders, la_name))
    rm(epc_data, joined_data)
    gc()
    next
  }

  # Step 4: Deduplicate within this LA (keep most recent EPC per building)
  dt <- create_xsection(joined_data)
  rm(epc_data, joined_data)

  # Step 5: Create key variables (outcome dummies, proprietorship, tax havens, geography)
  create_key_variables(dt)

  # Step 6: Resolve postcode column collision BEFORE clean_names().
  # The admin dataset (combined_expanded) has lowercase 'postcode' from script 01.
  # The EPC data has uppercase 'POSTCODE'.
  # Rename admin's to 'admin_postcode' so clean_names() doesn't create '_2' suffixes.
  if ("postcode" %in% names(dt) && "POSTCODE" %in% names(dt)) {
    setnames(dt, "postcode", "admin_postcode")
  }

  # Step 7: Clean variable names (resolves EPC UPPERCASE vs admin lowercase)
  dt <- clean_names(dt)

  # Rename EPC postcode to the canonical name used by energy data merges
  if ("postcode" %in% names(dt)) {
    setnames(dt, "postcode", "epc_postcode")
  }

  # Step 8: Ensure UPRN key is character type for merge consistency
  dt$uprn <- as.character(dt$uprn)

  # Step 9: Merge PPD
  setkey(dt, uprn)
  dt <- merge(dt, ppd_dedup, all.x = TRUE, by = "uprn")
  dt[, ppd_price_sqm := price / total_floor_area]

  # Ensure lodgement_year exists
  if (!"lodgement_year" %in% names(dt)) {
    dt[, lodgement_year := year(lodgement_date)]
  }

  # Step 10: Merge VOA
  dt <- merge(dt, voa_dedup, all.x = TRUE, by = "uprn")

  # Step 11: Merge ITL geography
  dt <- merge(dt, itl_map, all.x = TRUE, by = "local_authority")

  # Step 12: Merge electricity (left join — NAs where postcode not in energy data)
  setkey(dt, epc_postcode)
  dt <- merge(dt, el_postcode, all.x = TRUE, by = "epc_postcode")

  # Step 13: Merge gas (left join)
  dt <- merge(dt, gas_postcode, all.x = TRUE, by = "epc_postcode")

  # Set key to UPRN
  setkey(dt, uprn)

  # --- Derived Features (from old 03_refine_and_parquet.R) ---

  # Energy Efficiency Dummies: Binary indicators for poor or very poor ratings
  dt[!is.na(roof_energy_eff), roof_energy_eff_dum := as.numeric(roof_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(walls_energy_eff), walls_energy_eff_dum := as.numeric(walls_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(hot_water_energy_eff), hot_water_energy_eff_dum := as.numeric(hot_water_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(mainheat_energy_eff), mainheat_energy_eff_dum := as.numeric(mainheat_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(windows_energy_eff), windows_energy_eff_dum := as.numeric(windows_energy_eff %in% c("Poor", "Very Poor"))]

  # Normalize multi_glaze_proportion
  dt[, multi_glaze_proportion := multi_glaze_proportion / 100]

  # Energy consumption variables
  dt[, energy_consumption_current_property   := energy_consumption_current   * total_floor_area]
  dt[, energy_consumption_potential_property := energy_consumption_potential * total_floor_area]

  # Gap variables
  dt[, energy_consumption_gap          := energy_consumption_potential - energy_consumption_current]
  dt[, energy_consumption_gap_property := energy_consumption_potential_property - energy_consumption_current_property]
  dt[, energy_efficiency_potential_gap := potential_energy_efficiency - current_energy_efficiency]

  # Gap between current efficiency and borderline for EPC < C (<=68 numeric)
  dt[, energy_efficiency_bad_epc_gap := current_energy_efficiency - 68]

  # Gap between current efficiency and borderline for next worst EPC rating
  dt[, energy_efficiency_worse_epc_gap := fcase(
    is.na(current_energy_efficiency), NA_real_,
    current_energy_efficiency >= 92, current_energy_efficiency - 91,
    current_energy_efficiency >= 81, current_energy_efficiency - 80,
    current_energy_efficiency >= 69, current_energy_efficiency - 68,
    current_energy_efficiency >= 55, current_energy_efficiency - 54,
    current_energy_efficiency >= 39, current_energy_efficiency - 38,
    current_energy_efficiency >= 21, current_energy_efficiency - 20,
    default = 0
  )]

  ## Borderline variables

  # borderline_good_epc: just above C threshold (69) using global SD
  dt[, borderline_good_epc := fcase(
    current_energy_efficiency > 69 &
      current_energy_efficiency <= 69 + global_half_sd, 1,
    is.na(current_energy_efficiency), NA_real_,
    default = 0
  )]

  # borderline_better_epc: just above each band's lower cutoff using local bandwidth
  dt <- merge(dt, epc_lookup, by = "current_energy_rating", all.x = TRUE)

  dt[, borderline_better_epc := fcase(
    current_energy_efficiency > lower_cutoff &
      current_energy_efficiency <= lower_cutoff + half_sd, 1,
    is.na(current_energy_efficiency), NA_real_,
    default = 0
  )]

  ## Bad-EPC indicators at the two regulatory bounds (NA-safe)
  # bad_epc_c (main definition) = below C, the incoming MEES bound (cutoff 69);
  # bad_epc_e = below E, the present regulatory minimum (cutoff 39).
  dt[, bad_epc_c := fcase(is.na(current_energy_efficiency), NA_real_,
                          current_energy_efficiency < 69, 1, default = 0)]
  dt[, bad_epc_e := fcase(is.na(current_energy_efficiency), NA_real_,
                          current_energy_efficiency < 39, 1, default = 0)]

  # Efficiency gap to each regulatory threshold (alias of energy_efficiency_bad_epc_gap for C)
  dt[, energy_efficiency_c_gap := current_energy_efficiency - 68]
  dt[, energy_efficiency_e_gap := current_energy_efficiency - 38]

  # Bunching just above each regulatory cutoff (global SD bandwidth; alias of borderline_good_epc for C)
  dt[, borderline_good_epc_c := borderline_good_epc]
  dt[, borderline_good_epc_e := fcase(
    current_energy_efficiency > 39 &
      current_energy_efficiency <= 39 + global_half_sd, 1,
    is.na(current_energy_efficiency), NA_real_,
    default = 0
  )]

  # Clean up helper columns
  dt[, c("lower_cutoff", "half_sd") := NULL]

  ## Remove unused columns
  description_cols <- grep("_description", names(dt), value = TRUE)
  unused_specific <- c("fixed_lighting_outlets_count", "low_energy_fixed_light_count")
  cols_to_remove <- intersect(c(description_cols, unused_specific), names(dt))
  if (length(cols_to_remove) > 0) {
    dt[, (cols_to_remove) := NULL]
  }

  # Write Parquet directly (no intermediate .rds)
  arrow::write_parquet(dt, out_path)
  parquet_files_written <- c(parquet_files_written, out_path)
  rm(dt)
  gc()
}

n_written <- length(parquet_files_written)
elapsed_total <- (proc.time() - process_start)[["elapsed"]]
message(sprintf("Per-LA processing complete: %d / %d LAs produced matches (%.0f s total).",
                n_written, n_folders, elapsed_total))

if (n_written == 0L) {
  stop("No LA produced any matched rows. Check EPC_path and admin_dataset.")
}

# Free lookup data
rm(combined_expanded, ppd_dedup, voa_dedup, itl_map, el_postcode, gas_postcode)
gc()

message(sprintf("Analysis dataset built: %d Parquet files in %s", n_written, output_dir))
