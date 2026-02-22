# Script: 02_data_merging_pipeline.R
# Purpose:  Load the combined admin dataset,
#           extract local authorities from the EPC dataset,
#           perform UPRN and EPC matching, clean and process data,
#           and save per-LA matched datasets to EPC_LA_MERGED_DIR.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025, Last updated Febuary 20, 2026

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Set seed for reproducibility
set.seed(03072025)

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))
### Requirements ###
library(data.table)
library(janitor)

# SETUP: INPUTS REQUIRED  --------------------------------------------------
# Configuration section for user customization (using global variables from 00_setup.R)
ccod_version <- CCOD_VERSION
ocod_version <- OCOD_VERSION

input_dir <- PROCESSED_DATA_DIR
output_dir <- EPC_LA_MERGED_DIR
UPRN_dt_path <- file.path(RAW_LR_DIR, "LR_UPRN_FULL_JAN_2025.csv")
EPC_archive <- file.path(RAW_EPC_DIR, "all-domestic-certificates.zip")
EPC_path <- file.path(RAW_EPC_DIR, "domestic-EPC")

# Input file from data sourcing script
input_file <- file.path(input_dir, paste0("combined_admin_data_", ccod_version, ".RData"))

# Load the combined dataset
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}
message("Loading combined dataset from ", input_file)
load(input_file)


# Lookup UPRN of Land Registry Records ------------------------------------
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

# Function to create a cross-sectional dataset with only the most recent EPCs kept.
# Defined here (before all call sites) so it can be reused inside the per-LA loop.
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


# Extract EPC Certificates from their archives, create dataset ------------
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


# Merge EPC and administrative datasets (per-LA) --------------------------
# Processes one LA folder at a time: read EPC data, join with admin on UPRN,
# deduplicate (keep most recent EPC per building), create key variables,
# and save as an individual RDS file. Stays within 16GB RAM budget.

process_epc_per_la <- function(EPC_path, admin_dataset, la_output_dir) {

  # Validation
  if (!("data.table" %in% class(admin_dataset))) {
    stop("admin_dataset must be a data.table.")
  }
  if (is.null(key(admin_dataset)) || length(key(admin_dataset)) != 1 || key(admin_dataset) != "UPRN") {
    stop("admin_dataset must have a single key 'UPRN'.")
  }
  if (!dir.exists(la_output_dir)) {
    dir.create(la_output_dir, recursive = TRUE)
  }

  epc_folders <- list.files(path = EPC_path, pattern = "^domestic", full.names = TRUE)
  n_folders   <- length(epc_folders)

  if (n_folders == 0L) {
    stop("No domestic-* folders found in EPC_path: ", EPC_path)
  }

  message(sprintf("Starting per-LA processing of %d LA folders...", n_folders))
  process_start <- proc.time()

  rds_files_written <- character(0)

  for (i in seq_along(epc_folders)) {

    folder    <- epc_folders[[i]]
    la_name   <- basename(folder)
    cert_file <- file.path(folder, "certificates.csv")

    if (!file.exists(cert_file)) {
      warning(sprintf("[%d/%d] certificates.csv not found, skipping: %s", i, n_folders, folder))
      next
    }

    # Crash-resume: skip if this LA's RDS already exists from a previous interrupted run
    rds_path <- file.path(la_output_dir, paste0(la_name, ".rds"))
    if (file.exists(rds_path)) {
      message(sprintf("[%d/%d] RDS exists, skipping (crash-resume): %s", i, n_folders, la_name))
      rds_files_written <- c(rds_files_written, rds_path)
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

    # Step 3: Join - same semantics as original (nomatch = NA, preserves all EPC rows)
    joined_data <- admin_dataset[epc_data, on = "UPRN"]

    if (nrow(joined_data) == 0L) {
      message(sprintf("  [%d/%d] No rows after join for %s, skipping.", i, n_folders, la_name))
      rm(epc_data, joined_data)
      gc()
      next
    }

    # Step 4: Deduplicate within this LA (keep most recent EPC per building)
    la_xsection <- create_xsection(joined_data)

    # Step 5: Create key variables (outcome dummies, proprietorship, tax havens, geography)
    create_key_variables(la_xsection)

    # Step 6: Write per-LA result and release memory
    saveRDS(la_xsection, file = rds_path)
    rds_files_written <- c(rds_files_written, rds_path)
    rm(epc_data, joined_data, la_xsection)
    gc()
  }

  n_written <- length(rds_files_written)
  elapsed_total <- (proc.time() - process_start)[["elapsed"]]
  message(sprintf("Per-LA processing complete: %d / %d LAs produced matches (%.0f s total).",
                  n_written, n_folders, elapsed_total))

  if (n_written == 0L) {
    stop("No LA produced any matched rows. Check EPC_path and admin_dataset.")
  }

  return(rds_files_written)
}


# Run the per-LA EPC processing pipeline ----------------------------------
message("Running per-LA EPC processing pipeline...")
rds_paths <- process_epc_per_la(EPC_path, combined_expanded, output_dir)
message(sprintf("Per-LA merged datasets saved to: %s (%d files)", output_dir, length(rds_paths)))
