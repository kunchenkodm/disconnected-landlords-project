# Script: 02_data_merging_pipeline.R
# Purpose:  Load the combined admin dataset, 
#           extract a random sample of local authorities from the EPC dataset
#           perform UPRN and EPC matching, clean and process data, 
#           and save the final matched dataset.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025, Last updated Febuary 9, 2026

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
output_dir <- PROCESSED_DATA_DIR
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
# Defined here (before all call sites) so it can be reused inside convert_epc_datasets_chunked().
create_xsection <- function(datatable){
  setDT(datatable)
  setorder(datatable, BUILDING_REFERENCE_NUMBER, LODGEMENT_DATETIME)
  temp_data <- datatable[,.SD[.N], by = BUILDING_REFERENCE_NUMBER]
  return(temp_data)
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


# Merge EPC and administrative datasets -----------------------------------
# Function to convert and join all EPC datasets in a folder using the combined admin dataset and regional sub-datasets of EPC
convert_epc_datasets <- function(EPC_path, admin_dataset) {
  # Check if admin_dataset is a data.table
  if (!("data.table" %in% class(admin_dataset))) {
    stop("admin_dataset must be a data.table.")
  }
  
  # Check that the key for admin_dataset is set and is "UPRN"
  if (is.null(key(admin_dataset)) || length(key(admin_dataset)) != 1 || key(admin_dataset) != "UPRN") {
    stop("admin_dataset must have a single key 'UPRN'.")
  }
  
  # List all subfolders in EPC_path whose names start with "domestic"
  epc_folders <- list.files(path = EPC_path, pattern = "^domestic", full.names = TRUE)
  
  # Process each folder: read certificates.csv, convert UPRN to character, set key, and perform join.
  epc_joined_list <- lapply(epc_folders, function(folder) {
    cert_file <- file.path(folder, "certificates.csv")
    if (file.exists(cert_file)) {
      message("Processing folder: ", folder)
      
      # Read the certificates file
      epc_data <- fread(cert_file)
      
      # Convert UPRN to character [for some reason the only way to get it to work] and set it as key
      epc_data[, UPRN := as.character(UPRN)]
      setkey(epc_data, UPRN)
      
      # Perform an inner join with admin_dataset using UPRN as key
      joined_data <- admin_dataset[epc_data, on = "UPRN"]
      return(joined_data)
    } else {
      warning("File certificates.csv not found in folder: ", folder)
      return(NULL)
    }
  })
  
  # Remove any NULL entries (in case some folders did not have certificates.csv)
  epc_joined_list <- Filter(Negate(is.null), epc_joined_list)
  
  # Combine datasets into one data.table
  EPC_matched <- rbindlist(epc_joined_list)
  return(EPC_matched)
}

# Chunked EPC processor for full-sample mode (FULL_SAMPLE = TRUE).
# Processes one LA folder at a time to stay within a 16GB RAM budget when
# running the full ~400-LA England & Wales dataset.
#
# Per-LA workflow:
#   1. Read certificates.csv
#   2. Inner-join with admin_dataset on UPRN
#   3. Apply create_xsection() (keep only the most recent EPC per building)
#   4. Write deduplicated result to an individual RDS file in temp_dir
#   5. Remove LA objects and call gc() to release memory
#
# After the loop: combine all per-LA RDS files into a single data.table,
# delete the temp files, and return the result.
convert_epc_datasets_chunked <- function(EPC_path, admin_dataset, temp_dir) {
  
  # Validation
  if (!("data.table" %in% class(admin_dataset))) {
    stop("admin_dataset must be a data.table.")
  }
  if (is.null(key(admin_dataset)) || length(key(admin_dataset)) != 1 || key(admin_dataset) != "UPRN") {
    stop("admin_dataset must have a single key 'UPRN'.")
  }
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  epc_folders <- list.files(path = EPC_path, pattern = "^domestic", full.names = TRUE)
  n_folders   <- length(epc_folders)
  
  if (n_folders == 0L) {
    stop("No domestic-* folders found in EPC_path: ", EPC_path)
  }
  
  message(sprintf("Starting chunked processing of %d LA folders...", n_folders))
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
    rds_path <- file.path(temp_dir, paste0(la_name, ".rds"))
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
    
    # Step 5: Write per-LA result to temp file and release memory
    saveRDS(la_xsection, file = rds_path)
    rds_files_written <- c(rds_files_written, rds_path)
    rm(epc_data, joined_data, la_xsection)
    gc()
  }
  
  n_written <- length(rds_files_written)
  message(sprintf("Per-LA loop complete: %d / %d LAs produced matches.", n_written, n_folders))
  
  if (n_written == 0L) {
    stop("No LA produced any matched rows. Check EPC_path and admin_dataset.")
  }
  
  # Combine per-LA RDS files into a single data.table
  message("Combining per-LA RDS files into final dataset...")
  combine_start <- proc.time()
  
  la_results <- vector("list", n_written)
  for (j in seq_along(rds_files_written)) {
    la_results[[j]] <- readRDS(rds_files_written[[j]])
    if (j %% 50L == 0L || j == n_written) {
      elapsed <- (proc.time() - combine_start)[["elapsed"]]
      message(sprintf("  Read %d / %d RDS files (%.0f s)", j, n_written, elapsed))
    }
  }
  
  EPC_matched <- rbindlist(la_results, use.names = TRUE, fill = TRUE)
  rm(la_results)
  gc()
  
  # Clean up temp files now that the combined dataset is in memory
  message("Cleaning up per-LA temp RDS files...")
  deleted <- file.remove(rds_files_written)
  if (sum(deleted) < n_written) {
    warning(sprintf("Could not delete %d temp files in: %s", n_written - sum(deleted), temp_dir))
  } else {
    message(sprintf("Removed %d temp RDS files.", sum(deleted)))
  }
  
  message(sprintf("Combined dataset: %d rows, %d columns.", nrow(EPC_matched), ncol(EPC_matched)))
  return(EPC_matched)
}

# Loads or creates the matching dataset
# Cache path: full-sample uses RData (faster I/O); pilot mode keeps the original CSV path.
if (isTRUE(FULL_SAMPLE)) {
  EPC_matched_all_cache <- file.path(PROCESSED_DATA_DIR,
                                     paste0("epc_matched_xsection_full_", ccod_version, ".RData"))
} else {
  EPC_matched_all_cache <- file.path(RAW_EPC_DIR, "epc_matched-random_sample.csv")
}

if (file.exists(EPC_matched_all_cache)) {
  message("Matched dataset found. Loading from disk: ", EPC_matched_all_cache)
  if (isTRUE(FULL_SAMPLE)) {
    load(EPC_matched_all_cache)  # loads EPC_matched_all
  } else {
    EPC_matched_all <- fread(EPC_matched_all_cache)
  }
} else {
  if (isTRUE(FULL_SAMPLE)) {
    # Chunked path: per-LA deduplication is applied inside the loop
    message("Full-sample mode: running chunked EPC processor...")
    EPC_matched_all <- convert_epc_datasets_chunked(EPC_path, combined_expanded, EPC_TEMP_DIR)
    save(EPC_matched_all, file = EPC_matched_all_cache)
    message("Full-sample matched dataset cached to: ", EPC_matched_all_cache)
  } else {
    # Pilot path: original accumulate-all-then-combine approach
    message("Pilot mode: creating matched dataset...")
    EPC_matched_all <- convert_epc_datasets(EPC_path, combined_expanded)
  }
}

# Cross-section creation
# Full-sample mode: create_xsection() was already applied per-LA inside
# convert_epc_datasets_chunked(), so EPC_matched_all is already cross-sectional.
# Pilot mode: apply the original split-by-tenure -> deduplicate -> recombine logic.
if (!isTRUE(FULL_SAMPLE)) {
  message("Creating cross-sectional datasets (pilot mode)...")
  
  # Splits the dataset into the freehold and leasehold parts.
  setkey(EPC_matched_all, tenure)
  EPC_matched_lease <- EPC_matched_all["Leasehold"]
  EPC_matched_free  <- EPC_matched_all["Freehold"]
  EPC_matched_NA    <- EPC_matched_all[is.na(tenure)]
  
  EPC_matched_lease_clean <- create_xsection(EPC_matched_lease)
  EPC_matched_free_clean  <- create_xsection(EPC_matched_free)
  EPC_matched_NA_clean    <- create_xsection(EPC_matched_NA)
  
  # Duplicate check
  EPC_matched_free_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_free_clean$BUILDING_REFERENCE_NUMBER)]
  EPC_matched_lease_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_lease_clean$BUILDING_REFERENCE_NUMBER)]
  EPC_matched_NA_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_NA_clean$BUILDING_REFERENCE_NUMBER)]
  
  # Re-merge tenure datasets
  EPC_matched_combined <- rbind(EPC_matched_free_clean, EPC_matched_lease_clean, EPC_matched_NA_clean)
  # EPC_matched_combined <- rbind(EPC_matched_free_clean, EPC_matched_NA_clean)
  rm(EPC_matched_lease, EPC_matched_free, EPC_matched_NA,
     EPC_matched_lease_clean, EPC_matched_free_clean, EPC_matched_NA_clean)
} else {
  # Full-sample mode: per-LA deduplication already applied; assign directly
  message("Full-sample mode: cross-section already applied per-LA.")
  EPC_matched_combined <- EPC_matched_all
}
rm(EPC_matched_all)
gc()

EPC_matched_combined[, has_duplicates := .N > 1, by = BUILDING_REFERENCE_NUMBER]

setkey(EPC_matched_combined, BUILDING_REFERENCE_NUMBER)




# Creation of key variables -----------------------------------------------
## Outcome variables  -----------------------------------------------------
EPC_matched_combined[, bad_EPC := CURRENT_ENERGY_RATING %in% c("D", "E", "F", "G")]
EPC_matched_combined[, good_EPC := CURRENT_ENERGY_RATING %in% c("A", "B", "C")]

EPC_matched_combined[is.na(source), source := "Unknown"]
EPC_matched_combined[is.na(tenure), tenure := "Not in OCOD, CCOD"]

# Concatenation
EPC_matched_combined[, concatenation := paste0(PROPERTY_TYPE,TENURE)]

## Coarse Proprietorship  -------------------------------------------------
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

EPC_matched_combined[, coarse_proprietorship := fcase(
  proprietorship_category_1 %in% public_sector, "Public Sector",
  proprietorship_category_1 %in% for_profit,    "For-Profit",
  proprietorship_category_1 %in% non_profit,    "Non-Profit/Community Organisations",
  default = NA
)]

## Tax Haven List (IMF)  -------------------------------------------------
tax_havens <- c(
  "ANGUILLA", "ANTIGUA AND BARBUDA", "BAHAMAS", "BAHRAIN", "BARBADOS",
  "BELIZE", "BERMUDA", "BRITISH VIRGIN ISLANDS", "CAYMAN ISLANDS", "CYPRUS",
  "GIBRALTAR", "GUERNSEY", "HONG KONG", "ISLE OF MAN", "JERSEY", "LEBANON",
  "LIECHTENSTEIN", "MACAU", "MACAO", "MALTA", "MARSHALL ISLANDS", "MAURITIUS",
  "MONACO", "DUTCH ANTILLES", "PANAMA", "SAMOA", "SEYCHELLES",
  "SINGAPORE", "ST KITTS AND NEVIS", "ST LUCIA", "ST VINCENT AND GRENADINES",
  "TURKS AND CAICOS ISLANDS"
)

### Experimental: Disaggregated tax havens ---------------------------------
# British Tax Havens
# Many of these are British Overseas Territories or Crown Dependencies.
british_havens <- c(
  "ANGUILLA", "BERMUDA", "BRITISH VIRGIN ISLANDS", "CAYMAN ISLANDS", 
  "GIBRALTAR", "GUERNSEY", "ISLE OF MAN", "JERSEY", "TURKS AND CAICOS ISLANDS"
)

# European Tax Havens
# This group includes sovereign states and dependencies within Europe.
european_havens <- c(
  "CYPRUS", "GIBRALTAR", "GUERNSEY", "ISLE OF MAN", "JERSEY", 
  "LIECHTENSTEIN", "MALTA", "MONACO"
)

# Caribbean Tax Havens
# This list comprises nations and territories in the Caribbean region.
caribbean_havens <- c(
  "ANGUILLA", "ANTIGUA AND BARBUDA", "BAHAMAS", "BARBADOS", "BELIZE", 
  "BRITISH VIRGIN ISLANDS", "CAYMAN ISLANDS", "DUTCH ANTILLES", "PANAMA", 
  "ST KITTS AND NEVIS", "ST LUCIA", "ST VINCENT AND GRENADINES", 
  "TURKS AND CAICOS ISLANDS"
)

# Other Tax Havens
# The remaining locations outside of the other specified regions.
# Note: "MACAU" and "MACAO" from the original list refer to the same entity; 
# only one is included here to avoid duplication.
other_havens <- c(
  "BAHRAIN", "HONG KONG", "LEBANON", "MACAO", "MARSHALL ISLANDS", 
  "MAURITIUS", "SAMOA", "SEYCHELLES", "SINGAPORE"
)






EPC_matched_combined[, country_incorporated_tax_haven :=
                       as.integer(
                         country_incorporated_1 %in% tax_havens |
                           country_incorporated_2 %in% tax_havens |
                           country_incorporated_3 %in% tax_havens |
                           country_incorporated_4 %in% tax_havens
                       )
]

EPC_matched_combined[, country_incorporated_british_haven :=
                       as.integer(
                         country_incorporated_1 %in% british_havens |
                           country_incorporated_2 %in% british_havens |
                           country_incorporated_3 %in% british_havens |
                           country_incorporated_4 %in% british_havens
                       )
]

EPC_matched_combined[, country_incorporated_european_haven :=
                       as.integer(
                         country_incorporated_1 %in% european_havens |
                           country_incorporated_2 %in% european_havens |
                           country_incorporated_3 %in% european_havens |
                           country_incorporated_4 %in% european_havens
                       )
]

EPC_matched_combined[, country_incorporated_caribbean_haven :=
                       as.integer(
                         country_incorporated_1 %in% caribbean_havens |
                           country_incorporated_2 %in% caribbean_havens |
                           country_incorporated_3 %in% caribbean_havens |
                           country_incorporated_4 %in% caribbean_havens
                       )
]

EPC_matched_combined[, country_incorporated_other_haven :=
                       as.integer(
                         country_incorporated_1 %in% other_havens |
                           country_incorporated_2 %in% other_havens |
                           country_incorporated_3 %in% other_havens |
                           country_incorporated_4 %in% other_havens
                       )
]

## Geography and Time  -------------------------------------------------

EPC_matched_combined[, postcode_area := sub(" .*", "", POSTCODE)]
EPC_matched_combined[, postcode_sector := sub("^([^ ]+ [A-Z0-9]).*", "\\1", POSTCODE)]

EPC_matched_combined[, lodgement_year := year(LODGEMENT_DATE)]

# Saving the dataset ------------------------------------------------------
output_file <- file.path(output_dir, paste0("epc_matched_combined_", ccod_version, ".RData"))
save(EPC_matched_combined, file = output_file)
message("Final combined dataset saved to ", output_file)



