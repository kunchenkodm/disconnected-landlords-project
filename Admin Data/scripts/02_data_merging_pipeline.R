# Script: 02_data_merging_pipeline.R
# Purpose: Load the combined admin dataset, perform UPRN and EPC matching, clean and process data, and save the final matched dataset.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025, Last updated August 15, 2025

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Set seed for reproducibility
set.seed(03072025)

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))
### Requirements ###
library(data.table)
library(janitor)

#### SETUP: INPUTS REQUIRED ####
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

#### UPRN LOOKUP ####
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
# Convert UPRN to character [for some reason the only way to get it to work] and set is as key for the data.table
combined_expanded$UPRN <- as.character(combined_expanded$UPRN)
setkey(combined_expanded, UPRN)

#### EXTRACT CERTIFICATES FROM ARCHIVE (RANDOM SAMPLE VERSION) ####
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

# To extract, make sure to set enable_extraction to TRUE, select an appropriate sample of LADs. 
extractRandomFolders(EPC_archive, EPC_path, sample_size = SAMPLE_SIZE, enable_extraction = ENABLE_EXTRACTION)
#### EPC MATCHING ####
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

# Loads or creates the matching dataset
EPC_matched_all_dir <- file.path(RAW_EPC_DIR, "epc_matched-random_sample.csv")
if(file.exists(EPC_matched_all_dir)) {
  message("Matched dataset found. Loading from disk.")
  EPC_matched_all <- fread(EPC_matched_all_dir)
} else {
  message("Matched dataset not found. Creating.")
  EPC_matched_all <- convert_epc_datasets(EPC_path, combined_expanded)
}
# write.csv(EPC_matched_all, file = file.path(EPC_DATA_DIR, "epc_matched-random_sample.csv"))

#### Cross-sectional datasets split by freehold and leasehold ####
# Splits the dataset into the freehold and leasehold parts.
setkey(EPC_matched_all, tenure)
EPC_matched_lease <- EPC_matched_all["Leasehold"]
EPC_matched_free <- EPC_matched_all["Freehold"]
EPC_matched_NA <- EPC_matched_all[is.na(tenure)]

# Function to create a cross-sectional dataset with only the most recent EPCs kept.  
create_xsection <- function(datatable){
  setDT(datatable)
  setorder(datatable, BUILDING_REFERENCE_NUMBER, LODGEMENT_DATETIME)
  temp_data <- datatable[,.SD[.N], by = BUILDING_REFERENCE_NUMBER]
  return(temp_data)
}

# Creates the cross-sectional datasets. 
message("Creating cross-sectional datasets...")
EPC_matched_lease_clean <- create_xsection(EPC_matched_lease)
EPC_matched_free_clean <- create_xsection(EPC_matched_free)
EPC_matched_NA_clean <- create_xsection(EPC_matched_NA)

# Duplicate check
EPC_matched_free_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_free_clean$BUILDING_REFERENCE_NUMBER)]
EPC_matched_lease_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_lease_clean$BUILDING_REFERENCE_NUMBER)]
EPC_matched_NA_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_NA_clean$BUILDING_REFERENCE_NUMBER)]

# Re-merges datasets
EPC_matched_combined <- rbind(EPC_matched_free_clean, EPC_matched_lease_clean, EPC_matched_NA_clean)
# EPC_matched_combined <- rbind(EPC_matched_free_clean, EPC_matched_NA_clean)

EPC_matched_combined[, has_duplicates := .N > 1, by = BUILDING_REFERENCE_NUMBER]

setkey(EPC_matched_combined, BUILDING_REFERENCE_NUMBER)

# Outcome variables
EPC_matched_combined[, bad_EPC := CURRENT_ENERGY_RATING %in% c("D", "E", "F", "G")]
EPC_matched_combined[, good_EPC := CURRENT_ENERGY_RATING %in% c("A", "B", "C")]



EPC_matched_combined[is.na(source), source := "Unknown"]
EPC_matched_combined[is.na(tenure), tenure := "Not in OCOD, CCOD"]

# Concatenation
EPC_matched_combined[, concatenation := paste0(PROPERTY_TYPE,TENURE)]

# Coarse proprietorship
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

tax_havens <- c(
    "ANGUILLA", "ANTIGUA AND BARBUDA", "BAHAMAS", "BAHRAIN", "BARBADOS",
    "BELIZE", "BERMUDA", "BRITISH VIRGIN ISLANDS", "CAYMAN ISLANDS", "CYPRUS",
    "GIBRALTAR", "GUERNSEY", "HONG KONG", "ISLE OF MAN", "JERSEY", "LEBANON",
    "LIECHTENSTEIN", "MACAU", "MACAO", "MALTA", "MARSHALL ISLANDS", "MAURITIUS",
    "MONACO", "DUTCH ANTILLES", "PANAMA", "SAMOA", "SEYCHELLES",
    "SINGAPORE", "ST KITTS AND NEVIS", "ST LUCIA", "ST VINCENT AND GRENADINES",
    "TURKS AND CAICOS ISLANDS"
  )


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


EPC_matched_combined[, postcode_area := sub(" .*", "", POSTCODE)]
EPC_matched_combined[, postcode_sector := sub("^([^ ]+ [A-Z0-9]).*", "\\1", POSTCODE)]

EPC_matched_combined[, lodgement_year := year(LODGEMENT_DATE)]

# Save the final combined dataset
output_file <- file.path(output_dir, paste0("epc_matched_combined_", ccod_version, ".RData"))
save(EPC_matched_combined, file = output_file)
message("Final combined dataset saved to ", output_file)
