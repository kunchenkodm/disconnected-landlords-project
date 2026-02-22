# Script: 00_setup.R
# Purpose: Define global variables and paths for the Disconnected Landlords project.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025, Last updated November 14, 2025

library(here)


# Reproducibility  --------------------------------------------------------
# Set seed for reproducibility
set.seed(14112025)

# Parameters: Loading Datasets --------------------------------------------
API_KEY <- Sys.getenv("LAND_REGISTRY_API_KEY") # Load API key from .env file
CCOD_VERSION <- "CCOD_FULL_2025_01"
OCOD_VERSION <- "OCOD_FULL_2025_01"


# Parameters: Analysis ----------------------------------------------------
# Enable extaction from EPC archives
ENABLE_EXTRACTION <- FALSE # Manually unzip everything
# Pilot sample size as defined in PAP Section 2.3 (Random subset of LAs)
LA_SAMPLE_SIZE <- 30L
# Set TRUE to process all ~400 LAs (England & Wales); FALSE uses the pilot sample of LA_SAMPLE_SIZE LAs.
FULL_SAMPLE <- FALSE

ENERGY_CONSUMPTION_REFERENCE_YEAR <- "2020"

# Matching Thresholds (Minimum obs required per LA to attempt matching)
# "Low" threshold used when data is sparse (e.g. Price Paid Data present)
# "High" threshold used for admin-only data
MATCHING_MIN_TREATED_LOW  <- 5L
MATCHING_MIN_TREATED_HIGH <- 10L
MATCHING_MIN_CONTROL_LOW  <- 25L
MATCHING_MIN_CONTROL_HIGH <- 50L

# Directory Paths ---------------------------------------------------------
# Define all paths relative to the project root using here()

# Raw Data Structure
RAW_DATA_DIR       <- here::here("data", "raw")
RAW_ADMIN_DIR      <- here::here("data", "raw", "admin")
RAW_EPC_DIR        <- here::here("data", "raw", "epc")
RAW_LR_DIR         <- here::here("data", "raw", "land_registry")
RAW_LOOKUPS_DIR    <- here::here("data", "raw", "lookups")
RAW_POSTCODE_DIR   <- here::here("data", "raw", "postcode_level")

# Intermediate Data
PROCESSED_DATA_DIR <- here::here("data", "processed")
# Temporary per-LA RDS files written here during chunked EPC processing
EPC_TEMP_DIR       <- here::here("data", "processed", "epc_la_temp")

# Per-LA pipeline directories (one file per local authority at each stage)
EPC_LA_MERGED_DIR   <- here::here("data", "processed", "epc_la_merged")
EPC_LA_ENHANCED_DIR <- here::here("data", "processed", "epc_la_enhanced")
EPC_LA_REFINED_DIR  <- here::here("data", "processed", "epc_la_refined")

# Output Structure
OUTPUT_DIR         <- here::here("output")
MATCHED_DATA_DIR   <- here::here("output", "matched_data")
RESULTS_DIR        <- here::here("output", "results")         # For individual model .rds files
SUMMARY_TABLES_DIR <- here::here("output", "summary_tables")  # For aggregated CSVs
TABLES_DIR         <- here::here("output", "tables")
FIGURES_DIR        <- here::here("output", "figures")

# Directory Creation  ---------------------------------------------------------
# Create Raw Data Directories
dir.create(RAW_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RAW_ADMIN_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RAW_EPC_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RAW_LR_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RAW_LOOKUPS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RAW_POSTCODE_DIR, showWarnings = FALSE, recursive = TRUE)

# Create Intermediate Directories
dir.create(PROCESSED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(EPC_TEMP_DIR,       showWarnings = FALSE, recursive = TRUE)
dir.create(EPC_LA_MERGED_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(EPC_LA_ENHANCED_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(EPC_LA_REFINED_DIR,  showWarnings = FALSE, recursive = TRUE)


# Create Output Directories
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MATCHED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(SUMMARY_TABLES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

message("Global setup complete. Project root set to: ", here::here())
