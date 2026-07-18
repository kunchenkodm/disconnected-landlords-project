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
FULL_SAMPLE <- TRUE

ENERGY_CONSUMPTION_REFERENCE_YEAR <- "2020"

# Parameters: Matching ----------------------------------------------------
MATCHING_MIN_TREATED_LOW  <- 5L   # Min treated obs (sparse cores)
MATCHING_MIN_TREATED_HIGH <- 10L  # Min treated obs (admin-only cores)
MATCHING_MIN_CONTROL_LOW  <- 25L  # Min control obs (sparse cores)
MATCHING_MIN_CONTROL_HIGH <- 50L  # Min control obs (admin-only cores)
MATCHING_GEOGRAPHY <- Sys.getenv("MATCHING_GEOGRAPHY_OVERRIDE", unset = "LA")  # "LA", "ITL1", "ITL2", or "ITL3"
WITHIN_CORPORATE   <- as.logical(Sys.getenv("WITHIN_CORPORATE_OVERRIDE", unset = "FALSE"))
MATCHING_N_WORKERS <- local({
  ov <- suppressWarnings(as.integer(Sys.getenv("MATCHING_N_WORKERS_OVERRIDE", unset = "")))
  if (!is.na(ov) && ov > 0L) ov else 1L
})

# Directory Paths ---------------------------------------------------------
# Roots are overridable via environment variables (set in .Renviron) so the
# data can live outside the repo (e.g. a central research-data folder).
# Defaults preserve the historical in-repo layout: data/raw, data/processed, output.
.dl_dir <- function(env_var, default) {
  v <- Sys.getenv(env_var, unset = "")
  if (nzchar(v)) path.expand(v) else default
}
RAW_ROOT       <- .dl_dir("DL_RAW_DIR",       here::here("data", "raw"))
PROCESSED_ROOT <- .dl_dir("DL_PROCESSED_DIR", here::here("data", "processed"))
OUTPUT_ROOT    <- .dl_dir("DL_OUTPUT_DIR",    here::here("output"))

# Raw Data Structure
RAW_DATA_DIR       <- RAW_ROOT
RAW_ADMIN_DIR      <- file.path(RAW_ROOT, "admin")
RAW_EPC_DIR        <- file.path(RAW_ROOT, "epc")
RAW_LR_DIR         <- file.path(RAW_ROOT, "land_registry")
RAW_LOOKUPS_DIR    <- file.path(RAW_ROOT, "lookups")
RAW_POSTCODE_DIR   <- file.path(RAW_ROOT, "postcode_level")

# Intermediate Data
PROCESSED_DATA_DIR <- PROCESSED_ROOT
# Temporary per-LA RDS files written here during chunked EPC processing
EPC_TEMP_DIR       <- file.path(PROCESSED_ROOT, "epc_la_temp")

# Per-LA pipeline directory (one .parquet file per local authority)
EPC_LA_REFINED_DIR  <- file.path(PROCESSED_ROOT, "epc_la_refined")

# Output Structure
OUTPUT_DIR         <- OUTPUT_ROOT
MATCHED_DATA_DIR   <- file.path(OUTPUT_ROOT, "matched_data")
RESULTS_DIR        <- file.path(OUTPUT_ROOT, "results")         # For individual model .rds files
SUMMARY_TABLES_DIR <- file.path(OUTPUT_ROOT, "summary_tables")  # For aggregated CSVs
TABLES_DIR         <- file.path(OUTPUT_ROOT, "tables")
FIGURES_DIR        <- file.path(OUTPUT_ROOT, "figures")

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
dir.create(EPC_LA_REFINED_DIR,  showWarnings = FALSE, recursive = TRUE)


# Create Output Directories
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MATCHED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(RESULTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(SUMMARY_TABLES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

MODEL_ARCHIVE_DIR <- file.path(OUTPUT_ROOT, "model_archive")
dir.create(MODEL_ARCHIVE_DIR, showWarnings = FALSE, recursive = TRUE)

message("Global setup complete. Project root set to: ", here::here())
message("Data roots: raw=", RAW_ROOT, " | processed=", PROCESSED_ROOT, " | output=", OUTPUT_ROOT)
