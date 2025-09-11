# Script: 00_setup.R
# Purpose: Define global variables and paths for the Disconnected Landlords project.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025

library(here)

API_KEY <- Sys.getenv("LAND_REGISTRY_API_KEY") # Load API key from .env file
CCOD_VERSION <- "CCOD_FULL_2025_01"
OCOD_VERSION <- "OCOD_FULL_2025_01"
ENABLE_EXTRACTION <- FALSE
SAMPLE_SIZE <- 30L


# --- 3. DIRECTORY PATHS ---
# Define all paths relative to the project root using here()
# This makes the project portable.
RAW_DATA_DIR      <- here::here("data", "raw")
PROCESSED_DATA_DIR <- here::here("data", "processed")
OUTPUT_DIR        <- here::here("output")
MATCHED_DATA_DIR  <- here::here("output", "matched_data")
TABLES_DIR        <- here::here("output", "tables")
FIGURES_DIR       <- here::here("output", "figures")

# Specific raw data subdirectories
RAW_ADMIN_DIR     <- here::here("data", "raw", "admin")
RAW_EPC_DIR       <- here::here("data", "raw", "epc")
RAW_LR_DIR        <- here::here("data", "raw", "land_registry")
RAW_LOOKUPS_DIR   <- here::here("data", "raw", "lookups")
RAW_POSTCODE_DIR  <- here::here("data", "raw", "postcode_level")

# --- 4. CREATE DIRECTORIES ---
# Ensure all output directories exist
dir.create(PROCESSED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MATCHED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

message("Global setup complete. Project root set to: ", here::here())
