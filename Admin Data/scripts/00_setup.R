# Script: 00_setup.R
# Purpose: Define global variables and paths for the Disconnected Landlords project.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025

library(here)

config <- config::get(file = here::here("config", "config.yml"))# Base path for the project

API_KEY <- Sys.getenv("API_KEY") # Load API key from .env file
CCOD_VERSION <- config$ccod_version
OCOD_VERSION <- config$ocod_version
ENABLE_EXTRACTION <- config$enable_extraction
SAMPLE_SIZE <- config$sample_size


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

# --- 4. CREATE DIRECTORIES ---
# Ensure all output directories exist
dir.create(PROCESSED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MATCHED_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)

message("Global setup complete. Project root set to: ", here::here())
