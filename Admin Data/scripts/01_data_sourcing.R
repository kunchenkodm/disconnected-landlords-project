# Script: 01_data_sourcing.R
# Purpose: Source data from Land Registry API or local storage, perform initial cleaning, and save a combined dataset for further processing.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc() 
# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))
### Requirements ###
library(httr)
library(jsonlite)
library(data.table)

#### SETUP: INPUTS REQUIRED ####
# Configuration section for user customization (using global variables from 00_setup.R)
api_key <- API_KEY
ccod_version <- CCOD_VERSION
ocod_version <- OCOD_VERSION
admin_path <- RAW_ADMIN_DIR
output_dir <- PROCESSED_DATA_DIR

### PATH TO API URL ###
# NB! If you wish to request the newest dataset, change this to "https://use-land-property-data.service.gov.uk/api/v1/datasets/ccod/"
ccod_url <- paste0("https://use-land-property-data.service.gov.uk/api/v1/datasets/history/ccod/", ccod_version, ".zip", sep ='')
ocod_url <- paste0("https://use-land-property-data.service.gov.uk/api/v1/datasets/history/ocod/", ocod_version, ".zip", sep ='')

# Expected *cod file names #
ccod_file <- file.path(admin_path, paste0(ccod_version, ".csv"))
ocod_file <- file.path(admin_path, paste0(ocod_version, ".csv"))

#### CREATE *cod dataset ####

### Program to Fetch Land Registry Data #
fetch_cod_data <- function(api_url, api_key, dest_file) {
  if (file.exists(dest_file)) {
    message("File ", dest_file, " already exists. Loading from local storage.")
    return(read.csv(dest_file, stringsAsFactors = FALSE))
  }
  
  # Step 1: Fetch the temporary download URL
  response <- GET(
    api_url,
    accept_json(),
    add_headers(Authorization = api_key)
  )
  # Check if the API request was successful
  if (status_code(response) != 200) {
    stop(paste("Failed to fetch download URL. Status code:", status_code(response)))
  }
  
  # Extract the temporary download link from the JSON response
  content_json <- content(response, "parsed", simplifyVector = TRUE)
  if (!content_json$success) {
    stop("API response indicates failure: ", content_json)
  }
  download_url <- content_json$result$download_url
  if (is.null(download_url)) {
    stop("Download URL not found in the API response.")
  }
  
  # Download the zip file from the link, extract csvs
  temp_zip <- tempfile(fileext = ".zip")
  zip_response <- GET(download_url, write_disk(temp_zip, overwrite = TRUE))
  
  if (status_code(zip_response) != 200) {
    stop(paste("Failed to download the ZIP file. Status code:", status_code(zip_response)))
  }
  
  unzip(temp_zip, exdir = admin_path)
  
  csv_files <- list.files(admin_path, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    stop("No CSV files found in the extracted ZIP archive.")
  }
  
  # Re-identify dataset in folder
  dataset_type <- if (grepl("ocod", api_url, ignore.case = TRUE)) "OCOD" else "CCOD"
  pattern <- paste0("^", dataset_type, ".*\\.csv$")
  csv_files <- list.files(admin_path, pattern = pattern, full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop(paste("No", dataset_type, "CSV files found in the extracted ZIP archive."))
  }
  
  # Read the first matching CSV file into a data frame
  data <- tryCatch(
    read.csv(csv_files[1], stringsAsFactors = FALSE),
    error = function(e) stop("Error reading CSV file: ", e$message)
  )
  
  return(data)
}

# Load or download CCOD and OCOD
message("Fetching CCOD data...")
ccod <- fetch_cod_data(ccod_url, api_key, ccod_file)
message("Fetching OCOD data...")
ocod <- fetch_cod_data(ocod_url, api_key, ocod_file)

# Initial Cleaning
ccod$Country.Incorporated..1. = "UNITED KINGDOM" # Labels *all* CCOD observations as UK in the same variable as OCOD

# Labels source for admin data observations
ccod$Source <- "CCOD" 
ocod$Source <- "OCOD"

# Define datasets as data.tables and merge
setDT(ccod)
setkey(ccod, Title.Number)

setDT(ocod)
setkey(ocod, Title.Number)

combined <- rbindlist(list(ocod, ccod), fill = TRUE) # Data-table merge please

# Clean Dataset Names 
library(janitor)
combined <- clean_names(combined)

# Bring variable ordering in line with the OCOD dataset [optional]
setcolorder(combined, c("tenure", "property_address", "district", "county", "region", "postcode", "multiple_address_indicator", "price_paid", "proprietor_name_1", "company_registration_no_1", "proprietorship_category_1", "country_incorporated_1", "proprietor_1_address_1","proprietor_1_address_2","proprietor_1_address_3","proprietor_name_2","company_registration_no_2","proprietorship_category_2","country_incorporated_2","proprietor_2_address_1","proprietor_2_address_2" ,"proprietor_2_address_3","proprietor_name_3","company_registration_no_3","proprietorship_category_3","country_incorporated_3","proprietor_3_address_1","proprietor_3_address_2","proprietor_3_address_3","proprietor_name_4","company_registration_no_4","proprietorship_category_4","country_incorporated_4","proprietor_4_address_1","proprietor_4_address_2","proprietor_4_address_3","date_proprietor_added","additional_proprietor_indicator","source"))

# Save the combined dataset for further processing
output_file <- file.path(output_dir, paste0("combined_admin_data_", ccod_version, ".RData"))
save(combined, file = output_file)
message("Combined dataset saved to ", output_file)

# Clear some RAM
rm(ccod, ocod)
