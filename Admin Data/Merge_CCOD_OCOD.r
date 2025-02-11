rm(list=ls())

### Requirements ### 
library('tidyverse')
library(httr)
library(jsonlite)

### Accessing API ###
api_key <- "" # API key here
ccod_url <- "https://use-land-property-data.service.gov.uk/api/v1/datasets/ccod/CCOD_FULL_2025_01.zip"  # Endpoint for CCOD (update when required)
ocod_url <- "https://use-land-property-data.service.gov.uk/api/v1/datasets/ocod/OCOD_FULL_2025_01.zip"  # Endpoint for OCOD (update when required)

### Program to Fetch Data ### 
fetch_data <- function(api_url, api_key, dest_dir = tempdir()) {
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
  
  temp_zip <- tempfile(fileext = ".zip")
  zip_response <- GET(
    download_url,
    write_disk(temp_zip, overwrite = TRUE)
  )
  
  if (status_code(zip_response) != 200) {
    stop(paste("Failed to download the ZIP file. Status code:", status_code(zip_response)))
  }
  
  # Step 3: Unzip the file into the destination directory
  unzip_result <- tryCatch(
    unzip(temp_zip, exdir = dest_dir),
    error = function(e) stop("Error extracting ZIP file: ", e$message)
  )
  # Get all CSV files from the extracted content
  csv_files <- list.files(dest_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    stop("No CSV files found in the extracted ZIP archive.")
  }
  dataset_type <- if (grepl("ocod", api_url, ignore.case = TRUE)) {
    "OCOD"
  } else if (grepl("ccod", api_url, ignore.case = TRUE)) {
    "CCOD"
  } else {
    stop("Unknown dataset type in the API URL.")
  }
  
  # Step 5: Filter for the specific CSV file
  pattern <- paste0("^", dataset_type, ".*\\.csv$")
  csv_files <- list.files(dest_dir, pattern = pattern, full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop(paste("No", dataset_type, "CSV files found in the extracted ZIP archive."))
  }
  
  # Step 6: Read the first matching CSV file into a data frame
  data <- tryCatch(
    read.csv(csv_files[1], stringsAsFactors = FALSE),
    error = function(e) stop("Error reading CSV file: ", e$message)
  )
  
  return(data)
}

### Fetch CCOD and OCOD data ###
ccod <- fetch_data(ccod_url, api_key)
ocod <- fetch_data(ocod_url, api_key)

## Or use a local dataset ###
# ccod <- read.csv("[FILE PATH HERE]")
# ocod <- read.csv("[FILE PATH HERE]")

ccod$Country.Incorporated..1. = "UNITED KINGDOM" # Labels *all* CCOD observations as UK in the same variable as OCOD

ccod$Source <- "CCOD" #Labels source for observations
ocod$Source <- "OCOD"

### Merge Dataframe ###
combined <- merge(ccod,ocod, all = TRUE)
combined <- combined %>% relocate(Country.Incorporated..1., .after = Proprietorship.Category..1.) # Places country of registration where it would be in the OCOD dataset  
combined <- combined %>% relocate(Country.Incorporated..2., .after = Proprietorship.Category..2.) # Places country of registration where it would be in the OCOD dataset  
combined <- combined %>% relocate(Country.Incorporated..3., .after = Proprietorship.Category..3.) # Places country of registration where it would be in the OCOD dataset  
combined <- combined %>% relocate(Country.Incorporated..4., .after = Proprietorship.Category..4.) # Places country of registration where it would be in the OCOD dataset  


### Clean Dataset Names
library(janitor)
combined <- clean_names(combined)

