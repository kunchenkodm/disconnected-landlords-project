rm(list=ls())

### Requirements ### 
library(httr)
library(jsonlite)
library(data.table)

#### SETUP: INPUTS REQUIRED ####
api_key <- "" # Land Registry API key here

ccod_version <- "CCOD_FULL_2025_01"
ocod_version <- "OCOD_FULL_2025_01"

admin_path <- "~/RA_local/Admin"
UPRN_dt_path <- "C:/Users/Kunch/Documents/RA_local/LR_UPRN_FULL_JAN_2025.csv"
EPC_path <- "~/RA_local/domestic-EPC/"

### PATH TO API URL ###
# NB! If you wish to request the newest dataset,change this to "https://use-land-property-data.service.gov.uk/api/v1/datasets/ccod/"
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

# Load or download CCOD
ccod <- fetch_cod_data(ccod_url, api_key, ccod_file)
ocod <- fetch_cod_data(ocod_url, api_key, ocod_file)


ccod$Country.Incorporated..1. = "UNITED KINGDOM" # Labels *all* CCOD observations as UK in the same variable as OCOD


# Labels source for admin data observations
ccod$Source <- "CCOD" 
ocod$Source <- "OCOD"

# Define datasets as data.tables and merge ###
setDT(ccod)
setDT(ocod)

combined <- merge(ccod,ocod, all = TRUE)


# Clean Dataset Names 
library(janitor)
combined <- clean_names(combined)

# Bring variable ordering in line with the OCOD dataset [optional]
setcolorder(combined, c("tenure", "property_address", "district", "county", "region", "postcode", "multiple_address_indicator", "price_paid", "proprietor_name_1", "company_registration_no_1", "proprietorship_category_1", "country_incorporated_1", "proprietor_1_address_1","proprietor_1_address_2","proprietor_1_address_3","proprietor_name_2","company_registration_no_2","proprietorship_category_2","country_incorporated_2","proprietor_2_address_1","proprietor_2_address_2" ,"proprietor_2_address_3","proprietor_name_3","company_registration_no_3","proprietorship_category_3","country_incorporated_3","proprietor_3_address_1","proprietor_3_address_2","proprietor_3_address_3","proprietor_name_4","company_registration_no_4","proprietorship_category_4","country_incorporated_4","proprietor_4_address_1","proprietor_4_address_2","proprietor_4_address_3","date_proprietor_added","additional_proprietor_indicator","source"))

# Clear some Ram
rm(ccod, ocod)



#### UPRN LOOKUP ####

# Load the geovation Title Number-UPRN lookup dataset and set title_number as data.table key
UPRN_dt <- fread(UPRN_dt_path)
setnames(UPRN_dt, c("V1", "V2"), c("title_number","UPRN"))
UPRN_dt[,V3:=NULL]

setkey(UPRN_dt,title_number)
setkey(combined,title_number)


# Assign UPRNS to the Admin dataset.
# Titles with multiple UPRNs: Each UPRN is added as a separate entity in the combined dataset.
combined_expanded <- UPRN_dt[combined, nomatch = 0]

# Clear some memory
rm(combined, UPRN_dt)
tables()
# Convert UPRN to character [for some reason the only way to get it to work] and set is as key for the data.table
combined_expanded$UPRN <- as.character(combined_expanded$UPRN)
setkey(combined_expanded, UPRN)



#### EPC MATCHING ####

# Function to convert and join all EPC datasets in a folder using the combined admin dataset and regional sub-datasets of EPC (load however many is needed into target folder EPC_path)
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
EPC_matched_all_dir <- "~/disconnected-landlords-project/epc_matched_newham.csv"
if(file.exists(EPC_matched_all_dir)) {
  print("Matched dataset found. Loading from disk.") 
  EPC_matched_all <- fread(EPC_matched_all_dir)
} else {
  print("Matched dataset not found. Creating.") 
  EPC_matched_all <- convert_epc_datasets(EPC_path, combined_expanded)
  }
    

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
EPC_matched_lease_clean <- create_xsection(EPC_matched_lease)
EPC_matched_free_clean <- create_xsection(EPC_matched_free)
EPC_matched_NA_clean <- create_xsection(EPC_matched_NA)

#Duplicate check
EPC_matched_free_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_free_clean$BUILDING_REFERENCE_NUMBER)]
EPC_matched_lease_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_lease_clean$BUILDING_REFERENCE_NUMBER)]
EPC_matched_NA_clean$BUILDING_REFERENCE_NUMBER[duplicated(EPC_matched_NA_clean$BUILDING_REFERENCE_NUMBER)]

# Re-merges datasets
EPC_matched_combined <- rbind(EPC_matched_free_clean, EPC_matched_lease_clean, EPC_matched_NA_clean)

EPC_matched_combined[, has_duplicate := .N > 1, by = BUILDING_REFERENCE_NUMBER]
EPC_matched_combined[, EPC_bad := CURRENT_ENERGY_RATING %in% c("D", "E", "F", "G")]


#### PANEL DATASET: TO DO ####