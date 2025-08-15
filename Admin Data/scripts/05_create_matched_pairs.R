# Script: 05_create_matched_pairs.R
# Purpose: Implement matching logic to create sets of matched pairs based on two treatment definitions (For-Profit vs. Non-Profit and Abroad vs. Domestic) with exact matching by local_authority.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last Updated August 7, 2025

rm(list=setdiff(ls(), "script"))


# DIAGNOSTICS: RUNTIME
start.time <- Sys.time()


# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(MatchIt)

#### SETUP: INPUTS REQUIRED ####
# Configuration section using global variables from 00_setup.R
ccod_version <- CCOD_VERSION
input_dir <- PROCESSED_DATA_DIR
output_dir <- MATCHED_DATA_DIR # Save matched pairs to output/matched_data

# Input file from feature refinement script
input_file <- file.path(input_dir, paste0("epc_matched_refined_", ccod_version, ".RData"))

# Load the EPC matched refined dataset
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}
message("Loading EPC matched refined dataset from ", input_file)
load(input_file)

##### TREATMENT DEFINITIONS #####
message("Defining treatment variables...")
# Define the common control group
control_group_condition <- quote(source == "Unknown" & grepl("rental \\(private\\)|Rented \\(private\\)", tenure_2, ignore.case = TRUE))

# Treat 1: For-Profit vs. Privately Rented
EPC_matched_combined[, treat_for_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE), 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 1a: UK For-Profit vs. Privately Rented
EPC_matched_combined[, treat_uk_for_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 1b: Foreign For-Profit vs. Privately Rented
EPC_matched_combined[, treat_foreign_for_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 1c: Tax Haven For-Profit vs. Privately Rented
EPC_matched_combined[, treat_tax_haven_for_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_tax_haven) & country_incorporated_tax_haven == 1, 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]


# --- Non-Profit Treatments ---
# Treat 2: Non-Profit vs. Privately Rented
EPC_matched_combined[, treat_non_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE), 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 2a: UK Non-Profit vs. Privately Rented
EPC_matched_combined[, treat_uk_non_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 2b: Foreign Non-Profit vs. Privately Rented
EPC_matched_combined[, treat_foreign_non_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 2c: Tax Haven Non-Profit vs. Privately Rented
EPC_matched_combined[, treat_tax_haven_non_profit := fcase(
  !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_tax_haven) & country_incorporated_tax_haven == 1, 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 3: Public Sector vs. Privately Rented
EPC_matched_combined[, treat_public_sector := fcase(
  !is.na(coarse_proprietorship) & grepl("Public Sector", coarse_proprietorship, ignore.case = TRUE), 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 99: Abroad vs. Domestic (among For-Profits only)
EPC_matched_combined[, treat_abroad_domestic := fcase(
  !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L,
  !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 0L,
  default = NA_integer_
)]

# Tax Haven Treatments:
# Treat 4: Tax Haven vs Privately Rented
EPC_matched_combined[, treat_tax_haven := fcase(
  country_incorporated_tax_haven == 1, 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 5: British Haven vs Privately Rented
EPC_matched_combined[, treat_british_haven := fcase(
  country_incorporated_british_haven == 1, 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 6: European Haven vs Privately Rented
EPC_matched_combined[, treat_european_haven := fcase(
  country_incorporated_european_haven == 1, 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 7: Caribbean Haven vs Privately Rented
EPC_matched_combined[, treat_caribbean_haven := fcase(
  country_incorporated_caribbean_haven == 1, 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]

# Treat 8: Other Haven vs Privately Rented
EPC_matched_combined[, treat_other_haven := fcase(
  country_incorporated_other_haven == 1, 1L,
  eval(control_group_condition), 0L,
  default = NA_integer_
)]


#### MATCHING PROTOCOL ####
message("Starting matching process for  treatment definitions...")

# Set seed for reproducibility
set.seed(20230703)

# Matching will be done with exact matching by local_authority
# Following the protocol in '03 create matched pairs.R', create three sets of matched pairs for each treatment
# with varying matching variables: property level features, council tax band, and property price data

# Function to perform matching and save results
perform_matching <- function(treatment_var, treatment_name) {
  message("Performing matching for ", treatment_name)
  
  
  dat <- EPC_matched_combined[!is.na(get(treatment_var))]
  
  # 2. From this set, remove rows with NAs in ANY of the covariates used in the first matching step.
  base_matching_vars <- c(treatment_var, "number_habitable_rooms", "total_floor_area", "lodgement_year", 
                          "property_type", "main_fuel", "construction_age_band", 
                          "built_form", "local_authority")
  dat <- na.omit(dat, cols = base_matching_vars)
  
  # # Prepare dataset by removing rows with NA in key matching variables
  # dat <- na.omit(EPC_matched_combined, cols = c(treatment_var, "number_habitable_rooms", "total_floor_area", "lodgement_year", "property_type", "main_fuel", "construction_age_band", "built_form", "local_authority"))
  # # Corrected line
  # # Keep only rows where the treatment is defined (0 or 1)

    # dat <- EPC_matched_combined[!is.na(get(treatment_var))]
  dat[, rand_sort := runif(nrow(dat))]
  dat <- dat[order(rand_sort)]

  # Initialize results list
  matched_results <- list()
  # First matching set: Property level features
  if (nrow(dat[get(treatment_var) == 1]) > 10 & nrow(dat[get(treatment_var) == 0]) > 50) {
    matched_chars1 <- match.data(matchit(as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year")),
                                         data = dat,
                                         exact = ~property_type + main_fuel + construction_age_band + built_form + local_authority,
                                         method = "nearest",
                                         distance = "glm"))
    matched_results[[1]] <- matched_chars1[, list(local_authority, uprn, distance, weights, subclass)]
  }

  # Second matching set: Add council tax band information if available
  dat_tax <- na.omit(dat, "tax_band")
  if (nrow(dat_tax[get(treatment_var) == 1]) > 5 & nrow(dat_tax[get(treatment_var) == 0]) > 25) {
    matched_chars2 <- match.data(matchit(as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year")),
                                         data = dat_tax,
                                         exact = ~property_type + main_fuel + tax_band + construction_age_band + built_form + local_authority,
                                         method = "nearest",
                                         distance = "glm"))
    matched_results[[2]] <- matched_chars2[, list(local_authority, uprn, distance, weights, subclass)]
  }

  # Third matching set: Add property price data if available
  price_vars <- c("tax_band", "ppd_price_sqm", "ppd_year_transfer")
  dat_price <- na.omit(dat, cols = price_vars)
  dat_price <- dat_price[!is.infinite(ppd_price_sqm)]
  # dat_price <- dat[!is.na(tax_band) & !is.na(ppd_price_sqm) & !is.na(ppd_year_transfer) & !is.infinite(ppd_price_sqm)]
  if (nrow(dat_price[get(treatment_var) == 1]) > 5) {
    matched_chars3 <- match.data(matchit(as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year + ppd_price_sqm")),
                                         data = dat_price,
                                         exact = ~property_type + main_fuel + tax_band + ppd_year_transfer + construction_age_band + built_form + local_authority,
                                         method = "nearest",
                                         distance = "glm"))
    matched_results[[3]] <- matched_chars3[, list(local_authority, uprn, distance, weights, subclass)]
  }

  # Save results
  output_file <- file.path(output_dir, paste0("matched_pairs_", treatment_name, "_", ccod_version, ".RData"))
  save(matched_results, file = output_file)
  message("Matched pairs for ", treatment_name, " saved to ", output_file)

  return(matched_results)

}


# Perform matching for both treatment definitions
matched_for_profit <- perform_matching("treat_for_profit", "for_profit_vs_private_rental")
matched_uk_for_profit <- perform_matching("treat_uk_for_profit", "uk_for_profit_vs_private_rental")
matched_foreign_for_profit <- perform_matching("treat_foreign_for_profit", "foreign_for_profit_vs_private_rental")
matched_tax_haven_for_profit <- perform_matching("treat_tax_haven_for_profit", "tax_haven_for_profit_vs_private_rental")

matched_non_profit <- perform_matching("treat_non_profit", "non_profit_vs_private_rental")
matched_uk_non_profit <- perform_matching("treat_uk_non_profit", "uk_non_profit_vs_private_rental")
matched_foreign_non_profit <- perform_matching("treat_foreign_non_profit", "foreign_non_profit_vs_private_rental")
matched_tax_haven_non_profit <- perform_matching("treat_tax_haven_non_profit", "tax_haven_non_profit_vs_private_rental")

matched_public <- perform_matching("treat_public_sector", "public_sector_vs_private_rental")

matched_tax_haven <- perform_matching("treat_tax_haven", "tax_haven_vs_private_rental")
matched_british_haven <- perform_matching("treat_british_haven", "british_haven_vs_private_rental")
matched_european_haven <- perform_matching("treat_european_haven", "european_haven_vs_private_rental")
matched_caribbean_haven <- perform_matching("treat_caribbean_haven", "caribbean_haven_vs_private_rental")
matched_other_haven <- perform_matching("treat_other_haven", "other_haven_vs_private_rental")



# matched_abroad_domestic <- perform_matching("treat_abroad_domestic", "abroad_vs_domestic")





message("Matching process completed for both treatment definitions.")


# DIAGNOSTICS: RUNTIME
end.time <- Sys.time()
time.taken <- end.time - start.time
message("\n Script 5 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")


