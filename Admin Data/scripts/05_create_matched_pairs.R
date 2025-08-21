# Script: 05_create_matched_pairs.R
# Purpose: Implement matching logic to create sets of matched pairs based on two treatment definitions (For-Profit vs. Non-Profit and Abroad vs. Domestic) with exact matching by local_authority.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last Updated August 15, 2025

rm(list=setdiff(ls(), "script"))


# DIAGNOSTICS: RUNTIME
start.time <- Sys.time()


# Source global setup script 
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


# # Treat 99: Abroad vs. Domestic (among For-Profits only)
# EPC_matched_combined[, treat_abroad_domestic := fcase(
#   !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L,
#   !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) & !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 0L,
#   default = NA_integer_
# )]



#### MATCHING PROTOCOL ####
message("Starting matching process for  treatment definitions...")

# Set seed for reproducibility
set.seed(20230703)

# Matching will be done with exact matching, looping by NUTS1 Region
# Following the protocol in '03 create matched pairs.R', create three sets of matched pairs for each treatment
# with varying matching variables: property level features, council tax band, and property price data

# Function to perform matching and save results
perform_matching <- function(treatment_var, treatment_name,
                             min_treated_base = 10, min_control_base = 50,
                             min_treated_tax = 5, min_control_tax = 25,
                             min_treated_price = 5) {
  ##### Prepare Data For Matching #### 
  message("Performing matching for ", treatment_name)
  if (!"data.table" %in% class(EPC_matched_combined)) setDT(EPC_matched_combined)
  dat_all <- copy(EPC_matched_combined)
  
  # keep only rows where treatment is defined
  dat_all <- dat_all[!is.na(get(treatment_var))]
  message("Rows after removing NA in treatment: ", nrow(dat_all))
  if (nrow(dat_all) == 0) return(list())
  
  # core matching vars (exclude treatment var from this core list)
  base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                          "property_type", "main_fuel", "construction_age_band",
                          "built_form", "local_authority", "NUTS118NM")
  
  # remove rows with NA in any of the core vars (user's original logic)
  dat_all <- dat_all[complete.cases(dat_all[, ..base_matching_vars])]
  message("Rows after removing NA in base vars: ", nrow(dat_all))
  if (nrow(dat_all) == 0) return(list())
  
  # ensure region column is character
  dat_all[, NUTS118NM := as.character(NUTS118NM)]
  # split by region (robust base split)
  region_list <- split(dat_all, dat_all$NUTS118NM, drop = TRUE)
  region_names <- names(region_list)
  message("Number of regions to process: ", length(region_names))
  
  # storage for regional matched results (each element will be a list of up to 3 data.tables)
  regional_matches <- vector("list", length(region_names))
  names(regional_matches) <- region_names
  
  # iterate regions
  for (reg in region_names) {
    message("Processing region: ", reg)
    dat <- region_list[[reg]]
    
    # shuffle rows to avoid ordering issues (reproducible if user set seed before call)
    dat[, rand_sort := runif(.N)]
    setorder(dat, rand_sort)
    
    # container for this region's matched outputs
    r_matches <- vector("list", 3)
    
    ##### [1] Property-level features #####
    n_treated <- nrow(dat[get(treatment_var) == 1])
    n_control <- nrow(dat[get(treatment_var) == 0])
    message("  base counts (treated/control): ", n_treated, "/", n_control)
    if (n_treated > min_treated_base && n_control > min_control_base) {
      f1 <- as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year"))
      try({
        m1 <- matchit(f1,
                      data = dat,
                      exact = ~property_type + main_fuel + construction_age_band + built_form + local_authority,
                      method = "nearest",
                      distance = "glm")
        md1 <- match.data(m1)
        if (nrow(md1) > 0) {
          dt1 <- as.data.table(md1)[, .(NUTS118NM = reg, local_authority, uprn, distance, weights, subclass)]
          dt1[, subclass := paste(reg, subclass, sep = "_")]
          r_matches[[1]] <- dt1
          message("base: matched rows: ", nrow(dt1))
        }
      }, silent = TRUE)
    } else {
      message("base: skipped (too few treated/control)")
    }
    
    ##### [2] Council Tax Band #####
    dat_tax <- dat[!is.na(tax_band)]
    n_treated_tax <- nrow(dat_tax[get(treatment_var) == 1])
    n_control_tax <- nrow(dat_tax[get(treatment_var) == 0])
    message("tax counts (treated/control): ", n_treated_tax, "/", n_control_tax)
    if (n_treated_tax > min_treated_tax && n_control_tax > min_control_tax) {
      f2 <- as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year"))
      try({
        m2 <- matchit(f2,
                      data = dat_tax,
                      exact = ~property_type + main_fuel + tax_band + construction_age_band + built_form + local_authority,
                      method = "nearest",
                      distance = "glm")
        md2 <- match.data(m2)
        if (nrow(md2) > 0) {
          dt2 <- as.data.table(md2)[, .(NUTS118NM = reg, local_authority, uprn, distance, weights, subclass)]
          dt2[, subclass := paste(reg, subclass, sep = "_")]
          r_matches[[2]] <- dt2
          message("tax: matched rows: ", nrow(dt2))
        }
      }, silent = TRUE)
    } else {
      message("tax: skipped (too few treated/control)")
    }
    
    ##### [3] Price Paid Data #####
    dat_price <- dat[!is.na(tax_band) & !is.na(ppd_price_sqm) & !is.na(ppd_year_transfer) & is.finite(ppd_price_sqm)]
    n_treated_price <- nrow(dat_price[get(treatment_var) == 1])
    n_control_price <- nrow(dat_price[get(treatment_var) == 0])
    message("price counts (treated/control): ", n_treated_price, "/", n_control_price)
    if (n_treated_price > min_treated_price && n_control_price > 0) {
      f3 <- as.formula(paste0(treatment_var, " ~ number_habitable_rooms + total_floor_area + lodgement_year + ppd_price_sqm"))
      try({
        m3 <- matchit(f3,
                      data = dat_price,
                      exact = ~property_type + main_fuel + tax_band + ppd_year_transfer + construction_age_band + built_form + local_authority,
                      method = "nearest",
                      distance = "glm")
        md3 <- match.data(m3)
        if (nrow(md3) > 0) {
          dt3 <- as.data.table(md3)[, .(NUTS118NM = reg, local_authority, uprn, distance, weights, subclass)]
          dt3[, subclass := paste(reg, subclass, sep = "_")]
          r_matches[[3]] <- dt3
          message("    price: matched rows: ", nrow(dt3))
        }
      }, silent = TRUE)
    } else {
      message("price: skipped (too few treated/control)")
    }
    
    # store regional matches (may contain NULLs for skipped match types)
    regional_matches[[reg]] <- r_matches
  } # end region loop
  
  ##### Recombine Regional Outputs ####
  # For each of the 3 match types, collect all region-level data.tables (skip NULLs) and rbindlist
  combined <- vector("list", 3)
  names(combined) <- c("property_level", "with_taxband", "with_price")
  for (i in seq_len(3)) {
    pieces <- lapply(regional_matches, function(x) x[[i]])
    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) > 0) {
      combined[[i]] <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
      message("Combined match type ", i, " rows: ", nrow(combined[[i]]))
    } else {
      combined[[i]] <- data.table()  # empty table
      message("Combined match type ", i, " is empty.")
    }
  }
  
  # Save results (list of 3 data.tables)
  matched_results <- combined
  output_file <- file.path(output_dir, paste0("matched_pairs_", treatment_name, "_", ccod_version, ".RData"))
  save(matched_results, file = output_file)
  message("Matched pairs for ", treatment_name, " saved to ", output_file)
  
  return(matched_results)
}

#### Perform Matching and Save Matched Pairs ####

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


