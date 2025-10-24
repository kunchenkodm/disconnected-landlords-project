# Script: 05_create_matched_pairs.R
# Purpose: Implement matching logic to create sets of matched pairs based on two treatment definitions (For-Profit vs. Non-Profit and Abroad vs. Domestic) with exact matching by local_authority.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last Updated August 15, 2025

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
source(here::here("scripts", "treatment_definitions.R"))
EPC_matched_combined <- define_treatments(EPC_matched_combined) 

#### MATCHING PROTOCOL ####
message("Starting matching process for treatment definitions...")

# Define matching core filters
matching_core_filters <- list(
  base = quote(rep(TRUE, .N)),
  council_tax = quote(!is.na(tax_band)),
  ppd = quote(!is.na(ppd_price_sqm)),
  ppd_counciltax = quote(!is.na(tax_band) & !is.na(ppd_price_sqm))
)

# Define specification configurations
spec_configs <- list(
  list(
    name = "Baseline",
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
    exact_vars = c("property_type", "main_fuel", "construction_age_band", "built_form", "local_authority")
  ),
  list(
    name = "Council Tax", 
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
    exact_vars = c("property_type", "main_fuel", "tax_band", "construction_age_band", "built_form", "local_authority")
  ),
  list(
    name = "Council Tax + Price Paid",
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "lodgement_year", "ppd_price_sqm"),
    exact_vars = c("property_type", "main_fuel", "tax_band", "ppd_year_transfer", "construction_age_band", "built_form", "local_authority")
  )
)

# Mapping of treatment variables to descriptive output identifiers
treatment_map <- c(
  treat_for_profit = "for_profit_vs_private_rental",
  treat_uk_for_profit = "uk_for_profit_vs_private_rental",
  treat_foreign_for_profit = "foreign_for_profit_vs_private_rental",
  treat_tax_haven_for_profit = "tax_haven_for_profit_vs_private_rental",
  treat_non_profit = "non_profit_vs_private_rental",
  treat_uk_non_profit = "uk_non_profit_vs_private_rental",
  treat_foreign_non_profit = "foreign_non_profit_vs_private_rental",
  treat_tax_haven_non_profit = "tax_haven_non_profit_vs_private_rental",
  treat_public_sector = "public_sector_vs_private_rental",
  treat_tax_haven = "tax_haven_vs_private_rental",
  treat_british_haven = "british_haven_vs_private_rental",
  treat_european_haven = "european_haven_vs_private_rental",
  treat_caribbean_haven = "caribbean_haven_vs_private_rental",
  treat_other_haven = "other_haven_vs_private_rental"
)

spec_core_pairs <- list(
  Baseline = c("base", "council_tax", "ppd", "ppd_counciltax"),
  `Council Tax` = c("council_tax", "ppd_counciltax"),
  `Council Tax + Price Paid` = c("ppd_counciltax")
)


# Set seed for reproducibility
set.seed(20230703)

# Function to perform matching by local authority and save results
perform_matching <- function(treatment_var, treatment_name, matching_core, core_data, spec_config) {
  message("Performing matching for ", treatment_name, " in matching core: ", matching_core)
  
  # Prepare data
  if (!"data.table" %in% class(core_data)) setDT(core_data)
  
  # Keep only rows where treatment is defined
  dat_all <- core_data[!is.na(get(treatment_var))]
  message("  Rows after removing NA in treatment: ", nrow(dat_all))
  if (nrow(dat_all) == 0) return(list())
  
  # Define base matching variables
  base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                          "property_type", "main_fuel", "construction_age_band",
                          "built_form", "local_authority")
  
  # Remove rows missing any base vars
  dat_all <- dat_all[complete.cases(dat_all[, ..base_matching_vars])]
  message("  Rows after removing NA in base vars: ", nrow(dat_all))
  if (nrow(dat_all) == 0) return(list())
  
  # Get unique local authorities
  dat_all[, local_authority := as.character(local_authority)]
  la_names <- unique(dat_all$local_authority)
  message("  Number of local authorities to process: ", length(la_names))
  
  # Prepare storage for per-LA results for each specification
  files_by_spec <- vector("list", length(spec_configs))
  names(files_by_spec) <- sapply(spec_configs, function(x) x$name)
  for (i in seq_along(files_by_spec)) files_by_spec[[i]] <- character(0)
  
  # MAIN LOOP: iterate local authority by local authority
  for (la in la_names) {
    message("  Processing local authority: ", la)
    idx <- which(dat_all$local_authority == la)
    if (length(idx) == 0) {
      message("    LA has no rows, skipping")
      next
    }
    
    # Create a small LA subset
    dat <- dat_all[idx, ]
    # Shuffle rows deterministically
    dat[, rand_sort := runif(.N)]
    setorder(dat, rand_sort)
    
    # Loop through specifications
    spec_name <- spec_config$name
    
    # Apply spec-specific data filtering
    required_vars <- unique(c(base_matching_vars, spec_config$continuous_vars, spec_config$exact_vars))
    dat_spec <- na.omit(dat, cols = required_vars)
    
    # Remove infinite values if ppd_price_sqm is included
    if ("ppd_price_sqm" %in% spec_config$continuous_vars) {
      dat_spec <- dat_spec[!is.infinite(ppd_price_sqm)]
    }
    
    # Check minimum sample sizes
    min_treated <- if ("ppd_price_sqm" %in% spec_config$continuous_vars) 5 else 
      if ("tax_band" %in% spec_config$exact_vars) 5 else 10
    min_control <- if ("ppd_price_sqm" %in% spec_config$continuous_vars) 25 else
      if ("tax_band" %in% spec_config$exact_vars) 25 else 50
    
    n_treated <- nrow(dat_spec[get(treatment_var) == 1])
    n_control <- nrow(dat_spec[get(treatment_var) == 0])
    
    message("    Spec ", i, " (", spec_name, ") counts (treated/control): ", n_treated, "/", n_control)
    
    if (n_treated > min_treated && n_control > min_control) {
      # Build formula dynamically
      continuous_formula <- paste(spec_config$continuous_vars, collapse = " + ")
      
      # Remove local_authority from exact vars since we're matching within LA
      exact_vars_no_la <- setdiff(spec_config$exact_vars, "local_authority")
      
      tryCatch({
        if (length(exact_vars_no_la) > 0) {
          exact_formula <- paste(exact_vars_no_la, collapse = " + ")
          m <- matchit(
            as.formula(paste0(treatment_var, " ~ ", continuous_formula)),
            data = dat_spec,
            exact = as.formula(paste0("~ ", exact_formula)),
            method = "nearest",
            distance = "glm"
          )
        } else {
          # No exact matching needed (only continuous vars)
          m <- matchit(
            as.formula(paste0(treatment_var, " ~ ", continuous_formula)),
            data = dat_spec,
            method = "nearest",
            distance = "glm"
          )
        }
        
        md <- match.data(m)
        
        if (nrow(md) > 0) {
          dt <- as.data.table(md)[, .(local_authority = la, uprn, distance, weights, subclass)]
          
          # Make subclass unique by prepending LA name
          dt[, subclass := paste0(make.names(la), "_", subclass)]
          
          # Save to file
          fname <- file.path(
            output_dir,
            paste0(
              "matched_", treatment_name, "_", matching_core,
              "_spec_", make.names(spec_name),          # Use make.names for a safe filename
              "_", make.names(la),
              "_", ccod_version, ".rds"
            )
          )
          saveRDS(dt, fname, compress = "xz")
          files_by_spec[[spec_name]] <- c(files_by_spec[[spec_name]], fname)
          
          message("      Spec ", i, ": wrote file ", basename(fname), " (rows: ", nrow(dt), ")")
          rm(m, md, dt)
        } else {
          message("      Spec ", i, ": matchdata empty")
        }
      }, error = function(e) {
        message("      Spec ", i, ": matchit error in LA ", la, " -> ", e$message)
      })
      gc()
    } else {
      message("      Spec ", i, ": skipped (too few treated/control)")
    }
    
    
    # Clean up per-LA data
    rm(dat, idx)
    gc()
  } # end LA loop
  
  ##### Recombine Results by Specification ####
  matched_results <- vector("list", length(spec_configs))
  names(matched_results) <- sapply(spec_configs, function(x) x$name)
  
  for (i in seq_along(matched_results)) {
    spec_name <- names(matched_results)[i]
    fls <- files_by_spec[[i]]
    
    if (length(fls) == 0) {
      matched_results[[i]] <- data.table()
      message("  Combined spec ", i, " (", spec_name, ") is empty.")
      next
    }
    
    # Read files incrementally and combine
    dt_combined <- NULL
    for (j in seq_along(fls)) {
      tmp <- readRDS(fls[j])
      if (is.null(dt_combined)) {
        dt_combined <- tmp
      } else {
        dt_combined <- rbindlist(list(dt_combined, tmp), use.names = TRUE, fill = TRUE)
      }
      rm(tmp); gc()
    }
    
    matched_results[[i]] <- dt_combined
    message("  Combined spec ", i, " (", spec_name, ") rows: ", nrow(matched_results[[i]]))
  }
  
  # Store matching core information in saved file
  matching_core_metadata <- list(
    matching_core_name = matching_core,
    matching_core_filter_expression = matching_core_filters[[matching_core]],
    timestamp = Sys.time(),
    num_local_authorities = length(la_names)
  )
  
  # Save combined results
  output_file <- file.path(output_dir, paste0("matched_pairs_", treatment_name, "_matching_core_", matching_core, "_", ccod_version, ".RData"))
  save(matched_results, matching_core_metadata, file = output_file)
  message("  Matched pairs for ", treatment_name, " matching core ", matching_core, " saved to ", output_file)
  
  # Final cleanup
  rm(dat_all, files_by_spec)
  gc()
  
  return(matched_results)
}

# Run matching for all treatments and all matching cores
for (matching_core in names(matching_core_filters)) {
  matching_core_filter <- matching_core_filters[[matching_core]]
  core_data <- EPC_matched_combined[eval(matching_core_filter)]
  message("Matching for matching core: ", matching_core, " (", nrow(core_data), " rows)")
  
  for (treat_var in names(treatment_map)) {
    treatment_name <- treatment_map[[treat_var]]
    
    for (spec_config in spec_configs) {
      spec_name <- spec_config$name
      allowed_cores <- spec_core_pairs[[spec_name]]
      if (!(matching_core %in% allowed_cores)) next
      perform_matching(treat_var, treatment_name, matching_core, core_data, spec_config)
    }
  }
}

message("Matching process completed for all treatment definitions.")

# DIAGNOSTICS: RUNTIME
end.time <- Sys.time()
time.taken <- end.time - start.time
message("\n Script 5 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")