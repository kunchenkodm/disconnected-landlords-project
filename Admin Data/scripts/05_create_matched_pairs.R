# Script: 05_create_matched_pairs.R
# Purpose: Implement matching logic to create sets of matched pairs based on
#          treatment definitions with exact matching by local_authority.
#          Loads per-LA Parquet files one at a time to stay within 16GB RAM.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last Updated Febuary 20, 2026.

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()
# Set seed for reproducibility
set.seed(20230703)

# DIAGNOSTICS: RUNTIME
start.time <- Sys.time()

# Source global setup, specifications, and treatment definitions
source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))
message("Sourced setup, model specifications, and treatment definitions.")


### Requirements ###
library(data.table)
library(MatchIt)
library(arrow)

# SETUP: INPUTS REQUIRED  --------------------------------------------------
ccod_version <- CCOD_VERSION
input_dir <- EPC_LA_REFINED_DIR
output_dir <- MATCHED_DATA_DIR

la_files <- list.files(input_dir, pattern = "\\.parquet$", full.names = TRUE)
n_la_files <- length(la_files)

if (n_la_files == 0L) {
  stop("No per-LA Parquet files found in: ", input_dir)
}
message(sprintf("Found %d per-LA Parquet files.", n_la_files))


# Single-LA Matching Function ----------------------------------------------
# Runs matchit() for one LA, one treatment, one spec. Returns the matched
# data.table or NULL if matching is skipped/fails.
match_single_la <- function(dat, la_label, treatment_var, treatment_name,
                            matching_core, spec_config, output_dir, ccod_version) {

  spec_name <- spec_config$name

  # Define base matching variables (excluding local_authority — only one LA)
  base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                          "property_type", "main_fuel", "construction_age_band",
                          "built_form")

  # Keep only rows where treatment is defined
  dat <- dat[!is.na(get(treatment_var))]
  if (nrow(dat) == 0) return(NULL)

  # Remove rows missing any base vars
  dat <- dat[complete.cases(dat[, ..base_matching_vars])]
  if (nrow(dat) == 0) return(NULL)

  # Apply spec-specific data filtering
  required_vars <- unique(c(base_matching_vars, spec_config$continuous_vars, spec_config$exact_vars))
  required_vars <- setdiff(required_vars, "local_authority")
  dat <- na.omit(dat, cols = required_vars)

  # Remove infinite values if ppd_price_sqm is included
  if ("ppd_price_sqm" %in% spec_config$continuous_vars) {
    dat <- dat[!is.infinite(ppd_price_sqm)]
  }

  # Check minimum sample sizes
  is_sparse <- ("ppd_price_sqm" %in% spec_config$continuous_vars) || ("tax_band" %in% spec_config$exact_vars)
  min_treated <- if (is_sparse) MATCHING_MIN_TREATED_LOW else MATCHING_MIN_TREATED_HIGH
  min_control <- if (is_sparse) MATCHING_MIN_CONTROL_LOW else MATCHING_MIN_CONTROL_HIGH

  n_treated <- nrow(dat[get(treatment_var) == 1])
  n_control <- nrow(dat[get(treatment_var) == 0])

  if (n_treated <= min_treated || n_control <= min_control) {
    return(NULL)
  }

  # Shuffle rows deterministically
  dat[, rand_sort := runif(.N)]
  setorder(dat, rand_sort)

  # Build formula dynamically
  continuous_formula <- paste(spec_config$continuous_vars, collapse = " + ")

  # Remove local_authority from exact vars since we're matching within LA
  exact_vars_no_la <- setdiff(spec_config$exact_vars, "local_authority")

  result <- tryCatch({
    if (length(exact_vars_no_la) > 0) {
      exact_formula <- paste(exact_vars_no_la, collapse = " + ")
      m <- matchit(
        as.formula(paste0(treatment_var, " ~ ", continuous_formula)),
        data = dat,
        exact = as.formula(paste0("~ ", exact_formula)),
        method = "nearest",
        distance = "glm"
      )
    } else {
      m <- matchit(
        as.formula(paste0(treatment_var, " ~ ", continuous_formula)),
        data = dat,
        method = "nearest",
        distance = "glm"
      )
    }

    md <- match.data(m)
    if (nrow(md) == 0) return(NULL)

    dt_out <- as.data.table(md)[, .(local_authority = la_label, uprn, distance, weights, subclass)]

    # Make subclass unique by prepending LA name
    dt_out[, subclass := paste0(make.names(la_label), "_", subclass)]

    # Save per-LA matched RDS
    fname <- file.path(
      output_dir,
      paste0(
        "matched_", treatment_name, "_", matching_core,
        "_spec_", make.names(spec_name),
        "_", make.names(la_label),
        "_", ccod_version, ".rds"
      )
    )
    saveRDS(dt_out, fname, compress = "xz")
    rm(m, md)

    return(list(file = fname, nrows = nrow(dt_out)))
  }, error = function(e) {
    NULL
  })

  return(result)
}


# Main LA-Outer Matching Loop ----------------------------------------------
# For each LA Parquet file: load once, run all valid (matching_core x
# treatment x spec) combinations, then free memory.

message("Starting LA-outer matching loop...")

# Track which files are written for each (treatment, matching_core, spec) combination
# Key: "treatment_name|matching_core|spec_name"  Value: character vector of file paths
files_registry <- list()

for (i in seq_along(la_files)) {
  la_name <- tools::file_path_sans_ext(basename(la_files[i]))
  if (i == 1L || i %% 10L == 0L) {
    elapsed <- as.numeric(difftime(Sys.time(), start.time, units = "secs"))
    message(sprintf("[%d/%d] Loading LA: %s  (%.0f s elapsed)", i, n_la_files, la_name, elapsed))
  }

  dt <- as.data.table(arrow::read_parquet(la_files[i]))
  dt <- define_treatments(dt)

  # Get the actual LA code for labelling
  la_label <- if ("local_authority" %in% names(dt) && nrow(dt) > 0) {
    as.character(dt$local_authority[1])
  } else {
    la_name
  }

  for (matching_core in names(matching_core_filters)) {
    matching_core_filter <- matching_core_filters[[matching_core]]
    core_data <- dt[eval(matching_core_filter)]
    if (nrow(core_data) == 0) next

    for (config in treatment_metadata) {
      treat_var <- config$var
      treatment_name <- config$file_id

      for (spec_config in spec_configs) {
        spec_name <- spec_config$name
        allowed_cores <- spec_core_pairs[[spec_name]]
        if (!(matching_core %in% allowed_cores)) next

        result <- match_single_la(
          dat = copy(core_data),
          la_label = la_label,
          treatment_var = treat_var,
          treatment_name = treatment_name,
          matching_core = matching_core,
          spec_config = spec_config,
          output_dir = output_dir,
          ccod_version = ccod_version
        )

        if (!is.null(result)) {
          registry_key <- paste0(treatment_name, "|", matching_core, "|", spec_name)
          files_registry[[registry_key]] <- c(files_registry[[registry_key]], result$file)
        }
      }
    }
  }

  rm(dt, core_data)
  gc()
}


# Recombine Results --------------------------------------------------------
# Combine per-LA RDS files into per-(treatment, matching_core) .RData files
# that the regression script expects.
message("Recombining per-LA matched pairs into aggregated files...")

# Group registry by (treatment_name, matching_core) — each may have multiple specs
treatment_core_keys <- unique(sub("\\|[^|]+$", "", names(files_registry)))

for (tc_key in treatment_core_keys) {
  parts <- strsplit(tc_key, "\\|")[[1]]
  treatment_name <- parts[1]
  matching_core <- parts[2]

  matched_results <- vector("list", length(spec_configs))
  names(matched_results) <- sapply(spec_configs, function(x) x$name)

  for (spec_name in names(matched_results)) {
    registry_key <- paste0(tc_key, "|", spec_name)
    fls <- files_registry[[registry_key]]

    if (is.null(fls) || length(fls) == 0) {
      matched_results[[spec_name]] <- data.table()
      next
    }

    dt_combined <- rbindlist(lapply(fls, readRDS), use.names = TRUE, fill = TRUE)
    matched_results[[spec_name]] <- dt_combined
    message(sprintf("  Combined %s | %s | %s: %d rows from %d LAs",
                    treatment_name, matching_core, spec_name, nrow(dt_combined), length(fls)))
    rm(dt_combined)
  }

  # Store matching core metadata
  matching_core_metadata <- list(
    matching_core_name = matching_core,
    matching_core_filter_expression = matching_core_filters[[matching_core]],
    timestamp = Sys.time()
  )

  # Save combined results
  output_file <- file.path(output_dir, paste0("matched_pairs_", treatment_name,
                                               "_matching_core_", matching_core,
                                               "_", ccod_version, ".RData"))
  save(matched_results, matching_core_metadata, file = output_file)
  message("  Saved: ", basename(output_file))
  rm(matched_results)
  gc()
}

message("Matching process completed for all treatment definitions.")

# DIAGNOSTICS: RUNTIME
end.time <- Sys.time()
time.taken <- end.time - start.time
message("\n Script 5 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")
