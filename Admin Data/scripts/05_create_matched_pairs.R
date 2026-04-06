# Script: 05_create_matched_pairs.R
# Purpose: Implement matching logic to create sets of matched pairs based on
#          treatment definitions with exact matching by local_authority.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last Updated February 27, 2026.

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()
# Set seed for reproducibility
set.seed(20230703)

# Runtime -----------------------------------------------------------------
start.time <- Sys.time()
run_start  <- format(start.time, "%Y-%m-%dT%H:%M:%S")

# Run ID: shared across scripts 05 and 06 when launched from run_analysis.R.
# Falls back to a local timestamp when executed standalone.
run_id <- local({
  rid <- Sys.getenv("PIPELINE_RUN_ID", unset = "")
  if (nzchar(rid)) rid else format(start.time, "run_%Y%m%d_%H%M%S")
})

# Source global setup, specifications, and treatment definitions
source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))
message("Sourced setup, model specifications, and treatment definitions.")


# Requirements ------------------------------------------------------------
library(data.table)
library(MatchIt)
library(arrow)
library(jsonlite)

# Inputs ------------------------------------------------------------------
ccod_version <- CCOD_VERSION
input_dir <- EPC_LA_REFINED_DIR
output_dir <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

la_files <- list.files(input_dir, pattern = "\\.parquet$", full.names = TRUE)
n_la_files <- length(la_files)

if (n_la_files == 0L) {
  stop("No per-LA Parquet files found in: ", input_dir)
}
message(sprintf("Found %d per-LA Parquet files.", n_la_files))

# Write run manifest (once per geography level; skipped on crash-resume).
manifest_path <- file.path(SUMMARY_TABLES_DIR,
                           paste0("run_manifest_05_", MATCHING_GEOGRAPHY, ".json"))
if (!file.exists(manifest_path)) {
  jsonlite::write_json(
    list(
      run_id            = run_id,
      script            = "05_create_matched_pairs.R",
      matching_geography = MATCHING_GEOGRAPHY,
      ccod_version      = CCOD_VERSION,
      full_sample       = FULL_SAMPLE,
      r_version         = R.version$version.string,
      start_time        = run_start,
      hostname          = Sys.info()[["nodename"]]
    ),
    manifest_path, auto_unbox = TRUE, pretty = TRUE
  )
  message(sprintf("  Run manifest written: %s", basename(manifest_path)))
} else {
  message(sprintf("  Run manifest exists (crash-resume): %s", basename(manifest_path)))
}


# Geographic Units --------------------------------------------------------

if (MATCHING_GEOGRAPHY == "LA") {
  unknown_mask <- grepl("unknown", basename(la_files), ignore.case = TRUE)
  if (any(unknown_mask)) {
    message(sprintf("  Skipping %d unknown-LA file(s): %s",
                    sum(unknown_mask),
                    paste(basename(la_files[unknown_mask]), collapse = ", ")))
    la_files <- la_files[!unknown_mask]
  }
  geo_units <- lapply(la_files, function(f) {
    list(files = f, label = tools::file_path_sans_ext(basename(f)))
  })
} else {
  # ITL columns are baked into parquet files by 02_merge_and_enhance.R.
  # Read the ITL column for the chosen geography level from each parquet file.
  ITL_col <- switch(MATCHING_GEOGRAPHY,
    "ITL3" = "ITL3", "ITL2" = "ITL2", "ITL1" = "ITL1",
    stop("Unknown MATCHING_GEOGRAPHY: ", MATCHING_GEOGRAPHY))

  # Verify ITL columns exist in the parquet files
  test_schema <- names(arrow::read_parquet(la_files[1], col_select = NULL, as_data_frame = FALSE))
  if (!ITL_col %in% test_schema) {
    stop(sprintf("Column '%s' not found in parquet files. Re-run 02_merge_and_enhance.R and 03_refine_and_parquet.R to add ITL columns.",
                 ITL_col))
  }

  la_map <- rbindlist(lapply(la_files, function(f) {
    cols <- arrow::read_parquet(f, col_select = all_of(c("local_authority", ITL_col)))
    data.table(file = f,
               local_authority = as.character(cols[[1]][1]),
               ITL = as.character(cols[[2]][1]))
  }))

  # Drop files with no ITL mapping (e.g., unknown LA)
  unmatched <- la_map[is.na(ITL) | ITL == ""]
  if (nrow(unmatched) > 0) {
    message(sprintf("  DROPPING %d LA files with no %s mapping:",
                    nrow(unmatched), MATCHING_GEOGRAPHY))
    for (j in seq_len(nrow(unmatched))) {
      message(sprintf("    %s (%s)", unmatched$local_authority[j], basename(unmatched$file[j])))
    }
    la_map <- la_map[!is.na(ITL) & ITL != ""]
  }

  message(sprintf("  %d LA files mapped to %s codes (from %d total).",
                  nrow(la_map), MATCHING_GEOGRAPHY, n_la_files))

  # Group by ITL code
  geo_units <- lapply(split(la_map, by = "ITL"), function(grp)
    list(files = grp$file, label = grp$ITL[1]))
}

message(sprintf("  %d geographic units (%s) from %d LA files.",
                length(geo_units), MATCHING_GEOGRAPHY, n_la_files))


# Helper: extract ONS/ITL code from geographic label ---------------------------
get_geo_code <- function(geo_label) {
  m <- regmatches(geo_label, regexpr("[A-Z][0-9]{8}", geo_label))
  if (length(m) == 1L) m else geo_label
}

# match_single_unit() -----------------------------------------------------
# Pure matching function. Diagnostics (balance, logs) handled by 05b post-hoc.
match_single_unit <- function(dat, geo_label, treatment_var, treatment_name,
                              treat_short_id, treatment_title,
                              matching_core, spec_config, output_dir, ccod_version,
                              min_treated_low, min_treated_high,
                              min_control_low, min_control_high) {

  spec_short <- spec_config$short_id
  core_short <- core_short_ids[[matching_core]]
  geo_code   <- get_geo_code(geo_label)

  fname <- file.path(output_dir,
    paste0("mu_", geo_code, "_", treat_short_id, "_", core_short, "_", spec_short, ".rds"))

  # Crash-resume: skip if file already exists
  if (file.exists(fname)) {
    return(list(file = fname, nrows = tryCatch(nrow(readRDS(fname)), error = function(e) NA_integer_)))
  }

  # Data Preparation
  base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                          "property_type", "main_fuel", "construction_age_band", "built_form")

  dat <- dat[!is.na(get(treatment_var))]
  dat <- dat[complete.cases(dat[, ..base_matching_vars])]

  required_vars <- unique(c(base_matching_vars, spec_config$continuous_vars, spec_config$exact_vars))
  required_vars <- setdiff(required_vars, "local_authority")
  dat <- na.omit(dat, cols = required_vars)

  if ("ppd_price_sqm" %in% spec_config$continuous_vars) {
    dat <- dat[!is.infinite(ppd_price_sqm)]
  }

  if (nrow(dat) == 0L) return(NULL)

  # Check Sample Sizes
  is_sparse <- ("ppd_price_sqm" %in% spec_config$continuous_vars) || ("tax_band" %in% spec_config$exact_vars)
  min_treated <- if (is_sparse) min_treated_low else min_treated_high
  min_control <- if (is_sparse) min_control_low else min_control_high

  n_treated <- nrow(dat[get(treatment_var) == 1])
  n_control <- nrow(dat[get(treatment_var) == 0])

  if (n_treated <= min_treated || n_control <= min_control) return(NULL)

  # Matching
  dat[, rand_sort := runif(.N)]
  setorder(dat, rand_sort)

  continuous_formula <- paste(spec_config$continuous_vars, collapse = " + ")
  exact_vars_no_la   <- setdiff(spec_config$exact_vars, "local_authority")

  tryCatch({
    if (length(exact_vars_no_la) > 0) {
      m <- matchit(
        as.formula(paste0(treatment_var, " ~ ", continuous_formula)),
        data     = dat,
        exact    = as.formula(paste0("~ ", paste(exact_vars_no_la, collapse = " + "))),
        method   = "nearest",
        distance = "glm"
      )
    } else {
      m <- matchit(
        as.formula(paste0(treatment_var, " ~ ", continuous_formula)),
        data     = dat,
        method   = "nearest",
        distance = "glm"
      )
    }

    md <- match.data(m)
    if (nrow(md) == 0) return(NULL)

    dt_out <- as.data.table(md)[, .(local_authority, uprn, distance, weights, subclass)]
    dt_out[, subclass := paste0(make.names(geo_label), "_", subclass)]
    saveRDS(dt_out, fname, compress = FALSE)
    n_saved <- nrow(dt_out)
    rm(m, md, dt_out)

    list(file = fname, nrows = n_saved)

  }, error = function(e) {
    message(sprintf("      - [ERROR] matchit() failed: %s", e$message))
    NULL
  })
}

# process_geo_unit() ------------------------------------------------------
process_geo_unit <- function(unit, matching_core_filters, treatment_metadata,
                             spec_configs, spec_core_pairs, output_dir, ccod_version,
                             min_treated_low, min_treated_high,
                             min_control_low, min_control_high) {

  geo_label <- unit$label
  geo_code  <- get_geo_code(geo_label)

  # Pre-check: if every expected .rds file already exists, rebuild registry
  # and return immediately — avoids reading parquets for complete units.
  pre_registry <- list()
  all_present  <- TRUE

  for (mc in names(matching_core_filters)) {
    cs <- core_short_ids[[mc]]
    for (cfg in treatment_metadata) {
      for (sc in spec_configs) {
        if (!(mc %in% spec_core_pairs[[sc$name]])) next
        fpath <- file.path(output_dir,
          paste0("mu_", geo_code, "_", cfg$short_id, "_", cs, "_", sc$short_id, ".rds"))
        key <- paste0(cfg$file_id, "|", mc, "|", sc$name)
        if (file.exists(fpath)) {
          pre_registry[[key]] <- c(pre_registry[[key]], fpath)
        } else {
          all_present <- FALSE
        }
      }
    }
  }

  if (all_present) {
    message(sprintf("  [%-15s] all files pre-skipped", geo_code))
    return(list(registry = pre_registry))
  }

  dt <- rbindlist(lapply(unit$files, function(f) as.data.table(arrow::read_parquet(f))))

  # Deduplicate UPRNs when multiple LA-vintage parquets map to the same ITL unit
  if (length(unit$files) > 1L && "uprn" %in% names(dt) && anyDuplicated(dt$uprn)) {
    n_before <- nrow(dt)
    if ("lodgement_datetime" %in% names(dt)) {
      setorder(dt, uprn, -lodgement_datetime)
    } else if ("lodgement_date" %in% names(dt)) {
      setorder(dt, uprn, -lodgement_date)
    }
    dt <- dt[!duplicated(uprn)]
    message(sprintf("  Dedup %s: %d -> %d rows (%d duplicate UPRNs removed)",
                    unit$label, n_before, nrow(dt), n_before - nrow(dt)))
  }

  dt <- define_treatments(dt)

  local_registry <- list()
  n_matched <- 0L
  n_skipped <- 0L
  n_null    <- 0L

  for (matching_core in names(matching_core_filters)) {
    core_data <- dt[eval(matching_core_filters[[matching_core]])]
    if (nrow(core_data) == 0) next

    for (config in treatment_metadata) {
      for (spec_config in spec_configs) {
        if (!(matching_core %in% spec_core_pairs[[spec_config$name]])) next

        result <- match_single_unit(
          dat              = copy(core_data),
          geo_label        = geo_label,
          treatment_var    = config$var,
          treatment_name   = config$file_id,
          treat_short_id   = config$short_id,
          treatment_title  = config$title,
          matching_core    = matching_core,
          spec_config      = spec_config,
          output_dir       = output_dir,
          ccod_version     = ccod_version,
          min_treated_low  = min_treated_low,
          min_treated_high = min_treated_high,
          min_control_low  = min_control_low,
          min_control_high = min_control_high
        )

        if (!is.null(result)) {
          key <- paste0(config$file_id, "|", matching_core, "|", spec_config$name)
          local_registry[[key]] <- c(local_registry[[key]], result$file)
          n_matched <- n_matched + 1L
        } else {
          n_null <- n_null + 1L
        }
      }
    }
  }

  message(sprintf("  [%-15s] matched=%d skipped/insuff=%d", geo_code, n_matched, n_null))

  rm(dt, core_data); gc()
  list(registry = local_registry)
}


# Matching Loop -----------------------------------------------------------
message(sprintf("Starting matching loop (%s geography, %d workers)...",
                MATCHING_GEOGRAPHY, MATCHING_N_WORKERS))

if (MATCHING_N_WORKERS > 1L) {
  library(parallel)
  n_workers <- min(MATCHING_N_WORKERS, length(geo_units))
  message(sprintf("  Starting parallel matching: %d workers, %d units", n_workers, length(geo_units)))

  cl <- makeCluster(n_workers, type = "PSOCK", outfile = "")
  on.exit(stopCluster(cl), add = TRUE)

  clusterExport(cl, c(
    "match_single_unit", "define_treatments", "process_geo_unit",
    "get_geo_code",
    "matching_core_filters", "treatment_metadata", "spec_configs", "spec_core_pairs",
    "core_short_ids", "run_id", "run_start",
    "output_dir", "ccod_version", "MATCHING_GEOGRAPHY",
    "MATCHING_MIN_TREATED_LOW", "MATCHING_MIN_TREATED_HIGH",
    "MATCHING_MIN_CONTROL_LOW", "MATCHING_MIN_CONTROL_HIGH"
  ), envir = environment())

  clusterEvalQ(cl, {
    library(data.table)
    library(MatchIt)
    library(arrow)
    set.seed(20230703)
  })

  results <- parLapply(cl, geo_units, function(unit) {
    tryCatch(
      process_geo_unit(unit, matching_core_filters, treatment_metadata,
                       spec_configs, spec_core_pairs, output_dir, ccod_version,
                       MATCHING_MIN_TREATED_LOW, MATCHING_MIN_TREATED_HIGH,
                       MATCHING_MIN_CONTROL_LOW, MATCHING_MIN_CONTROL_HIGH),
      error = function(e) {
        message(sprintf("  ERROR in unit %s: %s", unit$label, conditionMessage(e)))
        list(registry = list())
      }
    )
  })

  stopCluster(cl)
  on.exit(NULL)
} else {
  results <- vector("list", length(geo_units))
  for (i in seq_along(geo_units)) {
    message(sprintf("[%d/%d] %s  (%.0f s elapsed)",
                    i, length(geo_units), geo_units[[i]]$label,
                    as.numeric(difftime(Sys.time(), start.time, units = "secs"))))
    results[[i]] <- tryCatch(
      process_geo_unit(geo_units[[i]], matching_core_filters, treatment_metadata,
                       spec_configs, spec_core_pairs, output_dir, ccod_version,
                       MATCHING_MIN_TREATED_LOW, MATCHING_MIN_TREATED_HIGH,
                       MATCHING_MIN_CONTROL_LOW, MATCHING_MIN_CONTROL_HIGH),
      error = function(e) {
        message(sprintf("  ERROR in unit %s: %s", geo_units[[i]]$label, conditionMessage(e)))
        list(registry = list())
      }
    )
  }
}

files_registry <- list()
for (res in results) {
  for (key in names(res$registry)) {
    files_registry[[key]] <- c(files_registry[[key]], res$registry[[key]])
  }
}


# Recombine Results -------------------------------------------------------
# Combine per-unit RDS files into per-(treatment, matching_core) .RData files
# that the regression script expects.
message("Recombining per-unit matched pairs into aggregated files...")

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
    message(sprintf("  Combined %s | %s | %s: %d rows from %d units",
                    treatment_name, matching_core, spec_name, nrow(dt_combined), length(fls)))
    rm(dt_combined)
  }

  # Store matching core metadata
  matching_core_metadata <- list(
    matching_core_name = matching_core,
    matching_core_filter_expression = matching_core_filters[[matching_core]],
    matching_geography = MATCHING_GEOGRAPHY,
    timestamp = Sys.time()
  )

  # Save combined results
  output_file <- file.path(output_dir, paste0("matched_pairs_", treatment_name,
                                               "_matching_core_", matching_core,
                                               "_", ccod_version, ".RData"))
  if (file.exists(output_file)) {
    message("  [SKIP] Already exists: ", basename(output_file))
    rm(matched_results)
    gc()
    next
  }
  save(matched_results, matching_core_metadata, file = output_file)
  message("  Saved: ", basename(output_file))
  rm(matched_results)
  gc()
}

message("Matching process completed for all treatment definitions.")

# Runtime -----------------------------------------------------------------
end.time <- Sys.time()
time.taken <- end.time - start.time
message("\n Script 5 runtime: ", round(time.taken, 2), " ", units(time.taken), ".")
