# Script: 04_feature_refinement.R
# Purpose: Refine per-LA enhanced datasets by adding derived features for
#          matching and analysis, removing unused columns, and saving as Parquet.
#          Uses a two-pass approach: pass 1 collects global statistics needed for
#          borderline EPC variables, pass 2 creates all features and writes Parquet.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: July 3, 2025. Last updated Febuary 20, 2026.

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(arrow)

# Inputs ------------------------------------------------------------------
input_dir <- EPC_LA_ENHANCED_DIR
output_dir <- EPC_LA_REFINED_DIR

la_files <- list.files(input_dir, pattern = "\\.rds$", full.names = TRUE)
n_files <- length(la_files)

if (n_files == 0L) {
  stop("No per-LA enhanced RDS files found in: ", input_dir)
}


# Pass 1: Collect Global Statistics ----------------------------------------
message("Pass 1: Collecting global statistics across all LAs...")
pass1_start <- proc.time()

# Accumulators for global SD of current_energy_efficiency
global_n <- 0L
global_sum <- 0
global_sum_sq <- 0

# Accumulators for per-band SD of energy_efficiency_worse_epc_gap
band_stats_accum <- data.table(
  current_energy_rating = character(),
  n = integer(),
  sum_gap = numeric(),
  sum_sq_gap = numeric()
)

for (i in seq_along(la_files)) {
  dt <- readRDS(la_files[i])
  setDT(dt)

  # Global stats: current_energy_efficiency
  vals <- dt$current_energy_efficiency[!is.na(dt$current_energy_efficiency)]
  global_n <- global_n + length(vals)
  global_sum <- global_sum + sum(vals)
  global_sum_sq <- global_sum_sq + sum(vals^2)

  # Compute energy_efficiency_worse_epc_gap on the fly
  dt[, worse_gap := fcase(
    is.na(current_energy_efficiency), NA_real_,
    current_energy_efficiency >= 92, current_energy_efficiency - 91,
    current_energy_efficiency >= 81, current_energy_efficiency - 80,
    current_energy_efficiency >= 69, current_energy_efficiency - 68,
    current_energy_efficiency >= 55, current_energy_efficiency - 54,
    current_energy_efficiency >= 39, current_energy_efficiency - 38,
    current_energy_efficiency >= 21, current_energy_efficiency - 20,
    default = 0
  )]

  # Per-band sufficient statistics
  la_band <- dt[!is.na(worse_gap), .(
    n = .N,
    sum_gap = sum(worse_gap),
    sum_sq_gap = sum(worse_gap^2)
  ), by = current_energy_rating]

  band_stats_accum <- rbindlist(list(band_stats_accum, la_band), use.names = TRUE)

  rm(dt, vals, la_band)
  gc()

  if (i %% 50L == 0L) {
    elapsed <- (proc.time() - pass1_start)[["elapsed"]]
    message(sprintf("  Pass 1: %d / %d files (%.0f s)", i, n_files, elapsed))
  }
}

# Compute global SD
global_mean <- global_sum / global_n
global_var <- (global_sum_sq - global_n * global_mean^2) / (global_n - 1)
global_sd <- sqrt(global_var)
global_half_sd <- 0.5 * global_sd
message(sprintf("  Global energy efficiency: n=%d, mean=%.2f, sd=%.2f",
                global_n, global_mean, global_sd))

# Combine per-band statistics
band_combined <- band_stats_accum[, .(
  n = sum(n),
  sum_gap = sum(sum_gap),
  sum_sq_gap = sum(sum_sq_gap)
), by = current_energy_rating]
band_combined[, mean_gap := sum_gap / n]
band_combined[, sd_gap := sqrt((sum_sq_gap - n * mean_gap^2) / (n - 1))]

# EPC cutoffs definition
epc_cutoffs <- data.table(
  current_energy_rating = c("A", "B", "C", "D", "E", "F", "G"),
  lower_cutoff = c(92, 81, 69, 55, 39, 21, 0)
)

# Merge SDs + cutoffs
epc_info <- merge(epc_cutoffs, band_combined[, .(current_energy_rating, sd_gap, n)],
                  by = "current_energy_rating", all.x = TRUE)

# Compute pooled SDs for adjacent bands
pooled_sd_fn <- function(sd1, n1, sd2, n2) {
  sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
}

setorder(epc_info, -lower_cutoff)  # ensure descending order (A -> G)
epc_info[, pooled_sd := fifelse(
  current_energy_rating != "G",
  pooled_sd_fn(sd_gap, n, shift(sd_gap, type = "lead"), shift(n, type = "lead")),
  NA_real_
)]

# Define bandwidth: Minimum of (0.5 * Pooled SD) and (0.5 * Global SD)
epc_info[, half_sd := pmin(0.5 * pooled_sd, global_half_sd)]

# Keep only columns needed for the per-LA merge
epc_lookup <- epc_info[, .(current_energy_rating, lower_cutoff, half_sd)]

rm(band_stats_accum, band_combined, epc_cutoffs, epc_info)
elapsed_pass1 <- (proc.time() - pass1_start)[["elapsed"]]
message(sprintf("Pass 1 complete (%.0f s). Global half-SD = %.4f", elapsed_pass1, global_half_sd))


# Pass 2: Derived Features & Parquet ---------------------------------------
message("Pass 2: Creating derived features and writing Parquet per-LA...")
pass2_start <- proc.time()

for (i in seq_along(la_files)) {
  la_name <- tools::file_path_sans_ext(basename(la_files[i]))
  out_path <- file.path(output_dir, paste0(la_name, ".parquet"))

  # Crash-resume: skip if output already exists
  if (file.exists(out_path)) {
    if (i == 1L || i %% 50L == 0L) {
      message(sprintf("[%d/%d] exists, skipping: %s", i, n_files, la_name))
    }
    next
  }

  if (i == 1L || i %% 10L == 0L) {
    elapsed <- (proc.time() - pass2_start)[["elapsed"]]
    message(sprintf("[%d/%d] Refining: %s  (%.0f s elapsed)", i, n_files, la_name, elapsed))
  }

  dt <- readRDS(la_files[i])
  setDT(dt)

  ## Per-row feature creation

  # Energy Efficiency Dummies: Binary indicators for poor or very poor ratings
  dt[!is.na(roof_energy_eff), roof_energy_eff_dum := as.numeric(roof_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(walls_energy_eff), walls_energy_eff_dum := as.numeric(walls_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(hot_water_energy_eff), hot_water_energy_eff_dum := as.numeric(hot_water_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(mainheat_energy_eff), mainheat_energy_eff_dum := as.numeric(mainheat_energy_eff %in% c("Poor", "Very Poor"))]
  dt[!is.na(windows_energy_eff), windows_energy_eff_dum := as.numeric(windows_energy_eff %in% c("Poor", "Very Poor"))]

  # Normalize multi_glaze_proportion
  dt[, multi_glaze_proportion := multi_glaze_proportion / 100]

  # Energy consumption variables
  dt[, energy_consumption_current_property   := energy_consumption_current   * total_floor_area]
  dt[, energy_consumption_potential_property := energy_consumption_potential * total_floor_area]

  # Gap variables
  dt[, energy_consumption_gap          := energy_consumption_potential - energy_consumption_current]
  dt[, energy_consumption_gap_property := energy_consumption_potential_property - energy_consumption_current_property]
  dt[, energy_efficiency_potential_gap := potential_energy_efficiency - current_energy_efficiency]

  # Gap between current efficiency and borderline for EPC < C (<=68 numeric)
  dt[, energy_efficiency_bad_epc_gap := current_energy_efficiency - 68]

  # Gap between current efficiency and borderline for next worst EPC rating
  dt[, energy_efficiency_worse_epc_gap := fcase(
    is.na(current_energy_efficiency), NA_real_,
    current_energy_efficiency >= 92, current_energy_efficiency - 91,
    current_energy_efficiency >= 81, current_energy_efficiency - 80,
    current_energy_efficiency >= 69, current_energy_efficiency - 68,
    current_energy_efficiency >= 55, current_energy_efficiency - 54,
    current_energy_efficiency >= 39, current_energy_efficiency - 38,
    current_energy_efficiency >= 21, current_energy_efficiency - 20,
    default = 0
  )]

  ## Borderline variables

  # borderline_good_epc: just above C threshold (69) using global SD
  dt[, borderline_good_epc := fcase(
    current_energy_efficiency > 69 &
      current_energy_efficiency <= 69 + global_half_sd, 1,
    is.na(current_energy_efficiency), NA_real_,
    default = 0
  )]

  # borderline_better_epc: just above each band's lower cutoff using local bandwidth
  dt <- merge(dt, epc_lookup, by = "current_energy_rating", all.x = TRUE)

  dt[, borderline_better_epc := fcase(
    current_energy_efficiency > lower_cutoff &
      current_energy_efficiency <= lower_cutoff + half_sd, 1,
    is.na(current_energy_efficiency), NA_real_,
    default = 0
  )]

  # Clean up helper columns
  dt[, c("lower_cutoff", "half_sd") := NULL]

  ## Remove unused columns
  description_cols <- grep("_description", names(dt), value = TRUE)
  unused_specific <- c("fixed_lighting_outlets_count", "low_energy_fixed_light_count")
  cols_to_remove <- intersect(c(description_cols, unused_specific), names(dt))
  if (length(cols_to_remove) > 0) {
    dt[, (cols_to_remove) := NULL]
  }

  # Write Parquet
  arrow::write_parquet(dt, out_path)
  rm(dt)
  gc()
}

elapsed_pass2 <- (proc.time() - pass2_start)[["elapsed"]]
n_output <- length(list.files(output_dir, pattern = "\\.parquet$"))
message(sprintf("Pass 2 complete: %d Parquet files in %.0f s. Output: %s",
                n_output, elapsed_pass2, output_dir))
