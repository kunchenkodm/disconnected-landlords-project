# Script: 02_compute_global_stats.R
# Purpose: Lightweight first pass over all EPC CSVs to compute global energy
#          efficiency statistics (mean, SD, per-band worse_gap stats). Reads
#          only 4 columns per LA. Saves global_epc_stats.rds for use by
#          03_build_analysis_dataset.R.
# Authors: Thiemo Fetzer, Dmytro Kunchenko
# Date: March 13, 2026.

rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)


# Inputs ------------------------------------------------------------------
EPC_path <- file.path(RAW_EPC_DIR, "domestic-EPC")
global_stats_path <- file.path(PROCESSED_DATA_DIR, "global_epc_stats.rds")


# Enumerate EPC folders ---------------------------------------------------
epc_folders <- list.files(path = EPC_path, pattern = "^domestic", full.names = TRUE)
n_folders   <- length(epc_folders)

if (n_folders == 0L) {
  stop("No domestic-* folders found in EPC_path: ", EPC_path)
}

message(sprintf("Computing global EPC statistics from %d LA folders...", n_folders))
process_start <- proc.time()


# Accumulators ------------------------------------------------------------
global_n      <- 0L
global_sum    <- 0
global_sum_sq <- 0
band_stats_accum <- data.table(
  current_energy_rating = character(),
  n = integer(),
  sum_gap = numeric(),
  sum_sq_gap = numeric()
)


# Per-LA loop: read only the 4 columns needed -----------------------------
for (i in seq_along(epc_folders)) {
  folder    <- epc_folders[[i]]
  cert_file <- file.path(folder, "certificates.csv")

  if (!file.exists(cert_file)) {
    next
  }

  if (i == 1L || i %% 50L == 0L) {
    elapsed <- (proc.time() - process_start)[["elapsed"]]
    message(sprintf("[%d/%d] Reading stats (%.0f s elapsed)", i, n_folders, elapsed))
  }

  epc <- fread(cert_file, showProgress = FALSE,
               select = c("CURRENT_ENERGY_EFFICIENCY", "CURRENT_ENERGY_RATING",
                           "BUILDING_REFERENCE_NUMBER", "LODGEMENT_DATETIME"))

  # Deduplicate: keep most recent EPC per building
  setorder(epc, BUILDING_REFERENCE_NUMBER, LODGEMENT_DATETIME)
  epc <- epc[, .SD[.N], by = BUILDING_REFERENCE_NUMBER]

  # Accumulate global energy efficiency sums
  vals <- epc$CURRENT_ENERGY_EFFICIENCY[!is.na(epc$CURRENT_ENERGY_EFFICIENCY)]
  global_n      <- global_n      + length(vals)
  global_sum    <- global_sum    + sum(vals)
  global_sum_sq <- global_sum_sq + sum(vals^2)

  # Compute worse_gap for per-band stats
  epc[, worse_gap := fcase(
    is.na(CURRENT_ENERGY_EFFICIENCY), NA_real_,
    CURRENT_ENERGY_EFFICIENCY >= 92, CURRENT_ENERGY_EFFICIENCY - 91,
    CURRENT_ENERGY_EFFICIENCY >= 81, CURRENT_ENERGY_EFFICIENCY - 80,
    CURRENT_ENERGY_EFFICIENCY >= 69, CURRENT_ENERGY_EFFICIENCY - 68,
    CURRENT_ENERGY_EFFICIENCY >= 55, CURRENT_ENERGY_EFFICIENCY - 54,
    CURRENT_ENERGY_EFFICIENCY >= 39, CURRENT_ENERGY_EFFICIENCY - 38,
    CURRENT_ENERGY_EFFICIENCY >= 21, CURRENT_ENERGY_EFFICIENCY - 20,
    default = 0
  )]

  la_band <- epc[!is.na(worse_gap), .(
    n = .N,
    sum_gap = sum(worse_gap),
    sum_sq_gap = sum(worse_gap^2)
  ), by = .(current_energy_rating = CURRENT_ENERGY_RATING)]

  band_stats_accum <- rbindlist(list(band_stats_accum, la_band), use.names = TRUE)

  rm(epc, vals, la_band)
  gc()
}


# Compute global statistics -----------------------------------------------
message("Computing global energy efficiency statistics...")

global_mean <- global_sum / global_n
global_var  <- (global_sum_sq - global_n * global_mean^2) / (global_n - 1)
global_sd   <- sqrt(global_var)
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


# Save global stats -------------------------------------------------------
global_stats <- list(
  global_half_sd = global_half_sd,
  epc_lookup = epc_lookup,
  global_n = global_n,
  global_mean = global_mean,
  global_sd = global_sd
)
saveRDS(global_stats, global_stats_path)

elapsed_total <- (proc.time() - process_start)[["elapsed"]]
message(sprintf("Saved global EPC statistics to: %s (%.0f s)", global_stats_path, elapsed_total))
