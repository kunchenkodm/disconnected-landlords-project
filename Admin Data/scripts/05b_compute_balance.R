# Script: 05b_compute_balance.R
# Purpose: Post-hoc balance diagnostics from saved .rds matched-pair files.
#          Reads per-unit .rds files + parquet source data, computes SMD,
#          variance ratios, and eCDF metrics, writes matching_balance and
#          matching_log CSVs with LONG names (no short→long translation needed).
#
# Run after 05_create_matched_pairs.R.  No dependency on matchit objects.
# Invoked by run_analysis.R (Phase 2) or standalone.
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: March 27, 2026.
rm(list = setdiff(ls(), c("script", "pipeline.start.time")))
gc()

set.seed(20230703)
start.time <- Sys.time()

library(data.table)
library(arrow)
library(here)

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))

message("===================================================================")
message("  05b_compute_balance.R - Post-hoc Balance Diagnostics")
message(sprintf("  Geography: %s", MATCHING_GEOGRAPHY))
message(sprintf("  Started:   %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("===================================================================")


# Reverse lookups: short ID → treatment metadata --------------------------
treat_meta_by_short <- setNames(treatment_metadata,
                                vapply(treatment_metadata, `[[`, "", "short_id"))

# spec_config by short_id
spec_by_short <- setNames(spec_configs,
                          vapply(spec_configs, `[[`, "", "short_id"))


# 1. Index .rds files -----------------------------------------------------
rds_dir <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY)
rds_files <- list.files(rds_dir, pattern = "^mu_.*\\.rds$", full.names = TRUE)

if (length(rds_files) == 0L) {
  stop("No mu_*.rds files found in: ", rds_dir,
       "\n  Run 05_create_matched_pairs.R first.")
}
message(sprintf("  Found %d .rds files in %s", length(rds_files), basename(rds_dir)))

# Parse filenames: mu_<geo_code>_<treat_short>_<core_short>_<spec_short>.rds
parse_rds_name <- function(path) {
  bn <- tools::file_path_sans_ext(basename(path))
  # Pattern: mu_<geo_code>_<treat>_<core>_<spec>
  # geo_code can contain underscores (rare), so match greedily from the right
  parts <- strsplit(bn, "_")[[1L]]
  n <- length(parts)
  if (n < 5L) return(NULL)
  spec_short  <- parts[n]
  core_short  <- parts[n - 1L]
  treat_short <- parts[n - 2L]
  geo_code    <- paste(parts[2:(n - 3L)], collapse = "_")
  list(path = path, geo_code = geo_code,
       treat_short = treat_short, core_short = core_short, spec_short = spec_short)
}

rds_index <- Filter(Negate(is.null), lapply(rds_files, parse_rds_name))
message(sprintf("  Parsed %d / %d filenames successfully.", length(rds_index), length(rds_files)))

# Validate all short IDs map to known configs
valid <- vapply(rds_index, function(r) {
  r$treat_short %in% names(treat_meta_by_short) &&
    r$core_short %in% core_short_ids &&
    r$spec_short %in% names(spec_by_short)
}, logical(1L))

if (any(!valid)) {
  n_bad <- sum(!valid)
  message(sprintf("  WARNING: %d .rds files have unrecognised short IDs — skipping.", n_bad))
  rds_index <- rds_index[valid]
}


# 2. Load parquet data once -----------------------------------------------
message("  Loading parquet data...")
t_load <- Sys.time()

input_dir <- EPC_LA_REFINED_DIR
ds <- arrow::open_dataset(input_dir, format = "parquet")

# Columns needed for treatment definitions + matching covariates
treatment_dep_cols <- c("source", "tenure_2", "coarse_proprietorship",
                        "country_incorporated_1",
                        "country_incorporated_tax_haven",
                        "country_incorporated_british_haven",
                        "country_incorporated_european_haven",
                        "country_incorporated_caribbean_haven",
                        "country_incorporated_other_haven")

matching_cov_cols <- c("number_habitable_rooms", "total_floor_area",
                       "lodgement_year", "property_type", "main_fuel",
                       "construction_age_band", "built_form",
                       "tax_band", "ppd_price_sqm", "ppd_year_transfer")

geo_col <- if (MATCHING_GEOGRAPHY == "LA") "local_authority" else MATCHING_GEOGRAPHY

needed_cols <- unique(c("uprn", "local_authority", geo_col,
                        treatment_dep_cols, matching_cov_cols))

available_cols <- intersect(needed_cols, names(ds))
missing_cols <- setdiff(needed_cols, available_cols)
if (length(missing_cols) > 0L) {
  message(sprintf("  NOTE: %d columns not in parquet (will be NA): %s",
                  length(missing_cols), paste(missing_cols, collapse = ", ")))
}

DT <- ds |> dplyr::select(dplyr::all_of(available_cols)) |> dplyr::collect() |> as.data.table()
message(sprintf("  Loaded %s rows x %d cols (%.1f s)",
                formatC(nrow(DT), big.mark = ","), ncol(DT),
                as.numeric(difftime(Sys.time(), t_load, units = "secs"))))

DT <- define_treatments(DT)

# Ensure geo_col values are character for matching with geo_code
DT[, (geo_col) := as.character(get(geo_col))]


# 3. Helper: compute per-covariate balance --------------------------------
compute_balance <- function(before_t, before_c, after_t, after_c, covariates) {
  # Replicates MatchIt's standardised balance metrics.
  # Denominator for SMD: pooled SD from FULL (before) sample.
  rows <- vector("list", length(covariates))
  for (i in seq_along(covariates)) {
    cov <- covariates[i]
    bt <- before_t[[cov]]; bc <- before_c[[cov]]
    at <- after_t[[cov]];  ac <- after_c[[cov]]

    # Remove NAs for safety
    bt <- bt[!is.na(bt)]; bc <- bc[!is.na(bc)]
    at <- at[!is.na(at)]; ac <- ac[!is.na(ac)]

    if (length(bt) < 2L || length(bc) < 2L) {
      rows[[i]] <- data.table(covariate = cov,
        smd_before = NA_real_, smd_after = NA_real_,
        var_ratio_before = NA_real_, var_ratio_after = NA_real_,
        ecdf_mean_before = NA_real_, ecdf_mean_after = NA_real_,
        ecdf_max_before = NA_real_, ecdf_max_after = NA_real_)
      next
    }

    var_bt <- var(bt); var_bc <- var(bc)
    pooled_sd <- sqrt((var_bt + var_bc) / 2)

    # SMD
    smd_before <- if (pooled_sd > 0) (mean(bt) - mean(bc)) / pooled_sd else NA_real_
    smd_after  <- if (pooled_sd > 0 && length(at) > 0L && length(ac) > 0L)
                    (mean(at) - mean(ac)) / pooled_sd else NA_real_

    # Variance ratio
    vr_before <- if (var_bc > 0) var_bt / var_bc else NA_real_
    vr_after  <- if (length(at) > 1L && length(ac) > 1L) {
                   vac <- var(ac)
                   if (vac > 0) var(at) / vac else NA_real_
                 } else NA_real_

    # eCDF
    ecdf_metrics <- function(x_t, x_c) {
      if (length(x_t) == 0L || length(x_c) == 0L) return(list(mean = NA_real_, max = NA_real_))
      all_vals <- sort(unique(c(x_t, x_c)))
      et <- ecdf(x_t); ec <- ecdf(x_c)
      diffs <- abs(et(all_vals) - ec(all_vals))
      list(mean = mean(diffs), max = max(diffs))
    }
    ecdf_b <- ecdf_metrics(bt, bc)
    ecdf_a <- ecdf_metrics(at, ac)

    rows[[i]] <- data.table(covariate = cov,
      smd_before = smd_before, smd_after = smd_after,
      var_ratio_before = vr_before, var_ratio_after = vr_after,
      ecdf_mean_before = ecdf_b$mean, ecdf_mean_after = ecdf_a$mean,
      ecdf_max_before = ecdf_b$max, ecdf_max_after = ecdf_a$max)
  }
  rbindlist(rows)
}


# 4. Process .rds files grouped by geo_code --------------------------------
# Group by geo_code so we subset DT once per unit
rds_by_geo <- split(rds_index, vapply(rds_index, `[[`, "", "geo_code"))

balance_all <- vector("list", length(rds_index))
log_all     <- vector("list", length(rds_index))
idx <- 0L
n_total <- length(rds_index)
n_geo   <- length(rds_by_geo)
t_loop  <- Sys.time()

base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                        "property_type", "main_fuel", "construction_age_band", "built_form")

for (gi in seq_along(rds_by_geo)) {
  geo_code <- names(rds_by_geo)[gi]
  geo_rds  <- rds_by_geo[[gi]]

  # Subset data to this geo unit (deduplicate by UPRN — parquet may have multiple EPCs)
  geo_dt <- DT[get(geo_col) == geo_code]
  if (nrow(geo_dt) == 0L) {
    message(sprintf("  [%d/%d] %-15s SKIP (no parquet rows)", gi, n_geo, geo_code))
    next
  }
  if (anyDuplicated(geo_dt$uprn)) {
    geo_dt <- geo_dt[!duplicated(uprn)]
  }

  for (r in geo_rds) {
    idx <- idx + 1L
    treat_short <- r$treat_short
    core_short  <- r$core_short
    spec_short  <- r$spec_short

    # Resolve to long names and configs
    core_name <- core_long_from_short[[core_short]]
    spec_name <- spec_long_from_short[[spec_short]]
    treat_meta <- treat_meta_by_short[[treat_short]]
    spec_cfg   <- spec_by_short[[spec_short]]
    treatment_var <- treat_meta$var

    # Read matched UPRNs
    matched <- tryCatch(readRDS(r$path), error = function(e) NULL)
    if (is.null(matched) || nrow(matched) == 0L) {
      log_all[[idx]] <- data.table(
        geo_level = MATCHING_GEOGRAPHY, geo_code = geo_code,
        treatment_short_id = treat_short, matching_core = core_name, spec = spec_name,
        status = "empty_rds", n_matched = 0L)
      next
    }

    # Reconstruct "before" sample (same filtering as match_single_unit)
    before <- geo_dt[!is.na(get(treatment_var))]
    before <- before[eval(matching_core_filters[[core_name]])]
    before <- before[complete.cases(before[, ..base_matching_vars])]

    spec_vars <- unique(c(base_matching_vars, spec_cfg$continuous_vars, spec_cfg$exact_vars))
    spec_vars <- setdiff(spec_vars, "local_authority")
    before <- na.omit(before, cols = spec_vars)

    if ("ppd_price_sqm" %in% spec_cfg$continuous_vars) {
      before <- before[!is.infinite(ppd_price_sqm)]
    }

    if (nrow(before) == 0L) {
      log_all[[idx]] <- data.table(
        geo_level = MATCHING_GEOGRAPHY, geo_code = geo_code,
        treatment_short_id = treat_short, matching_core = core_name, spec = spec_name,
        status = "no_before_sample", n_matched = nrow(matched))
      next
    }

    # "after" sample: inner join matched UPRNs to before data
    # Deduplicate by UPRN — control units may appear multiple times in match.data()
    matched_uprns <- unique(matched[, .(uprn, distance)], by = "uprn")
    after <- before[uprn %in% matched_uprns$uprn]

    if (nrow(after) == 0L) {
      log_all[[idx]] <- data.table(
        geo_level = MATCHING_GEOGRAPHY, geo_code = geo_code,
        treatment_short_id = treat_short, matching_core = core_name, spec = spec_name,
        status = "no_after_join", n_matched = nrow(matched),
        n_before = nrow(before))
      next
    }

    # Split before/after by treatment
    before_t <- before[get(treatment_var) == 1L]
    before_c <- before[get(treatment_var) == 0L]
    after_t  <- after[get(treatment_var) == 1L]
    after_c  <- after[get(treatment_var) == 0L]

    # Covariates to check balance on (formula covariates only; exact vars balanced by construction)
    bal_covs <- spec_cfg$continuous_vars

    bal_dt <- compute_balance(before_t, before_c, after_t, after_c, bal_covs)

    # Add distance (propensity score) — "before" metrics NA since we lack full-sample PS
    dist_matched <- matched_uprns[uprn %in% after$uprn]
    after_with_dist <- merge(after[, .(uprn, tv = get(treatment_var))],
                             dist_matched, by = "uprn")
    dist_at <- after_with_dist[tv == 1L, distance]
    dist_ac <- after_with_dist[tv == 0L, distance]

    dist_row <- data.table(covariate = "distance",
      smd_before = NA_real_, smd_after = NA_real_,
      var_ratio_before = NA_real_, var_ratio_after = NA_real_,
      ecdf_mean_before = NA_real_, ecdf_mean_after = NA_real_,
      ecdf_max_before = NA_real_, ecdf_max_after = NA_real_)

    if (length(dist_at) > 1L && length(dist_ac) > 1L) {
      var_dt <- var(dist_at); var_dc <- var(dist_ac)
      ps <- sqrt((var_dt + var_dc) / 2)
      if (ps > 0) dist_row$smd_after <- (mean(dist_at) - mean(dist_ac)) / ps
      if (var_dc > 0) dist_row$var_ratio_after <- var_dt / var_dc
      all_v <- sort(unique(c(dist_at, dist_ac)))
      et <- ecdf(dist_at); ec <- ecdf(dist_ac)
      dd <- abs(et(all_v) - ec(all_v))
      dist_row$ecdf_mean_after <- mean(dd)
      dist_row$ecdf_max_after  <- max(dd)
    }

    bal_dt <- rbind(bal_dt, dist_row)

    # Tag with identifiers (long names)
    bal_dt[, `:=`(geo_level = MATCHING_GEOGRAPHY, geo_code = geo_code,
                  treatment_short_id = treat_short,
                  matching_core = core_name, spec = spec_name)]

    balance_all[[idx]] <- bal_dt

    # Aggregate log row (substantive covariates only, excluding distance)
    subst <- bal_dt[covariate != "distance"]
    subst_abs_smd <- abs(subst$smd_after)

    log_all[[idx]] <- data.table(
      geo_level              = MATCHING_GEOGRAPHY,
      geo_code               = geo_code,
      treatment_short_id     = treat_short,
      matching_core          = core_name,
      spec                   = spec_name,
      status                 = "success",
      n_before               = nrow(before),
      n_treated_before       = nrow(before_t),
      n_control_before       = nrow(before_c),
      n_matched              = nrow(matched),
      n_matched_treated      = nrow(after_t),
      n_matched_control      = nrow(after_c),
      match_rate             = if (nrow(before_t) > 0L) nrow(after_t) / nrow(before_t) else NA_real_,
      smd_max_before         = if (nrow(subst) > 0L) max(abs(subst$smd_before), na.rm = TRUE) else NA_real_,
      smd_max_after          = if (nrow(subst) > 0L) max(subst_abs_smd, na.rm = TRUE) else NA_real_,
      smd_mean_after         = if (nrow(subst) > 0L) mean(subst_abs_smd, na.rm = TRUE) else NA_real_,
      smd_median_after       = if (nrow(subst) > 0L) median(subst_abs_smd, na.rm = TRUE) else NA_real_,
      frac_covs_balanced_01  = if (nrow(subst) > 0L) mean(subst_abs_smd < 0.1, na.rm = TRUE) else NA_real_,
      frac_covs_balanced_025 = if (nrow(subst) > 0L) mean(subst_abs_smd < 0.25, na.rm = TRUE) else NA_real_,
      n_covariates           = nrow(subst)
    )
  }

  if (gi %% 20L == 0L || gi == n_geo) {
    elapsed <- as.numeric(difftime(Sys.time(), t_loop, units = "secs"))
    message(sprintf("  [%d/%d geo units] %d/%d files (%.0f s)",
                    gi, n_geo, idx, n_total, elapsed))
  }
}


# 5. Write outputs atomically ---------------------------------------------
balance_dt <- rbindlist(Filter(Negate(is.null), balance_all), fill = TRUE)
log_dt     <- rbindlist(Filter(Negate(is.null), log_all), fill = TRUE)

balance_file <- file.path(rds_dir, paste0("matching_balance_", MATCHING_GEOGRAPHY, ".csv"))
log_file     <- file.path(rds_dir, paste0("matching_log_", MATCHING_GEOGRAPHY, ".csv"))

fwrite(balance_dt, balance_file)
message(sprintf("  Balance written: %s (%s rows)",
                basename(balance_file), formatC(nrow(balance_dt), big.mark = ",")))

fwrite(log_dt, log_file)
message(sprintf("  Log written: %s (%s rows)",
                basename(log_file), formatC(nrow(log_dt), big.mark = ",")))

# Summary
if (nrow(log_dt) > 0L) {
  status_tbl <- log_dt[, .N, by = status]
  for (i in seq_len(nrow(status_tbl))) {
    message(sprintf("    %s: %d", status_tbl$status[i], status_tbl$N[i]))
  }
}

rm(DT, balance_all, log_all, balance_dt, log_dt)
gc()

end.time <- Sys.time()
message(sprintf("\n  05b_compute_balance.R complete [%s]. Runtime: %.1f min.",
                MATCHING_GEOGRAPHY,
                as.numeric(difftime(end.time, start.time, units = "mins"))))
