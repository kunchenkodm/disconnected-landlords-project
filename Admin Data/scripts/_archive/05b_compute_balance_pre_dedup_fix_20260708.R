# Script: 05b_compute_balance.R
# Purpose: Post-hoc balance diagnostics AND attrition accounting from saved
#          .rds matched-pair files. Iterates the FULL grid of
#          (geo unit x treatment x valid core-spec pair) — combos with no
#          .rds file get diagnostic status rows so the matching funnel is
#          complete (PI feedback, attrition workstream). Writes:
#            - matching_balance_<GEO>.csv  per-covariate SMD/variance/eCDF
#            - matching_log_<GEO>.csv      per-combo funnel counts + balance
#            - attrition_funnel_<GEO>.csv  pooled per (treatment, core, spec)
#              stage counts (raw -> complete-case -> gate -> matched ->
#              caliper 0.2 -> caliper 0.1) and LA survival counts
#
# Run after 05_create_matched_pairs.R (and, for n_las_regression, after
# 06_run_regressions.R). No dependency on matchit objects.
# Invoked by run_analysis.R (RUN_BALANCE phase) or standalone.
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: March 27, 2026. Last Updated: July 7, 2026 (attrition workstream).
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

if (WITHIN_CORPORATE) {
  message(">>> WITHIN_CORPORATE mode: using within-corporate treatment metadata.")
  define_treatments  <- define_within_corporate_treatments
  treatment_metadata <- within_corporate_metadata
}
wc_suffix <- if (WITHIN_CORPORATE) "_wc" else ""

message("===================================================================")
message("  05b_compute_balance.R - Balance Diagnostics + Attrition Funnel")
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

# Lookup: combo key -> rds path (full-grid iteration checks existence here)
rds_lookup <- new.env(hash = TRUE, parent = emptyenv(),
                      size = max(2L * length(rds_index), 16L))
for (r in rds_index) {
  rds_lookup[[paste(r$geo_code, r$treat_short, r$core_short, r$spec_short,
                    sep = "|")]] <- r$path
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


# 4. Full-grid iteration: attrition counts + balance -----------------------
# Geo-unit universe: all codes present in the data plus any that have .rds
# files (belt-and-braces; the union should be a no-op in a healthy run).
all_geo_codes <- unique(as.character(DT[[geo_col]]))
all_geo_codes <- all_geo_codes[!is.na(all_geo_codes) & nzchar(all_geo_codes)]
if (MATCHING_GEOGRAPHY == "LA") {
  # Match 05_create_matched_pairs.R: the unknown-LA file is skipped; real LA
  # codes are ONS codes ([A-Z][0-9]{8}).
  all_geo_codes <- all_geo_codes[grepl("^[A-Z][0-9]{8}$", all_geo_codes)]
}
all_geo_codes <- sort(union(all_geo_codes,
                            vapply(rds_index, `[[`, "", "geo_code")))

# Gate thresholds: must reproduce 05_create_matched_pairs.R exactly.
# Sparse = ppd_price_sqm among continuous vars OR tax_band among exact vars;
# the gate uses STRICT inequality (n > min passes).
gate_thresholds <- function(spec_cfg) {
  is_sparse <- ("ppd_price_sqm" %in% spec_cfg$continuous_vars) ||
               ("tax_band" %in% spec_cfg$exact_vars)
  list(
    min_treated = if (is_sparse) MATCHING_MIN_TREATED_LOW else MATCHING_MIN_TREATED_HIGH,
    min_control = if (is_sparse) MATCHING_MIN_CONTROL_LOW else MATCHING_MIN_CONTROL_HIGH
  )
}

base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                        "property_type", "main_fuel", "construction_age_band", "built_form")

treat_shorts <- vapply(treatment_metadata, `[[`, "", "short_id")

n_combos_per_geo <- length(treatment_metadata) *
  sum(vapply(spec_configs, function(sc) length(spec_core_pairs[[sc$name]]), integer(1L)))
message(sprintf("  Full grid: %d geo units x %d combos = %s rows expected",
                length(all_geo_codes), n_combos_per_geo,
                formatC(length(all_geo_codes) * n_combos_per_geo, big.mark = ",")))

balance_all <- vector("list", length(all_geo_codes) * n_combos_per_geo)
log_all     <- vector("list", length(all_geo_codes) * n_combos_per_geo)
idx    <- 0L
n_geo  <- length(all_geo_codes)
t_loop <- Sys.time()

for (gi in seq_len(n_geo)) {
  geo_code <- all_geo_codes[gi]

  # Subset data to this geo unit (deduplicate by UPRN — parquet may have
  # multiple EPCs). NOTE: 05 does not dedupe within single-file LA units, so
  # counts here can differ marginally from 05's internal counts when an LA
  # parquet contains duplicate UPRNs.
  geo_dt <- DT[get(geo_col) == geo_code]
  if (nrow(geo_dt) > 0L && anyDuplicated(geo_dt$uprn)) {
    geo_dt <- geo_dt[!duplicated(uprn)]
  }

  for (core_name in names(matching_core_filters)) {
    core_short <- core_short_ids[[core_name]]
    core_dt <- if (nrow(geo_dt) > 0L) geo_dt[eval(matching_core_filters[[core_name]])] else geo_dt

    # Raw counts per treatment: core filter + non-NA treatment, PRE
    # complete-cases (funnel stage 1).
    raw_counts <- lapply(treatment_metadata, function(tm) {
      tv <- core_dt[[tm$var]]
      list(n_raw         = as.integer(sum(!is.na(tv))),
           n_treated_raw = as.integer(sum(tv == 1L, na.rm = TRUE)),
           n_control_raw = as.integer(sum(tv == 0L, na.rm = TRUE)))
    })
    names(raw_counts) <- treat_shorts

    for (spec_cfg in spec_configs) {
      spec_name <- spec_cfg$name
      if (!(core_name %in% spec_core_pairs[[spec_name]])) next
      spec_short <- spec_cfg$short_id

      # Complete-case filter, treatment-independent (funnel stage 2).
      # Reproduces the filter sequence of match_single_unit() in 05.
      cc_dt <- core_dt
      if (nrow(cc_dt) > 0L) {
        cc_dt <- cc_dt[complete.cases(cc_dt[, ..base_matching_vars])]
        spec_vars <- unique(c(base_matching_vars, spec_cfg$continuous_vars, spec_cfg$exact_vars))
        spec_vars <- setdiff(spec_vars, "local_authority")
        cc_dt <- na.omit(cc_dt, cols = intersect(spec_vars, names(cc_dt)))
        if ("ppd_price_sqm" %in% spec_cfg$continuous_vars &&
            "ppd_price_sqm" %in% names(cc_dt)) {
          cc_dt <- cc_dt[!is.infinite(ppd_price_sqm)]
        }
      }

      gt <- gate_thresholds(spec_cfg)

      for (treat_short in treat_shorts) {
        idx <- idx + 1L
        treat_meta    <- treat_meta_by_short[[treat_short]]
        treatment_var <- treat_meta$var
        rc <- raw_counts[[treat_short]]

        # "before" sample = complete cases with non-NA treatment
        before   <- cc_dt[!is.na(get(treatment_var))]
        before_t <- before[get(treatment_var) == 1L]
        before_c <- before[get(treatment_var) == 0L]
        n_treated_before <- nrow(before_t)
        n_control_before <- nrow(before_c)

        # Gate (funnel stage 3): strict inequality, as in 05
        gate_passed <- (n_treated_before > gt$min_treated) &&
                       (n_control_before > gt$min_control)

        base_row <- data.table(
          geo_level          = MATCHING_GEOGRAPHY,
          geo_code           = geo_code,
          treatment_short_id = treat_short,
          matching_core      = core_name,
          spec               = spec_name,
          n_raw              = rc$n_raw,
          n_treated_raw      = rc$n_treated_raw,
          n_control_raw      = rc$n_control_raw,
          n_before           = nrow(before),
          n_treated_before   = n_treated_before,
          n_control_before   = n_control_before,
          min_treated_req    = gt$min_treated,
          min_control_req    = gt$min_control,
          gate_passed        = gate_passed
        )

        rds_path <- rds_lookup[[paste(geo_code, treat_short, core_short, spec_short,
                                      sep = "|")]]

        if (is.null(rds_path)) {
          # No matched output for this combo: classify why (funnel diagnostics)
          status <- if (nrow(before) == 0L) "no_eligible_rows"
                    else if (!gate_passed)  "not_matched_gate"
                    else                    "not_matched_other"
          log_all[[idx]] <- cbind(base_row, data.table(status = status))
          next
        }

        # Read matched UPRNs
        matched <- tryCatch(readRDS(rds_path), error = function(e) NULL)
        if (is.null(matched) || nrow(matched) == 0L) {
          log_all[[idx]] <- cbind(base_row,
                                  data.table(status = "empty_rds", n_matched = 0L))
          next
        }
        if (nrow(before) == 0L) {
          log_all[[idx]] <- cbind(base_row,
                                  data.table(status = "no_before_sample",
                                             n_matched = nrow(matched)))
          next
        }

        # "after" sample: inner join matched UPRNs to before data
        # Deduplicate by UPRN — control units may appear multiple times in match.data()
        matched_uprns <- unique(matched[, .(uprn, distance)], by = "uprn")
        after <- before[uprn %in% matched_uprns$uprn]

        if (nrow(after) == 0L) {
          log_all[[idx]] <- cbind(base_row,
                                  data.table(status = "no_after_join",
                                             n_matched = nrow(matched)))
          next
        }

        # Split before/after by treatment
        after_t <- after[get(treatment_var) == 1L]
        after_c <- after[get(treatment_var) == 0L]

        # Post-hoc caliper counts (funnel stages 5-6): distance <= 0.2 / 0.1
        # mirrors the PS<=0.2 / PS<=0.1 filters in 06_run_regressions.R.
        n_matched_cal02 <- as.integer(sum(matched$distance <= 0.2, na.rm = TRUE))
        n_matched_cal01 <- as.integer(sum(matched$distance <= 0.1, na.rm = TRUE))
        tv_join <- merge(matched[, .(uprn, distance)],
                         before[, .(uprn, tv = get(treatment_var))], by = "uprn")
        n_matched_treated_cal02 <- as.integer(
          sum(tv_join$distance <= 0.2 & tv_join$tv == 1L, na.rm = TRUE))
        n_matched_treated_cal01 <- as.integer(
          sum(tv_join$distance <= 0.1 & tv_join$tv == 1L, na.rm = TRUE))

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

        log_all[[idx]] <- cbind(base_row, data.table(
          status                  = "success",
          n_matched               = nrow(matched),
          n_matched_treated       = nrow(after_t),
          n_matched_control       = nrow(after_c),
          n_matched_cal02         = n_matched_cal02,
          n_matched_treated_cal02 = n_matched_treated_cal02,
          n_matched_cal01         = n_matched_cal01,
          n_matched_treated_cal01 = n_matched_treated_cal01,
          match_rate              = if (n_treated_before > 0L) nrow(after_t) / n_treated_before else NA_real_,
          smd_max_before          = if (nrow(subst) > 0L) max(abs(subst$smd_before), na.rm = TRUE) else NA_real_,
          smd_max_after           = if (nrow(subst) > 0L) max(subst_abs_smd, na.rm = TRUE) else NA_real_,
          smd_mean_after          = if (nrow(subst) > 0L) mean(subst_abs_smd, na.rm = TRUE) else NA_real_,
          smd_median_after        = if (nrow(subst) > 0L) median(subst_abs_smd, na.rm = TRUE) else NA_real_,
          frac_covs_balanced_01   = if (nrow(subst) > 0L) mean(subst_abs_smd < 0.1, na.rm = TRUE) else NA_real_,
          frac_covs_balanced_025  = if (nrow(subst) > 0L) mean(subst_abs_smd < 0.25, na.rm = TRUE) else NA_real_,
          n_covariates            = nrow(subst)
        ))
      }
    }
  }

  if (gi %% 20L == 0L || gi == n_geo) {
    elapsed <- as.numeric(difftime(Sys.time(), t_loop, units = "secs"))
    message(sprintf("  [%d/%d geo units] %s combo rows (%.0f s)",
                    gi, n_geo, formatC(idx, big.mark = ","), elapsed))
  }
}


# 5. Write balance + log outputs -------------------------------------------
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


# 6. Attrition funnel: pooled per (treatment, core, spec) ------------------
message("\n--- Building attrition funnel ---")

funnel <- log_dt[, .(
  # LA (geo unit) survival by stage
  n_las_input         = uniqueN(geo_code),
  n_las_eligible      = uniqueN(geo_code[n_treated_raw > 0L & n_control_raw > 0L]),
  n_las_complete_case = uniqueN(geo_code[n_treated_before > 0L & n_control_before > 0L]),
  n_las_gate_passed   = uniqueN(geo_code[gate_passed %in% TRUE]),
  n_las_matched       = uniqueN(geo_code[!is.na(n_matched_treated) & n_matched_treated > 0L]),
  n_las_cal02         = uniqueN(geo_code[!is.na(n_matched_treated_cal02) & n_matched_treated_cal02 > 0L]),
  n_las_cal01         = uniqueN(geo_code[!is.na(n_matched_treated_cal01) & n_matched_treated_cal01 > 0L]),
  # Pooled property counts by stage
  n_raw                    = sum(n_raw, na.rm = TRUE),
  n_treated_raw            = sum(n_treated_raw, na.rm = TRUE),
  n_complete_case          = sum(n_before, na.rm = TRUE),
  n_treated_complete_case  = sum(n_treated_before, na.rm = TRUE),
  n_gate_passed            = sum(n_before[gate_passed %in% TRUE], na.rm = TRUE),
  n_treated_gate_passed    = sum(n_treated_before[gate_passed %in% TRUE], na.rm = TRUE),
  n_matched                = sum(n_matched, na.rm = TRUE),
  n_matched_treated        = sum(n_matched_treated, na.rm = TRUE),
  n_matched_cal02          = sum(n_matched_cal02, na.rm = TRUE),
  n_matched_treated_cal02  = sum(n_matched_treated_cal02, na.rm = TRUE),
  n_matched_cal01          = sum(n_matched_cal01, na.rm = TRUE),
  n_matched_treated_cal01  = sum(n_matched_treated_cal01, na.rm = TRUE)
), by = .(treatment_short_id, matching_core, spec)]

# Rates
funnel[, match_rate      := fifelse(n_treated_gate_passed > 0L,
                                    n_matched_treated / n_treated_gate_passed, NA_real_)]
funnel[, cal02_retention := fifelse(n_matched_treated > 0L,
                                    n_matched_treated_cal02 / n_matched_treated, NA_real_)]
funnel[, cal01_retention := fifelse(n_matched_treated > 0L,
                                    n_matched_treated_cal01 / n_matched_treated, NA_real_)]
funnel[, la_retention    := fifelse(n_las_input > 0L,
                                    n_las_matched / n_las_input, NA_real_)]

# n_las_regression: distinct-LA count of the headline regression, read from
# results_table (clustering is ~local_authority, so n_clusters = distinct LAs).
# Headline row: PSM (Matched) + Subclass FE, regression_core == matching_core,
# outcome current_energy_efficiency, status ok. NA if 06 has not run yet.
funnel[, n_las_regression := NA_integer_]
results_path <- file.path(SUMMARY_TABLES_DIR,
                          paste0("results_table_", MATCHING_GEOGRAPHY, wc_suffix, ".csv"))
if (file.exists(results_path)) {
  res <- tryCatch(fread(results_path, na.strings = c("NA", "")),
                  error = function(e) NULL)
  needed_res_cols <- c("model", "spec", "matching_core", "regression_core",
                       "outcome", "status", "n_clusters", "treatment_short_id")
  if (!is.null(res) && nrow(res) > 0L && all(needed_res_cols %in% names(res))) {
    headline <- res[model == "PSM (Matched) + Subclass FE" &
                      regression_core == matching_core &
                      outcome == "current_energy_efficiency" &
                      status == "ok",
                    .(n_las_reg = as.integer(n_clusters[1L])),
                    by = .(treatment_short_id, matching_core, spec)]
    funnel[headline, n_las_regression := i.n_las_reg,
           on = c("treatment_short_id", "matching_core", "spec")]
    message(sprintf("  n_las_regression filled for %d / %d funnel rows.",
                    sum(!is.na(funnel$n_las_regression)), nrow(funnel)))
    rm(res, headline)
  } else {
    message("  NOTE: results_table unreadable or missing columns; n_las_regression = NA.")
  }
} else {
  message(sprintf("  NOTE: %s not found (06 not run yet?); n_las_regression = NA.",
                  basename(results_path)))
}

# Order rows: treatment (metadata order), core, spec
funnel[, .t_idx := match(treatment_short_id, treat_shorts)]
setorder(funnel, .t_idx, matching_core, spec)
funnel[, .t_idx := NULL]

# Sanity checks: funnel monotonicity ---------------------------------------
viol_prop <- funnel[n_raw < n_complete_case | n_complete_case < n_gate_passed |
                    n_treated_gate_passed < n_matched_treated |
                    n_matched_treated < n_matched_treated_cal02 |
                    n_matched_treated_cal02 < n_matched_treated_cal01]
if (nrow(viol_prop) > 0L) {
  message(sprintf("  WARNING: %d funnel rows violate property-count monotonicity:", nrow(viol_prop)))
  print(viol_prop[, .(treatment_short_id, matching_core, spec)])
} else {
  message("  Sanity: property-count funnel monotone for all rows.")
}

viol_la <- funnel[!is.na(n_las_regression) & n_las_regression > n_las_matched]
if (nrow(viol_la) > 0L) {
  message(sprintf("  WARNING: %d funnel rows have n_las_regression > n_las_matched:", nrow(viol_la)))
  print(viol_la[, .(treatment_short_id, matching_core, spec, n_las_regression, n_las_matched)])
} else {
  message("  Sanity: n_las_regression <= n_las_matched wherever available.")
}

funnel_file <- file.path(SUMMARY_TABLES_DIR,
                         paste0("attrition_funnel_", MATCHING_GEOGRAPHY, wc_suffix, ".csv"))
fwrite(funnel, funnel_file)
message(sprintf("  Attrition funnel written: %s (%d rows)",
                basename(funnel_file), nrow(funnel)))

rm(DT, balance_all, log_all, balance_dt, log_dt, funnel)
gc()

end.time <- Sys.time()
message(sprintf("\n  05b_compute_balance.R complete [%s]. Runtime: %.1f min.",
                MATCHING_GEOGRAPHY,
                as.numeric(difftime(end.time, start.time, units = "mins"))))
