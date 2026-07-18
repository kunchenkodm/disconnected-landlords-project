###############################################################################
# 09a_geography_recommendation.R
# Geography Recommendation — Standalone Script
#
# Reads enriched regression results, matching logs, and balance diagnostics
# across three geographies (LA, ITL2, ITL3) and produces:
#   1. geography_recommendation.csv — quantitative geography comparison + scoring
#   2. geography_diagnostic_detail.csv — per-treatment geography diagnostics
#
# Run after 06b_enrich_results.R for each geography.
# Outputs to: output/synthesis/
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: March 28, 2026.
###############################################################################

rm(list = setdiff(ls(), c("MATCHING_GEOGRAPHY_OVERRIDE"))); gc()

library(data.table)
library(here)

source(here::here("scripts", "00_setup.R"))

wc_suffix <- if (WITHIN_CORPORATE) "_wc" else ""

message("===================================================================")
message("  09a_geography_recommendation.R")
message(sprintf("  Started: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

SYNTHESIS_DIR <- file.path(OUTPUT_ROOT, "synthesis")
dir.create(SYNTHESIS_DIR, showWarnings = FALSE, recursive = TRUE)

geo_levels <- c("LA", "ITL2", "ITL3")

# --- Data Ingestion ---
message("=== Loading data ===")

enriched <- rbindlist(lapply(geo_levels, function(g) {
  path <- file.path(SUMMARY_TABLES_DIR, paste0("results_enriched_", g, wc_suffix, ".csv"))
  if (!file.exists(path)) { message("  Missing: ", path); return(NULL) }
  dt <- fread(path, na.strings = c("NA", ""))
  dt[, geography := g]
  message("  Loaded enriched ", g, ": ", nrow(dt), " rows")
  dt
}), fill = TRUE)

match_logs <- rbindlist(lapply(geo_levels, function(g) {
  path <- file.path(MATCHED_DATA_DIR, g, paste0("matching_log_", g, ".csv"))
  if (!file.exists(path)) return(NULL)
  dt <- fread(path, na.strings = c("NA", ""))
  dt[, geography := g]
  dt
}), fill = TRUE)

taxonomy <- tryCatch(
  fread(file.path(SUMMARY_TABLES_DIR, "treatment_taxonomy.csv"), na.strings = c("NA", "")),
  error = function(e) data.table()
)
outcomes <- tryCatch(
  fread(file.path(SUMMARY_TABLES_DIR, "outcome_metadata.csv"), na.strings = c("NA", "")),
  error = function(e) data.table()
)

message("  Total enriched rows: ", nrow(enriched))
message("  Total matching log rows: ", nrow(match_logs))

# --- Helpers ---
modal_sign <- function(coefs) {
  s <- sign(coefs[!is.na(coefs)])
  if (length(s) == 0L) return(NA_integer_)
  tb <- tabulate(factor(s, levels = c(-1L, 0L, 1L)))
  c(-1L, 0L, 1L)[which.max(tb)]
}

ols_models <- c("OLS Additive FE", "OLS Interactive FE")

treat_order <- c("fp", "ukfp", "frfp", "thfp",
                 "np", "uknp", "frnp", "thnp",
                 "ps",
                 "th", "bh", "eh", "ch", "oh")

# --- Compute Taxonomy (needed for sign consistency scoring) ---
message("=== Computing taxonomy ===")

ok <- enriched[status == "ok" & !is.na(coef)]
message("  OK rows: ", nrow(ok))

tax <- ok[, {
  coefs <- coef
  std_coefs <- standardised_coef
  pvals <- p_value
  is_ols <- model %in% ols_models
  is_psm <- !is_ols

  signs <- sign(coefs[!is.na(coefs)])
  sign_unan <- as.integer(length(unique(signs[signs != 0])) <= 1L)

  ols_sign <- modal_sign(coefs[is_ols])
  psm_sign <- modal_sign(coefs[is_psm])
  sign_op <- if (is.na(ols_sign) || is.na(psm_sign)) NA_integer_ else as.integer(ols_sign == psm_sign)

  sig_pos <- any(coefs > 0 & pvals < 0.05, na.rm = TRUE)
  sig_neg <- any(coefs < 0 & pvals < 0.05, na.rm = TRUE)
  janus <- as.integer(sig_pos & sig_neg)

  frac5 <- mean(pvals < 0.05, na.rm = TRUE)
  grade <- "High"
  if (frac5 < 0.75 || sign_unan == 0L) grade <- "Moderate"
  if (frac5 < 0.50 || janus == 1L) grade <- "Low"
  if (frac5 < 0.25 && sign_unan == 0L) grade <- "Very Low"

  list(
    n_specs = .N,
    median_coef = median(coefs, na.rm = TRUE),
    median_std_coef = median(std_coefs, na.rm = TRUE),
    frac_sig_5pct = frac5,
    sign_unanimous = sign_unan,
    sign_ols_vs_psm = sign_op,
    janus_effect = janus,
    robustness_grade = grade
  )
}, by = .(treatment_short_id, outcome, geography)]

tax <- merge(tax, taxonomy, by = "treatment_short_id", all.x = TRUE)
tax <- merge(tax, outcomes, by = "outcome", all.x = TRUE)

message("  Taxonomy: ", nrow(tax), " rows")


# --- Geography Recommendation ---
message("=== Geography Recommendation ===")

geo_rec <- rbindlist(lapply(geo_levels, function(g) {

  # Component A: Matching Quality
  psm_ok <- enriched[geography == g & status == "ok" & !model %in% ols_models]
  med_mr <- median(psm_ok$psm_match_rate, na.rm = TRUE)

  mean_smd_vals <- if ("psm_mean_abs_smd_after" %in% names(psm_ok)) psm_ok$psm_mean_abs_smd_after else NA_real_
  frac_bal_01_vals <- if ("psm_frac_balanced_01" %in% names(psm_ok)) psm_ok$psm_frac_balanced_01 else NA_real_
  frac_bal_025_vals <- if ("psm_frac_balanced_025" %in% names(psm_ok)) psm_ok$psm_frac_balanced_025 else NA_real_
  max_smd_vals <- psm_ok$psm_max_smd_after

  if (all(is.na(mean_smd_vals))) {
    med_mean_smd <- NA_real_; med_frac_bal_01 <- NA_real_
    med_frac_bal_025 <- NA_real_; med_max_smd <- NA_real_
  } else {
    med_mean_smd <- median(mean_smd_vals, na.rm = TRUE)
    med_frac_bal_01 <- median(frac_bal_01_vals, na.rm = TRUE)
    med_frac_bal_025 <- median(frac_bal_025_vals, na.rm = TRUE)
    med_max_smd <- median(max_smd_vals, na.rm = TRUE)
  }

  # Component B: Regression Diagnostics
  eg <- enriched[geography == g]
  n_total <- nrow(eg)
  n_ok <- sum(eg$status == "ok", na.rm = TRUE)
  err_rate <- 1 - n_ok / n_total
  ok_g <- eg[status == "ok"]
  n_fc <- if ("few_clusters_flag" %in% names(ok_g)) sum(ok_g$few_clusters_flag == 1L, na.rm = TRUE) else 0L
  frac_fc <- if (nrow(ok_g) > 0 && "few_clusters_flag" %in% names(ok_g)) {
    mean(ok_g$few_clusters_flag == 1L, na.rm = TRUE)
  } else 0
  med_nc <- median(ok_g$n_clusters, na.rm = TRUE)
  med_nobs <- median(ok_g$nobs, na.rm = TRUE)

  # Component C: Specification-Curve Coherence
  tax_g <- tax[geography == g]
  mean_fs <- mean(tax_g$frac_sig_5pct, na.rm = TRUE)
  mean_su <- mean(tax_g$sign_unanimous, na.rm = TRUE)
  mean_sop <- mean(tax_g$sign_ols_vs_psm, na.rm = TRUE)
  n_high <- sum(tax_g$robustness_grade == "High", na.rm = TRUE)
  n_jan <- sum(tax_g$janus_effect == 1L, na.rm = TRUE)
  frac_hm <- mean(tax_g$robustness_grade %in% c("High", "Moderate"), na.rm = TRUE)

  data.table(
    geography = g,
    median_match_rate = med_mr,
    median_mean_abs_smd = med_mean_smd,
    median_frac_balanced_01 = med_frac_bal_01,
    median_frac_balanced_025 = med_frac_bal_025,
    median_max_smd_excl_distance = med_max_smd,
    n_models_total = n_total,
    n_models_ok = n_ok,
    error_rate = err_rate,
    n_few_clusters = n_fc,
    frac_few_clusters = frac_fc,
    median_n_clusters = med_nc,
    median_nobs = med_nobs,
    mean_frac_sig = mean_fs,
    mean_sign_unanimous = mean_su,
    mean_sign_ols_vs_psm = mean_sop,
    n_high_grade = n_high,
    n_janus = n_jan,
    frac_high_or_moderate = frac_hm
  )
}))


# Component D: Cross-Geography Sign Consistency
tax_wide <- dcast(tax, treatment_short_id + outcome ~ geography,
                  value.var = "median_coef", fun.aggregate = function(x) x[1])

if (all(geo_levels %in% names(tax_wide))) {
  tax_wide[, majority_sign := sign(rowMeans(cbind(
    sign(get("LA")), sign(get("ITL2")), sign(get("ITL3"))
  ), na.rm = TRUE))]

  for (g in geo_levels) {
    geo_sign <- sign(tax_wide[[g]])
    agree <- geo_sign == tax_wide$majority_sign &
             !is.na(geo_sign) & tax_wide$majority_sign != 0
    frac_agree <- mean(agree, na.rm = TRUE)
    geo_rec[geography == g, frac_cross_geo_sign_agree := frac_agree]
  }
} else {
  geo_rec[, frac_cross_geo_sign_agree := NA_real_]
}


# Scoring formula
# Balance quality uses mean|SMD| (continuous, discriminating) rather than
# frac_balanced_01 (too coarse with only 3-4 covariates per unit).
# Balance score = 1 - mean|SMD|/0.5 (capped at 0), so |SMD|=0 -> 1, |SMD|>=0.5 -> 0.
geo_rec[, has_balance := !is.na(median_mean_abs_smd)]
geo_rec[, balance_score := fifelse(
  is.na(median_mean_abs_smd), 0,
  pmax(1 - median_mean_abs_smd / 0.5, 0)
)]
geo_rec[, recommendation_score := fifelse(has_balance,
  # Full formula with matching balance
  balance_score * 0.25 +
  fifelse(is.na(median_match_rate), 0, median_match_rate) * 0.15 +
  fifelse(is.na(mean_sign_unanimous), 0, mean_sign_unanimous) * 0.20 +
  fifelse(is.na(mean_sign_ols_vs_psm), 0, mean_sign_ols_vs_psm) * 0.15 +
  fifelse(is.na(frac_high_or_moderate), 0, frac_high_or_moderate) * 0.15 +
  (1 - fifelse(is.na(error_rate), 0, error_rate)) * 0.05 +
  fifelse(is.na(frac_cross_geo_sign_agree), 0, frac_cross_geo_sign_agree) * 0.05,
  # Fallback without balance data (reweight to 1.0)
  fifelse(is.na(mean_sign_unanimous), 0, mean_sign_unanimous) * 0.35 +
  fifelse(is.na(mean_sign_ols_vs_psm), 0, mean_sign_ols_vs_psm) * 0.25 +
  fifelse(is.na(frac_high_or_moderate), 0, frac_high_or_moderate) * 0.20 +
  (1 - fifelse(is.na(error_rate), 0, error_rate)) * 0.10 +
  fifelse(is.na(frac_cross_geo_sign_agree), 0, frac_cross_geo_sign_agree) * 0.10
)]


# --- Write outputs ---
fwrite(geo_rec, file.path(SYNTHESIS_DIR, paste0("geography_recommendation", wc_suffix, ".csv")))
message("  Geography recommendation written: ", nrow(geo_rec), " rows")


# Geography Diagnostic Detail
geo_detail <- tax[, .(
  n_outcomes_sign_unanimous = sum(sign_unanimous == 1L, na.rm = TRUE),
  n_outcomes_high_grade = sum(robustness_grade == "High", na.rm = TRUE),
  n_outcomes = .N
), by = .(treatment_short_id, geography)]

if (nrow(match_logs) > 0) {
  ml_agg <- match_logs[status == "success", .(
    median_treated_n = median(n_treated_before, na.rm = TRUE),
    median_control_n = median(n_control_before, na.rm = TRUE),
    median_match_rate_for_treatment = median(match_rate, na.rm = TRUE)
  ), by = .(treatment_short_id, geography)]
  geo_detail <- merge(geo_detail, ml_agg,
                      by = c("treatment_short_id", "geography"), all.x = TRUE)
}

geo_detail[, treat_idx := match(treatment_short_id, treat_order)]
setorder(geo_detail, treat_idx, geography)
geo_detail[, treat_idx := NULL]

fwrite(geo_detail, file.path(SYNTHESIS_DIR, paste0("geography_diagnostic_detail", wc_suffix, ".csv")))
message("  Geography diagnostic detail written: ", nrow(geo_detail), " rows")


# --- Print summary ---
winner <- geo_rec[which.max(recommendation_score), geography]

message("\n  --- Geography Recommendation ---")
for (i in seq_len(nrow(geo_rec))) {
  g <- geo_rec[i]
  smd_str <- if (is.na(g$median_mean_abs_smd)) "N/A" else sprintf("%.3f", g$median_mean_abs_smd)
  message(sprintf("  %s: score=%.3f | match_rate=%.3f | sign_unan=%.3f | mean|SMD|=%s | balance=%s",
                  g$geography, g$recommendation_score, g$median_match_rate,
                  g$mean_sign_unanimous, smd_str,
                  if (g$has_balance) "yes" else "no"))
}
message("  >> Recommended geography: ", winner)

message(sprintf("\n  09a_geography_recommendation.R complete [%s].",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
