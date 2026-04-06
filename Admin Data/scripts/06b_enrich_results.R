# Script: 06b_enrich_results.R
# Purpose: Post-hoc enrichment of the regression results CSV.
#          Reads results_table_<GEO>.csv and matching_balance_<GEO>.csv,
#          computes sign-consistency, robustness aggregates, and taxonomy
#          tables, then writes enriched outputs to output/summary_tables/.
#
# Run after 06_run_regressions.R for each geography.
# Invoked by run_analysis.R (Phase 3).
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: March 19, 2026.
rm(list = setdiff(ls(), c("script", "pipeline.start.time")))
gc()

library(data.table)
library(here)

source(here::here("scripts", "00_setup.R"))

# Geography: read from env var (same pattern as 06_run_regressions.R)
# MATCHING_GEOGRAPHY is already set by 00_setup.R from MATCHING_GEOGRAPHY_OVERRIDE.
wc_suffix <- if (WITHIN_CORPORATE) "_wc" else ""

message("===================================================================")
message("  06b_enrich_results.R - Post-hoc Enrichment")
message(sprintf("  Geography: %s", MATCHING_GEOGRAPHY))
message(sprintf("  Started:   %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

# Read inputs ---------------------------------------------------------------
results_path <- file.path(SUMMARY_TABLES_DIR,
                          paste0("results_table_", MATCHING_GEOGRAPHY, wc_suffix, ".csv"))
if (!file.exists(results_path)) {
  stop("Results CSV not found: ", results_path,
       "\n  Run 06_run_regressions.R first.")
}
results <- fread(results_path)
message(sprintf("  Loaded %d rows from %s", nrow(results), basename(results_path)))

balance_path <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY,
                          paste0("matching_balance_", MATCHING_GEOGRAPHY, ".csv"))
has_balance <- file.exists(balance_path)
if (has_balance) {
  balance <- fread(balance_path)
  message(sprintf("  Loaded balance data: %d rows", nrow(balance)))
} else {
  message("  Balance file not found — psm_max_smd_after will be NA.")
}


# Enrichment ----------------------------------------------------------------

## 1. significance_level
results[, significance_level := fcase(
  p_value < 0.01, "***",
  p_value < 0.05, "**",
  p_value < 0.10, "*",
  default = ""
)]

## 2. sign_consistent_all
# For each (treatment_short_id, outcome): is sign(coef) unanimous across ALL
# model types where status == "ok"?
sign_all <- results[status == "ok" & !is.na(coef),
                    .(sign_consistent_all = as.integer(length(unique(sign(coef))) == 1L)),
                    by = .(treatment_short_id, outcome)]
results <- merge(results, sign_all, by = c("treatment_short_id", "outcome"),
                 all.x = TRUE, sort = FALSE)

## 3. sign_consistent_ols_vs_psm
# Modal sign within OLS models and within PSM models, then check agreement.
ols_models <- c("OLS Additive FE", "OLS Interactive FE")
modal_sign <- function(coefs) {
  s <- sign(coefs[!is.na(coefs)])
  if (length(s) == 0L) return(NA_integer_)
  tb <- tabulate(factor(s, levels = c(-1L, 0L, 1L)))
  c(-1L, 0L, 1L)[which.max(tb)]
}
sign_ols <- results[status == "ok" & !is.na(coef) & model %in% ols_models,
                    .(modal_sign_ols = modal_sign(coef)),
                    by = .(treatment_short_id, outcome)]
sign_psm <- results[status == "ok" & !is.na(coef) & !model %in% ols_models,
                    .(modal_sign_psm = modal_sign(coef)),
                    by = .(treatment_short_id, outcome)]
sign_compare <- merge(sign_ols, sign_psm,
                      by = c("treatment_short_id", "outcome"), all = TRUE)
sign_compare[, sign_consistent_ols_vs_psm :=
               as.integer(!is.na(modal_sign_ols) & !is.na(modal_sign_psm) &
                          modal_sign_ols == modal_sign_psm)]
sign_compare[, c("modal_sign_ols", "modal_sign_psm") := NULL]
results <- merge(results, sign_compare,
                 by = c("treatment_short_id", "outcome"), all.x = TRUE, sort = FALSE)

## 4. rank_consistent
# For each (outcome), check whether the top-ranked and bottom-ranked
# treatment_short_id are the same across >80% of specification slices.
# A specification slice is a unique (model, spec, matching_core, regression_core).
ok_results <- results[status == "ok" & !is.na(coef)]
if (nrow(ok_results) > 0L) {
  slice_cols <- c("model", "spec", "matching_core", "regression_core")

  ok_results[, slice_id := do.call(paste, c(.SD, list(sep = "|"))), .SDcols = slice_cols]

  rank_tags <- ok_results[, {
    n_treats <- uniqueN(treatment_short_id)
    if (n_treats >= 2L) {
      # Rank treatments by coef within this slice (rank 1 = highest coef)
      .SD[order(-coef), .(treatment_short_id, rank_pos = seq_len(.N))]
    } else {
      .SD[, .(treatment_short_id, rank_pos = 1L)]
    }
  }, by = .(outcome, slice_id)]

  top_bottom <- rank_tags[, .(
    top_treat    = treatment_short_id[rank_pos == 1L][1L],
    bottom_treat = treatment_short_id[rank_pos == max(rank_pos)][1L]
  ), by = .(outcome, slice_id)]

  rank_consist <- top_bottom[, {
    n_slices <- .N
    if (n_slices == 0L) {
      data.table(rank_consistent = NA_integer_)
    } else {
      top_mode    <- names(sort(table(top_treat),    decreasing = TRUE))[1L]
      bottom_mode <- names(sort(table(bottom_treat), decreasing = TRUE))[1L]
      frac_top    <- sum(top_treat    == top_mode,    na.rm = TRUE) / n_slices
      frac_bottom <- sum(bottom_treat == bottom_mode, na.rm = TRUE) / n_slices
      data.table(rank_consistent = as.integer(frac_top > 0.8 & frac_bottom > 0.8))
    }
  }, by = outcome]

  results <- merge(results, rank_consist, by = "outcome", all.x = TRUE, sort = FALSE)
} else {
  results[, rank_consistent := NA_integer_]
}

## 5. Specification robustness aggregates (per treatment_short_id × outcome)
rob <- results[status == "ok" & !is.na(coef), .(
  n_specs_total    = .N,
  n_specs_positive = sum(coef > 0L, na.rm = TRUE),
  n_specs_negative = sum(coef < 0L, na.rm = TRUE),
  n_specs_sig_5pct = sum(p_value < 0.05, na.rm = TRUE),
  frac_sig_5pct    = mean(p_value < 0.05, na.rm = TRUE),
  median_coef      = median(coef, na.rm = TRUE),
  iqr_coef         = IQR(coef, na.rm = TRUE),
  median_standardised_coef = median(standardised_coef, na.rm = TRUE)
), by = .(treatment_short_id, outcome)]

results <- merge(results, rob, by = c("treatment_short_id", "outcome"),
                 all.x = TRUE, sort = FALSE)

## 6. Matching balance metrics (per treatment × matching_core × spec)
# Balance file from 05b uses long names (matching_core="base", spec="Baseline")
# and treatment_short_id — aligns directly with results keys.

# Initialise all balance columns
bal_cols <- c("psm_max_smd_after", "psm_mean_abs_smd_after",
              "psm_frac_balanced_01", "psm_frac_balanced_025",
              "psm_n_balance_units")
for (bc in bal_cols) results[, (bc) := NA_real_]

if (has_balance && nrow(balance) > 0L &&
    all(c("treatment_short_id", "matching_core", "spec", "smd_after") %in% names(balance))) {

  # Include all covariates (including propensity score 'distance') in balance assessment.
  # With only 2-3 substantive covariates per spec, excluding distance makes fraction-based
  # metrics (frac_balanced_01/025) extremely coarse (only values like 0, 0.5, 1.0).
  # Distance is a meaningful covariate (the propensity score) and its balance matters.
  bal_valid <- balance[!is.na(smd_after)]

  # Two-stage aggregation: per-unit first, then median across units
  per_unit <- bal_valid[, .(
    mean_abs_smd     = mean(abs(smd_after)),
    frac_balanced_01 = mean(abs(smd_after) < 0.1),
    frac_balanced_025 = mean(abs(smd_after) < 0.25),
    max_smd          = max(abs(smd_after))
  ), by = .(treatment_short_id, matching_core, spec, geo_code)]

  smd_agg <- per_unit[, .(
    psm_max_smd_after       = median(max_smd, na.rm = TRUE),
    psm_mean_abs_smd_after  = median(mean_abs_smd, na.rm = TRUE),
    psm_frac_balanced_01    = median(frac_balanced_01, na.rm = TRUE),
    psm_frac_balanced_025   = median(frac_balanced_025, na.rm = TRUE),
    psm_n_balance_units     = .N
  ), by = .(treatment_short_id, matching_core, spec)]

  # PSM rows have matching_core not NA/empty; OLS rows get "" (CSV round-trip of NA).
  psm_idx <- which(!results$model %in% ols_models & !is.na(results$matching_core) & nzchar(results$matching_core))
  if (length(psm_idx) > 0L) {
    psm_sub <- data.table(row_idx = psm_idx,
                          treatment_short_id = results$treatment_short_id[psm_idx],
                          matching_core = results$matching_core[psm_idx],
                          spec = results$spec[psm_idx])
    psm_sub <- merge(psm_sub, smd_agg,
                     by = c("treatment_short_id", "matching_core", "spec"),
                     all.x = TRUE, sort = FALSE)
    for (bc in bal_cols) {
      results[psm_sub$row_idx, (bc) := psm_sub[[bc]]]
    }
  }
  rm(bal_valid, per_unit)
}


# Write outputs -------------------------------------------------------------

## Enriched results
enriched_path <- file.path(SUMMARY_TABLES_DIR,
                           paste0("results_enriched_", MATCHING_GEOGRAPHY, wc_suffix, ".csv"))
fwrite(results, enriched_path)
message(sprintf("  Enriched results written: %s (%d rows)", basename(enriched_path), nrow(results)))

## Narrative summary
narrative <- results[status == "ok" & !is.na(coef), .(
  n_models              = .N,
  n_significant_5pct    = sum(p_value < 0.05, na.rm = TRUE),
  frac_significant      = mean(p_value < 0.05, na.rm = TRUE),
  median_coef           = median(coef, na.rm = TRUE),
  iqr_coef              = IQR(coef, na.rm = TRUE),
  min_coef              = min(coef, na.rm = TRUE),
  max_coef              = max(coef, na.rm = TRUE),
  sign_unanimous        = as.integer(length(unique(sign(coef))) == 1L),
  median_standardised   = median(standardised_coef, na.rm = TRUE),
  median_pct_effect     = median(pct_effect, na.rm = TRUE)
), by = .(treatment_short_id, outcome, hypothesis_tag)]

narrative_path <- file.path(SUMMARY_TABLES_DIR,
                             paste0("narrative_summary_", MATCHING_GEOGRAPHY, wc_suffix, ".csv"))
fwrite(narrative, narrative_path)
message(sprintf("  Narrative summary written: %s (%d rows)", basename(narrative_path), nrow(narrative)))

## Outcome metadata (static, written once per geography)
outcome_metadata <- data.table(
  outcome = c("bad_epc", "current_energy_efficiency",
              "energy_consumption_current", "energy_consumption_current_property",
              "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh",
              "energy_consumption_gap", "energy_consumption_gap_property",
              "energy_efficiency_potential_gap",
              "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap",
              "borderline_good_epc", "borderline_better_epc"),
  outcome_family = c("Regulatory Compliance", "Energy Efficiency",
                     "Energy Use", "Energy Use",
                     "Realised Energy", "Realised Energy",
                     "Efficiency Gap", "Efficiency Gap",
                     "Efficiency Gap",
                     "Efficiency Gap", "Efficiency Gap",
                     "Bunching", "Bunching"),
  hypothesis_primary = c("H2", "H1", "H1", "H1", "H1", "H1",
                         "H1", "H1", "H1", "H2", "H2", "H3", "H3")
)
fwrite(outcome_metadata, file.path(SUMMARY_TABLES_DIR, "outcome_metadata.csv"))
message("  Outcome metadata written: outcome_metadata.csv")

## Treatment taxonomy (static, written once per geography)
treatment_taxonomy <- data.table(
  treatment_short_id = c("fp","ukfp","frfp","thfp","np","uknp","frnp","thnp",
                         "ps","th","bh","eh","ch","oh"),
  legal_form    = c("For-Profit","For-Profit","For-Profit","For-Profit",
                    "Non-Profit","Non-Profit","Non-Profit","Non-Profit",
                    "Public Sector","Any","Any","Any","Any","Any"),
  jurisdiction  = c("Any","UK","Foreign","Tax Haven",
                    "Any","UK","Foreign","Tax Haven",
                    "UK","Tax Haven","British Haven","European Haven",
                    "Caribbean Haven","Other Haven"),
  parent_treatment = c(NA,"fp","fp","frfp", NA,"np","np","frnp",
                       NA, NA,"th","th","th","th"),
  pap_T_code    = c("T=1,3","T=1","T=3","T=3+Haven",
                    "T=2,4","T=2","T=4","T=4+Haven",
                    "T=5","Haven","BritHaven","EuroHaven","CaribHaven","OtherHaven")
)
fwrite(treatment_taxonomy, file.path(SUMMARY_TABLES_DIR, "treatment_taxonomy.csv"))
message("  Treatment taxonomy written: treatment_taxonomy.csv")

message(sprintf("\n  06b_enrich_results.R complete [%s].", MATCHING_GEOGRAPHY))
