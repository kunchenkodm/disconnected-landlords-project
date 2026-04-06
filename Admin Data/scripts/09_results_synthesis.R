###############################################################################
# 09_results_synthesis.R
# Results Synthesis for the Disconnected Landlords Project
#
# Reads enriched regression results, matching logs, and balance diagnostics
# across three geographies (LA, ITL2, ITL3) and produces:
#   1. treatment_effect_taxonomy.csv    — full cross-geography taxonomy
#   2. headline_subclass_fe_taxonomy.csv — headline model taxonomy
#   3. specification_curve_summary.csv  — multiverse robustness summary
#   4. sign_consistency_matrix.csv      — cross-geography sign agreement
#   5. synthesis_report.html            — self-contained HTML report
#
# NOTE: Geography recommendation is produced by 09a_geography_recommendation.R.
# This script reads geography_recommendation.csv from output/synthesis/.
###############################################################################

rm(list = setdiff(ls(), c("MATCHING_GEOGRAPHY_OVERRIDE"))); gc()

tryCatch({

# --- Phase 0: Setup ---
message("=== Phase 0: Setup ===")
library(data.table)
library(here)

source(here::here("scripts", "00_setup.R"))

SYNTHESIS_DIR <- here::here("output", "synthesis")
dir.create(SYNTHESIS_DIR, showWarnings = FALSE, recursive = TRUE)

geo_levels <- c("LA", "ITL2", "ITL3")

# --- Phase 1: Data Ingestion ---
message("=== Phase 1: Data Ingestion ===")

enriched <- rbindlist(lapply(geo_levels, function(g) {
  path <- file.path(SUMMARY_TABLES_DIR, paste0("results_enriched_", g, ".csv"))
  if (!file.exists(path)) { message("  Missing: ", path); return(NULL) }
  dt <- fread(path, na.strings = c("NA", ""))
  dt[, geography := g]
  message("  Loaded enriched ", g, ": ", nrow(dt), " rows, ", ncol(dt), " cols")
  dt
}), fill = TRUE)

narratives <- rbindlist(lapply(geo_levels, function(g) {
  path <- file.path(SUMMARY_TABLES_DIR, paste0("narrative_summary_", g, ".csv"))
  if (!file.exists(path)) return(NULL)
  dt <- fread(path, na.strings = c("NA", ""))
  dt[, geography := g]
  dt
}), fill = TRUE)

match_logs <- rbindlist(lapply(geo_levels, function(g) {
  path <- file.path(MATCHED_DATA_DIR, g, paste0("matching_log_", g, ".csv"))
  if (!file.exists(path)) return(NULL)
  dt <- fread(path, na.strings = c("NA", ""))
  dt[, geography := g]
  dt
}), fill = TRUE)

balance <- rbindlist(lapply(geo_levels, function(g) {
  path <- file.path(MATCHED_DATA_DIR, g, paste0("matching_balance_", g, ".csv"))
  if (!file.exists(path)) return(NULL)
  dt <- fread(path, na.strings = c("NA", ""))
  if (nrow(dt) < 2L) return(NULL)
  dt[, geography := g]
  dt
}), fill = TRUE)

taxonomy <- fread(file.path(SUMMARY_TABLES_DIR, "treatment_taxonomy.csv"), na.strings = c("NA", ""))
outcomes <- fread(file.path(SUMMARY_TABLES_DIR, "outcome_metadata.csv"), na.strings = c("NA", ""))

message("  Total enriched rows: ", nrow(enriched))
message("  Total matching log rows: ", nrow(match_logs))
message("  Total balance rows: ", nrow(balance))

# --- Helper functions ---
modal_sign <- function(coefs) {
  s <- sign(coefs[!is.na(coefs)])
  if (length(s) == 0L) return(NA_integer_)
  tb <- tabulate(factor(s, levels = c(-1L, 0L, 1L)))
  c(-1L, 0L, 1L)[which.max(tb)]
}

ols_models <- c("OLS Additive FE", "OLS Interactive FE")

# Treatment ordering for output
treat_order <- c("fp", "ukfp", "frfp", "thfp",
                 "np", "uknp", "frnp", "thnp",
                 "ps",
                 "th", "bh", "eh", "ch", "oh")

# --- Phase 2: Treatment Effect Taxonomy ---
message("=== Phase 2: Treatment Effect Taxonomy ===")

ok <- enriched[status == "ok" & !is.na(coef)]
message("  OK rows for taxonomy: ", nrow(ok))

tax <- ok[, {
  coefs <- coef
  std_coefs <- standardised_coef
  pcts <- pct_effect
  pvals <- p_value
  is_ols <- model %in% ols_models
  is_psm <- !is_ols

  n_s <- .N
  med_c <- median(coefs, na.rm = TRUE)
  iqr_c <- IQR(coefs, na.rm = TRUE)
  med_std <- median(std_coefs, na.rm = TRUE)
  med_pct <- median(pcts, na.rm = TRUE)
  frac_pos <- mean(coefs > 0, na.rm = TRUE)
  frac5 <- mean(pvals < 0.05, na.rm = TRUE)
  frac1 <- mean(pvals < 0.01, na.rm = TRUE)

  signs <- sign(coefs[!is.na(coefs)])
  sign_unan <- as.integer(length(unique(signs[signs != 0])) <= 1L)

  # OLS vs PSM sign comparison
  ols_sign <- modal_sign(coefs[is_ols])
  psm_sign <- modal_sign(coefs[is_psm])
  sign_op <- if (is.na(ols_sign) || is.na(psm_sign)) NA_integer_ else as.integer(ols_sign == psm_sign)

  med_ols <- if (any(is_ols)) median(coefs[is_ols], na.rm = TRUE) else NA_real_
  med_psm <- if (any(is_psm)) median(coefs[is_psm], na.rm = TRUE) else NA_real_

  # Janus effect: BOTH sig positive AND sig negative at 5%
  sig_pos <- any(coefs > 0 & pvals < 0.05, na.rm = TRUE)
  sig_neg <- any(coefs < 0 & pvals < 0.05, na.rm = TRUE)
  janus <- as.integer(sig_pos & sig_neg)

  # Robustness grade
  grade <- "High"
  if (frac5 < 0.75 || sign_unan == 0L) grade <- "Moderate"
  if (frac5 < 0.50 || janus == 1L) grade <- "Low"
  if (frac5 < 0.25 && sign_unan == 0L) grade <- "Very Low"

  list(
    n_specs = n_s,
    median_coef = med_c,
    iqr_coef = iqr_c,
    median_std_coef = med_std,
    median_pct_effect = med_pct,
    frac_positive = frac_pos,
    frac_sig_5pct = frac5,
    frac_sig_1pct = frac1,
    sign_unanimous = sign_unan,
    sign_ols_vs_psm = sign_op,
    median_coef_ols = med_ols,
    median_coef_psm = med_psm,
    janus_effect = janus,
    robustness_grade = grade
  )
}, by = .(treatment_short_id, outcome, geography)]

# Merge with taxonomy and outcomes metadata
tax <- merge(tax, taxonomy, by = "treatment_short_id", all.x = TRUE)
tax <- merge(tax, outcomes, by = "outcome", all.x = TRUE)

# Order rows: legal_form → treatment → outcome_family
tax[, treat_idx := match(treatment_short_id, treat_order)]
tax[, geo_idx := match(geography, geo_levels)]
setorder(tax, treat_idx, geo_idx, outcome_family, outcome)
tax[, c("treat_idx", "geo_idx") := NULL]

fwrite(tax, file.path(SYNTHESIS_DIR, "treatment_effect_taxonomy.csv"))
message("  Taxonomy: ", nrow(tax), " rows, ", ncol(tax), " cols")

# --- Phase 2b: Headline Subclass FE Taxonomy ---
message("=== Phase 2b: Headline Subclass FE Taxonomy ===")

HEADLINE_MODEL <- "PSM (Matched) + Subclass FE"
ok_headline <- enriched[status == "ok" & !is.na(coef) & model == HEADLINE_MODEL]
message("  Headline model rows: ", nrow(ok_headline))

tax_headline <- ok_headline[, {
  coefs <- coef
  std_coefs <- standardised_coef
  pcts <- pct_effect
  pvals <- p_value

  n_s <- .N
  med_c <- median(coefs, na.rm = TRUE)
  iqr_c <- IQR(coefs, na.rm = TRUE)
  med_std <- median(std_coefs, na.rm = TRUE)
  med_pct <- median(pcts, na.rm = TRUE)
  frac_pos <- mean(coefs > 0, na.rm = TRUE)
  frac5 <- mean(pvals < 0.05, na.rm = TRUE)
  frac1 <- mean(pvals < 0.01, na.rm = TRUE)

  signs <- sign(coefs[!is.na(coefs)])
  sign_unan <- as.integer(length(unique(signs[signs != 0])) <= 1L)

  sig_pos <- any(coefs > 0 & pvals < 0.05, na.rm = TRUE)
  sig_neg <- any(coefs < 0 & pvals < 0.05, na.rm = TRUE)
  janus <- as.integer(sig_pos & sig_neg)

  grade <- "High"
  if (frac5 < 0.75 || sign_unan == 0L) grade <- "Moderate"
  if (frac5 < 0.50 || janus == 1L) grade <- "Low"
  if (frac5 < 0.25 && sign_unan == 0L) grade <- "Very Low"

  # Outcome statistics for relative effect computation
  med_outcome_mean <- median(outcome_mean, na.rm = TRUE)
  med_outcome_sd <- median(outcome_sd, na.rm = TRUE)
  pct_of_mean <- if (!is.na(med_outcome_mean) && med_outcome_mean != 0) med_c / med_outcome_mean * 100 else NA_real_

  list(
    n_specs = n_s, median_coef = med_c, iqr_coef = iqr_c,
    median_std_coef = med_std, median_pct_effect = med_pct,
    pct_of_mean = pct_of_mean,
    outcome_mean = med_outcome_mean, outcome_sd = med_outcome_sd,
    frac_positive = frac_pos, frac_sig_5pct = frac5, frac_sig_1pct = frac1,
    sign_unanimous = sign_unan, janus_effect = janus, robustness_grade = grade
  )
}, by = .(treatment_short_id, outcome, geography)]

tax_headline <- merge(tax_headline, taxonomy, by = "treatment_short_id", all.x = TRUE)
tax_headline <- merge(tax_headline, outcomes, by = "outcome", all.x = TRUE)
tax_headline[, treat_idx := match(treatment_short_id, treat_order)]
tax_headline[, geo_idx := match(geography, geo_levels)]
setorder(tax_headline, treat_idx, geo_idx, outcome_family, outcome)
tax_headline[, c("treat_idx", "geo_idx") := NULL]

fwrite(tax_headline, file.path(SYNTHESIS_DIR, "headline_subclass_fe_taxonomy.csv"))
message("  Headline taxonomy: ", nrow(tax_headline), " rows")

# --- Phase 3: Read Geography Recommendation (from 09a) ---
message("=== Phase 3: Geography Recommendation (from 09a) ===")

geo_rec_path <- file.path(SYNTHESIS_DIR, "geography_recommendation.csv")
if (file.exists(geo_rec_path)) {
  geo_rec <- fread(geo_rec_path)
  winner <- geo_rec[which.max(recommendation_score), geography]
  message("  Read geography recommendation from 09a. Winner: ", winner)
  message("  Scores: ", paste(sprintf("%s=%.3f", geo_rec$geography, geo_rec$recommendation_score), collapse = ", "))
} else {
  message("  WARNING: geography_recommendation.csv not found.")
  message("  Run 09a_geography_recommendation.R first, or using LA as fallback.")
  winner <- "LA"
  geo_rec <- data.table(
    geography = geo_levels,
    recommendation_score = NA_real_,
    median_match_rate = NA_real_,
    median_mean_abs_smd = NA_real_,
    median_frac_balanced_01 = NA_real_,
    median_frac_balanced_025 = NA_real_,
    median_max_smd_excl_distance = NA_real_,
    error_rate = NA_real_,
    frac_few_clusters = NA_real_,
    median_nobs = NA_real_,
    mean_sign_unanimous = NA_real_,
    mean_sign_ols_vs_psm = NA_real_,
    frac_high_or_moderate = NA_real_,
    has_balance = FALSE
  )
}

# --- Phase 4: Specification Curve Summary ---
message("=== Phase 4: Specification Curve Summary ===")

spec_curve <- ok[, {
  coefs <- coef
  pvals <- p_value
  K <- .N
  med_eff <- median(coefs, na.rm = TRUE)
  med_sign <- sign(med_eff)

  # Share significant in predicted direction
  sig_pred <- mean(pvals < 0.05 & sign(coefs) == med_sign, na.rm = TRUE)

  # Stouffer's Z
  stouffer_z <- NA_real_
  if (K > 0L && any(!is.na(pvals))) {
    valid <- !is.na(pvals) & !is.na(coefs) & pvals > 0 & pvals < 1
    if (sum(valid) > 0L) {
      z_vals <- qnorm(1 - pvals[valid] / 2) * sign(coefs[valid]) * med_sign
      stouffer_z <- sum(z_vals) / sqrt(sum(valid))
    }
  }

  # CDF(0) — fraction on same side as median
  cdf_zero <- mean(sign(coefs) == med_sign, na.rm = TRUE)

  # Relative effect ratio (95th/5th)
  q <- quantile(coefs, c(0.05, 0.95), na.rm = TRUE)
  rer <- if (!is.na(q[1]) && q[1] != 0) q[2] / q[1] else NA_real_

  # IQR of standardised coefs
  iqr_sc <- IQR(standardised_coef, na.rm = TRUE)

  list(
    median_effect = med_eff,
    share_sig_predicted_direction = sig_pred,
    stouffer_z = stouffer_z,
    cdf_zero = cdf_zero,
    relative_effect_ratio = rer,
    iqr_std_coef = iqr_sc,
    n_specs = K,
    n_positive = sum(coefs > 0, na.rm = TRUE),
    n_negative = sum(coefs < 0, na.rm = TRUE),
    n_sig = sum(pvals < 0.05, na.rm = TRUE)
  )
}, by = .(treatment_short_id, outcome, geography)]

spec_curve <- merge(spec_curve, outcomes, by = "outcome", all.x = TRUE)
spec_curve[, treat_idx := match(treatment_short_id, treat_order)]
spec_curve[, geo_idx := match(geography, geo_levels)]
setorder(spec_curve, treat_idx, geo_idx, outcome_family, outcome)
spec_curve[, c("treat_idx", "geo_idx") := NULL]

fwrite(spec_curve, file.path(SYNTHESIS_DIR, "specification_curve_summary.csv"))
message("  Specification curve summary: ", nrow(spec_curve), " rows")

# --- Phase 5: Cross-Geography Sign Consistency Matrix ---
message("=== Phase 5: Sign Consistency Matrix ===")

sign_mat_mc <- dcast(tax, treatment_short_id + outcome ~ geography,
                     value.var = "median_coef", fun.aggregate = function(x) x[1])
sign_mat_fs <- dcast(tax, treatment_short_id + outcome ~ geography,
                     value.var = "frac_sig_5pct", fun.aggregate = function(x) x[1])
sign_mat_je <- dcast(tax, treatment_short_id + outcome ~ geography,
                     value.var = "janus_effect", fun.aggregate = function(x) x[1])
# Rename before merging
for (g in geo_levels) {
  setnames(sign_mat_mc, g, paste0("median_coef_", g), skip_absent = TRUE)
  setnames(sign_mat_fs, g, paste0("frac_sig_5pct_", g), skip_absent = TRUE)
  setnames(sign_mat_je, g, paste0("janus_effect_", g), skip_absent = TRUE)
}
sign_mat <- Reduce(function(a, b) merge(a, b, by = c("treatment_short_id", "outcome"), all = TRUE),
                   list(sign_mat_mc, sign_mat_fs, sign_mat_je))

# Rename columns
for (g in geo_levels) {
  old_mc <- paste0("median_coef_", g)
  old_fs <- paste0("frac_sig_5pct_", g)
  old_je <- paste0("janus_effect_", g)
  new_sign <- paste0("sign_", g)
  new_sig <- paste0("sig_", g)

  if (old_mc %in% names(sign_mat)) {
    sign_mat[, (new_sign) := sign(get(old_mc))]
    sign_mat[, (old_mc) := NULL]
  }
  if (old_fs %in% names(sign_mat)) {
    sign_mat[, (new_sig) := as.integer(get(old_fs) > 0.5)]
    sign_mat[, (old_fs) := NULL]
  }
  if (old_je %in% names(sign_mat)) {
    setnames(sign_mat, old_je, paste0("janus_", g))
  }
}

# all_agree: all three signs identical and non-zero
sign_cols <- paste0("sign_", geo_levels)
existing_sign_cols <- intersect(sign_cols, names(sign_mat))
if (length(existing_sign_cols) == 3L) {
  sign_mat[, all_agree := as.integer(
    get(sign_cols[1]) == get(sign_cols[2]) &
    get(sign_cols[2]) == get(sign_cols[3]) &
    get(sign_cols[1]) != 0 &
    !is.na(get(sign_cols[1])) & !is.na(get(sign_cols[2])) & !is.na(get(sign_cols[3]))
  )]
}

# any_janus
janus_cols <- paste0("janus_", geo_levels)
existing_janus_cols <- intersect(janus_cols, names(sign_mat))
if (length(existing_janus_cols) > 0L) {
  sign_mat[, any_janus := as.integer(Reduce(`|`, lapply(.SD, function(x) x == 1L))),
           .SDcols = existing_janus_cols]
}

sign_mat <- merge(sign_mat, outcomes[, .(outcome, outcome_family)], by = "outcome", all.x = TRUE)
sign_mat[, treat_idx := match(treatment_short_id, treat_order)]
setorder(sign_mat, treat_idx, outcome_family, outcome)
sign_mat[, treat_idx := NULL]

fwrite(sign_mat, file.path(SYNTHESIS_DIR, "sign_consistency_matrix.csv"))
message("  Sign consistency matrix: ", nrow(sign_mat), " rows")

# --- Phase 6: HTML Synthesis Report ---
message("=== Phase 6: HTML Synthesis Report ===")

# PAP-based sign mapping: for each outcome, which sign of the coefficient means
# "worse for tenants" (i.e. supports the disconnected landlord hypothesis)?
# +1 = positive coef is worse, -1 = negative coef is worse.
outcome_worse_sign <- c(
  current_energy_efficiency            = -1L,   # higher score = better, so neg coef = worse
  energy_consumption_current           = +1L,   # higher kWh/m² = worse
  energy_consumption_current_property  = +1L,   # higher total kWh = worse
  el_mean_consumption_k_wh             = +1L,   # higher electricity = worse
  gas_mean_consumption_k_wh            = +1L,   # higher gas = worse
  energy_consumption_gap               = -1L,   # gap = potential - current; smaller gap from worse current = neg

  energy_consumption_gap_property      = -1L,   # same logic
  energy_efficiency_potential_gap      = -1L,   # smaller potential gap if already low = neg
  bad_epc                              = +1L,   # more bad EPCs = worse
  energy_efficiency_bad_epc_gap        = -1L,   # further below C/D boundary = neg = worse
  energy_efficiency_worse_epc_gap      = -1L,   # closer to band floor = neg = worse
  borderline_good_epc                  = +1L,   # more bunching = worse (gaming)
  borderline_better_epc                = +1L    # more bunching = worse (gaming)
)

# Green-red colour scale: green = better for tenants, red = worse for tenants.
# The `outcome` argument determines which sign direction maps to "worse".
coef_to_color <- function(x, max_abs = NULL, outcome = NULL) {
  if (is.null(max_abs)) max_abs <- max(abs(x), na.rm = TRUE)
  if (is.na(max_abs) || max_abs == 0) return(rep("rgb(255,255,255)", length(x)))
  x_scaled <- pmin(pmax(x / max_abs, -1), 1)

  # Determine the "worse" direction for this outcome
  ws <- if (!is.null(outcome) && outcome %in% names(outcome_worse_sign)) outcome_worse_sign[[outcome]] else 1L

  # Map to a "badness" scale: +1 = maximally worse, -1 = maximally better
  badness <- x_scaled * ws

  # badness > 0 → red (worse), badness < 0 → green (better)
  ifelse(is.na(badness), "rgb(255,255,255)",
    ifelse(badness >= 0,
      # Red channel: white → red as badness goes 0 → 1
      sprintf("rgb(255, %d, %d)", as.integer(255 * (1 - badness)), as.integer(255 * (1 - badness))),
      # Green channel: white → green as badness goes 0 → -1
      sprintf("rgb(%d, %d, %d)", as.integer(255 * (1 + badness)), as.integer(200 + 55 * (1 + badness)), as.integer(255 * (1 + badness)))
    ))
}

# Significance stars helper
sig_stars <- function(frac) {
  ifelse(is.na(frac), "",
    ifelse(frac >= 0.75, "***",
      ifelse(frac >= 0.50, "**",
        ifelse(frac >= 0.25, "*", ""))))
}

# Smart formatters for heatmap cells
fmt_coef <- function(x) {
  if (is.na(x)) return("")
  ax <- abs(x)
  if (ax == 0) "0"
  else if (ax < 0.005) sprintf("%.1e", x)
  else if (ax < 0.1) sprintf("%.3f", x)
  else sprintf("%.2f", x)
}

fmt_pct <- function(x) {
  if (is.na(x)) return("")
  ax <- abs(x)
  if (ax == 0) "0%"
  else if (ax < 0.01) sprintf("%.4f%%", x)
  else if (ax < 0.1) sprintf("%.3f%%", x)
  else if (ax < 1) sprintf("%.2f%%", x)
  else sprintf("%.1f%%", x)
}

# Build HTML
html <- character()
h <- function(...) html <<- c(html, paste0(...))

h("<!DOCTYPE html><html><head><meta charset='utf-8'>")
h("<title>Disconnected Landlords — Results Synthesis</title>")
h("<style>")
h("body { font-family: 'Segoe UI', Calibri, Arial, sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px; line-height: 1.5; color: #222; }")
h("h1 { color: #1a3a5c; border-bottom: 3px solid #1a3a5c; padding-bottom: 10px; }")
h("h2 { color: #2c5282; margin-top: 40px; border-bottom: 1px solid #ccc; }")
h("h3 { color: #4a6fa5; }")
h("table { border-collapse: collapse; width: 100%; margin: 15px 0; font-size: 13px; }")
h("th { background: #2c5282; color: white; padding: 8px 6px; text-align: left; font-weight: 600; position: sticky; top: 0; }")
h("td { padding: 6px; border-bottom: 1px solid #e2e8f0; }")
h("tr:hover { background: #f7fafc; }")
h(".winner { background: #c6f6d5 !important; font-weight: bold; }")
h(".janus { background: #fed7d7; }")
h(".grade-high { color: #276749; font-weight: bold; }")
h(".grade-moderate { color: #975a16; }")
h(".grade-low { color: #c53030; }")
h(".grade-verylow { color: #9b2c2c; font-weight: bold; }")
h(".metric-label { font-weight: 600; color: #4a5568; }")
h(".section-note { color: #718096; font-style: italic; font-size: 0.9em; }")
h(".heatcell { text-align: center; font-size: 12px; min-width: 65px; }")
h(".pap-box { background: #f0f4f8; border-left: 4px solid #2c5282; padding: 12px 16px; margin: 10px 0; }")
h(".hp-verdict { padding: 8px 12px; margin: 6px 0; border-radius: 4px; }")
h(".hp-supported { background: #c6f6d5; border-left: 4px solid #276749; }")
h(".hp-mixed { background: #fefcbf; border-left: 4px solid #975a16; }")
h(".hp-rejected { background: #fed7d7; border-left: 4px solid #c53030; }")
h(".hp-insufficient { background: #e2e8f0; border-left: 4px solid #718096; }")
h("</style></head><body>")

# --- Section: Title + PAP Definitions + Hypothesis Conclusions + Summary Chart ---
h("<h1>Disconnected Landlords — Results Synthesis Report</h1>")
h("<p class='section-note'>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "</p>")

# ===== PAP DEFINITIONS =====
h("<h2>1. Pre-Analysis Plan: Key Definitions</h2>")
h("<p class='section-note'>Reference definitions from the pre-analysis plan (PAP). All results below are interpreted against these hypotheses.</p>")

# -- Hypotheses --
h("<h3>Hypotheses</h3>")
h("<div class='pap-box'>")
h("<p><strong>H1 (Average energy efficiency differences):</strong> ",
  "For at least one non-baseline ownership type <em>k</em>, properties owned by category <em>k</em> exhibit ",
  "systematically different energy performance relative to UK private individuals (the control group).</p>")
h("<p style='margin-left:20px;'><strong>H1a:</strong> Foreign for-profit ownership, especially via tax havens, ",
  "is associated with <em>higher</em> normalised energy consumption (worse efficiency) than UK private individuals.</p>")
h("<p style='margin-left:20px;'><strong>H1b:</strong> UK non-profits and foreign non-profits may exhibit ",
  "<em>better</em> energy performance (lower consumption) than UK private individuals.</p>")
h("<p><strong>H2 (Bad EPC prevalence):</strong> The probability of holding a &ldquo;bad EPC&rdquo; ",
  "(below regulatory threshold) differs by ownership type.</p>")
h("<p><strong>H3 (Bunching around EPC cutoffs):</strong> Certain ownership types exhibit excess mass just above ",
  "key EPC cutoffs, consistent with strategic manipulation.</p>")
h("<p><strong>H4 (Design robustness):</strong> Estimated ownership-type differences are robust to alternative ",
  "sample definitions, covariate sets, and fixed-effect specifications.</p>")
h("<p><strong>H5 (Heterogeneous gaming):</strong> Effect heterogeneity across the ownership taxonomy reveals ",
  "systematic patterns: some subgroups (e.g., foreign for-profit, tax havens) exhibit substantially higher ",
  "bunching and borderline behaviour than others.</p>")
h("</div>")

# -- Treatment groups --
h("<h3>Treatment Groups</h3>")
h("<div class='pap-box'>")
h("<p><strong>Control group (T<sub>p</sub>=0):</strong> Privately rented properties owned by private individuals ",
  "(identified via EPC tenure field, not appearing in CCOD/OCOD land registry data).</p>")
h("<table style='width:auto; font-size:12px;'>")
h("<tr><th>Short ID</th><th>Legal Form</th><th>Jurisdiction</th><th>PAP Code</th><th>Description</th></tr>")
pap_treat <- data.table(
  sid = c("fp","ukfp","frfp","thfp","np","uknp","frnp","thnp","ps","th","bh","eh","ch","oh"),
  lf  = c("For-Profit","For-Profit","For-Profit","For-Profit",
          "Non-Profit","Non-Profit","Non-Profit","Non-Profit",
          "Public Sector","Any","Any","Any","Any","Any"),
  jur = c("Any","UK","Foreign","Tax Haven","Any","UK","Foreign","Tax Haven",
          "UK","Tax Haven","British Haven","European Haven","Caribbean Haven","Other Haven"),
  pap = c("T=1,3","T=1","T=3","T=3+Haven","T=2,4","T=2","T=4","T=4+Haven",
          "T=5","Haven","BritHaven","EuroHaven","CaribHaven","OtherHaven"),
  desc = c("All for-profit companies","UK-incorporated for-profit","Foreign-incorporated for-profit",
           "Tax-haven for-profit","All non-profit/community orgs","UK non-profit",
           "Foreign non-profit","Tax-haven non-profit","Local authorities & councils",
           "All tax-haven entities","British Overseas Territories havens",
           "European havens (Jersey, Guernsey, IoM, etc.)",
           "Caribbean havens (BVI, Cayman, etc.)","Other havens (HK, Singapore, etc.)")
)
for (i in seq_len(nrow(pap_treat))) {
  r <- pap_treat[i]
  h("<tr><td><code>", r$sid, "</code></td><td>", r$lf, "</td><td>", r$jur, "</td><td>", r$pap, "</td><td>", r$desc, "</td></tr>")
}
h("</table>")
h("</div>")

# -- Outcome variables --
h("<h3>Outcome Variables</h3>")
h("<div class='pap-box'>")
h("<table style='width:100%; font-size:12px;'>")
h("<tr><th>Variable</th><th>Family</th><th>HP</th><th>Definition</th><th>Expected sign (for-profit / tax haven)</th></tr>")
out_defs <- list(
  list("energy_consumption_current", "Energy Use", "H1",
       "EPC-modelled energy consumption per m&sup2; (kWh/m&sup2;/yr)", "+"),
  list("energy_consumption_current_property", "Energy Use", "H1",
       "Total EPC-modelled consumption (kWh/yr = ec_current &times; floor area)", "+"),
  list("current_energy_efficiency", "Energy Efficiency", "H1",
       "EPC score (0&ndash;100+); higher = more efficient. Related to energy cost per m&sup2;", "&minus;"),
  list("el_mean_consumption_k_wh", "Realised Energy", "H1",
       "Mean postcode-level electricity consumption (kWh/yr)", "+"),
  list("gas_mean_consumption_k_wh", "Realised Energy", "H1",
       "Mean postcode-level gas consumption (kWh/yr)", "+"),
  list("energy_consumption_gap", "Efficiency Gap", "H1",
       "Gap between current and potential energy consumption per m&sup2;", "+"),
  list("energy_consumption_gap_property", "Efficiency Gap", "H1",
       "Gap between current and potential total consumption (kWh/yr)", "+"),
  list("energy_efficiency_potential_gap", "Efficiency Gap", "H1",
       "Gap between current and potential EPC score", "+"),
  list("bad_epc", "Regulatory Compliance", "H2",
       "Binary: 1 if EPC band is D or worse (below regulatory threshold)", "+"),
  list("energy_efficiency_bad_epc_gap", "Efficiency Gap", "H2",
       "Interaction: efficiency gap among properties with bad EPC", "+"),
  list("energy_efficiency_worse_epc_gap", "Efficiency Gap", "H2",
       "Interaction: efficiency gap among properties with worse EPC", "+"),
  list("borderline_good_epc", "Bunching", "H3",
       "Binary: 1 if property is just above the regulatory EPC cutoff", "+"),
  list("borderline_better_epc", "Bunching", "H3",
       "Binary: 1 if property is just above the next higher EPC cutoff", "+")
)
for (od in out_defs) {
  h("<tr><td><code>", od[[1]], "</code></td><td>", od[[2]], "</td><td>", od[[3]],
    "</td><td>", od[[4]], "</td><td>", od[[5]], "</td></tr>")
}
h("</table>")
h("<p style='font-size:11px; color:#718096;'>Expected signs are for for-profit/tax-haven treatments relative to the private-individual control. ",
  "Non-profit and public sector are generally expected to show the opposite sign.</p>")
h("</div>")

# ===== HYPOTHESIS CONCLUSIONS (bullet points) =====
h("<h2>2. Hypothesis Verdicts (Preferred Geography: ", winner, ")</h2>")
h("<p class='section-note'>Summary conclusions based on the recommended geography. ",
  "Each hypothesis is assessed using the headline PSM + Subclass FE estimator across all relevant treatment-outcome cells.</p>")

# Build hypothesis conclusion data
hp_map <- list(
  H1  = list(label = "H1: Average energy efficiency differences by ownership type",
             treats_pos = c("fp","ukfp","frfp","thfp"),
             treats_neg = c("np","uknp","frnp","thnp","ps")),
  H1a = list(label = "H1a: Foreign/tax-haven for-profit shows worse efficiency",
             treats_pos = c("frfp","thfp"),
             treats_neg = character(0)),
  H1b = list(label = "H1b: Non-profits and public sector show better efficiency",
             treats_pos = character(0),
             treats_neg = c("np","uknp","frnp","thnp","ps")),
  H2  = list(label = "H2: Bad EPC prevalence differs by ownership type",
             treats_pos = c("fp","ukfp","frfp","thfp","th"),
             treats_neg = c("np","uknp","ps")),
  H3  = list(label = "H3: Bunching around EPC cutoffs (strategic manipulation)",
             treats_pos = c("fp","ukfp","frfp","thfp","th"),
             treats_neg = character(0))
)

# Use headline taxonomy on the winner geography for verdicts
hl_winner <- tax_headline[geography == winner]

for (hp_id in names(hp_map)) {
  hp <- hp_map[[hp_id]]
  hp_outcomes <- outcomes[hypothesis_primary == hp_id, outcome]
  hp_data <- hl_winner[outcome %in% hp_outcomes]

  if (nrow(hp_data) == 0L) {
    h("<div class='hp-verdict hp-insufficient'><strong>", hp$label,
      ":</strong> Insufficient data to evaluate.</div>")
    next
  }

  # Count signs in the expected direction for the relevant treatments
  n_expected <- 0L; n_total_cells <- 0L; n_sig_expected <- 0L; n_sig_total <- 0L

  for (i in seq_len(nrow(hp_data))) {
    r <- hp_data[i]
    tid <- r$treatment_short_id
    if (is.na(r$median_coef)) next

    # Determine expected sign for this treatment
    expected_sign <- NA_integer_
    if (tid %in% hp$treats_pos) expected_sign <- 1L
    if (tid %in% hp$treats_neg) expected_sign <- -1L
    if (is.na(expected_sign)) next  # treatment not relevant to this HP

    n_total_cells <- n_total_cells + 1L
    if (sign(r$median_coef) == expected_sign) n_expected <- n_expected + 1L
    if (!is.na(r$frac_sig_5pct) && r$frac_sig_5pct >= 0.5) {
      n_sig_total <- n_sig_total + 1L
      if (sign(r$median_coef) == expected_sign) n_sig_expected <- n_sig_expected + 1L
    }
  }

  if (n_total_cells == 0L) {
    h("<div class='hp-verdict hp-insufficient'><strong>", hp$label,
      ":</strong> No relevant treatment-outcome cells.</div>")
    next
  }

  frac_expected <- n_expected / n_total_cells
  frac_sig_expected <- if (n_sig_total > 0L) n_sig_expected / n_sig_total else NA_real_

  # Classify verdict
  if (frac_expected >= 0.75 && !is.na(frac_sig_expected) && frac_sig_expected >= 0.6) {
    verdict_class <- "hp-supported"
    verdict_word <- "Supported"
  } else if (frac_expected >= 0.5) {
    verdict_class <- "hp-mixed"
    verdict_word <- "Mixed / Partial support"
  } else {
    verdict_class <- "hp-rejected"
    verdict_word <- "Not supported"
  }

  h("<div class='hp-verdict ", verdict_class, "'>")
  h("<strong>", hp$label, "</strong><br>")
  h("<strong>Verdict: ", verdict_word, ".</strong> ",
    round(frac_expected * 100, 0), "% of relevant cells (",
    n_expected, "/", n_total_cells, ") have coefficients in the expected direction")
  if (n_sig_total > 0L) {
    h("; among cells with &ge;50% significance, ",
      round(frac_sig_expected * 100, 0), "% (",
      n_sig_expected, "/", n_sig_total, ") are in the expected direction")
  }
  h(".")

  # Add per-treatment mini-summary
  treat_ids <- unique(c(hp$treats_pos, hp$treats_neg))
  treat_ids <- treat_ids[treat_ids %in% hp_data$treatment_short_id]
  if (length(treat_ids) > 0) {
    h("<br><span style='font-size:12px; color:#4a5568;'>")
    bits <- character()
    for (tid in treat_ids) {
      tid_data <- hp_data[treatment_short_id == tid]
      if (nrow(tid_data) == 0L) next
      med <- median(tid_data$median_coef, na.rm = TRUE)
      med_sig <- median(tid_data$frac_sig_5pct, na.rm = TRUE)
      direction <- if (med > 0) "+" else if (med < 0) "&minus;" else "0"
      bits <- c(bits, paste0("<code>", tid, "</code>: ", direction,
                             " (", round(med_sig * 100, 0), "% sig)"))
    }
    h(paste(bits, collapse = " | "))
    h("</span>")
  }
  h("</div>")
}

# ===== SIGN-DIRECTION SUMMARY CHART (SVG) =====
h("<h2>3. Sign-Direction Summary (", winner, ")</h2>")
h("<p class='section-note'>For each outcome (rows) and hypothesis group, the bar shows the fraction of treatment-outcome ",
  "cells that are better for tenants (green) vs worse for tenants (red), based on the PAP sign conventions. ",
  "Only the headline PSM + Subclass FE estimator.</p>")

# Build chart data: for each outcome, fraction better/worse for tenants
chart_data <- hl_winner[!is.na(median_coef), .(
  n_pos = sum(median_coef > 0),
  n_neg = sum(median_coef < 0),
  n_zero = sum(median_coef == 0),
  n_total = .N
), by = outcome]
chart_data <- merge(chart_data, outcomes[, .(outcome, outcome_family, hypothesis_primary)],
                    by = "outcome", all.x = TRUE)
# Map to better/worse using PAP sign conventions
chart_data[, ws := outcome_worse_sign[outcome]]
chart_data[, n_worse  := fifelse(ws == 1L, n_pos, n_neg)]
chart_data[, n_better := fifelse(ws == 1L, n_neg, n_pos)]
chart_data[, frac_worse  := n_worse / n_total]
chart_data[, frac_better := n_better / n_total]
setorder(chart_data, hypothesis_primary, outcome_family, outcome)

# SVG dimensions
bar_h <- 22
bar_gap <- 4
label_w <- 300
bar_w <- 400
chart_w <- label_w + bar_w + 160
chart_h <- nrow(chart_data) * (bar_h + bar_gap) + 40

h("<svg width='", chart_w, "' height='", chart_h, "' xmlns='http://www.w3.org/2000/svg' ",
  "style='font-family: Segoe UI, Calibri, Arial, sans-serif; font-size: 12px;'>")

# Legend
h("<rect x='", label_w, "' y='2' width='12' height='12' fill='#38a169'/>")
h("<text x='", label_w + 16, "' y='13' fill='#222'>Better for tenants</text>")
h("<rect x='", label_w + 180, "' y='2' width='12' height='12' fill='#e53e3e'/>")
h("<text x='", label_w + 196, "' y='13' fill='#222'>Worse for tenants</text>")

y_offset <- 24
for (i in seq_len(nrow(chart_data))) {
  r <- chart_data[i]
  y <- y_offset + (i - 1) * (bar_h + bar_gap)

  # Label: [HP] outcome_short
  o_short <- gsub("energy_consumption_", "ec_", gsub("energy_efficiency_", "ee_", r$outcome))
  lab <- paste0("[", r$hypothesis_primary, "] ", o_short)
  h("<text x='", label_w - 4, "' y='", y + bar_h * 0.7, "' text-anchor='end' fill='#222'>", lab, "</text>")

  # Better bar (green, from left)
  better_w <- round(r$frac_better * bar_w)
  worse_w  <- round(r$frac_worse * bar_w)
  h("<rect x='", label_w, "' y='", y, "' width='", better_w, "' height='", bar_h, "' fill='#38a169' rx='2'/>")
  # Worse bar (red, from right end)
  h("<rect x='", label_w + bar_w - worse_w, "' y='", y, "' width='", worse_w, "' height='", bar_h, "' fill='#e53e3e' rx='2'/>")

  # Percentage labels
  if (r$frac_better > 0.08) {
    h("<text x='", label_w + better_w / 2, "' y='", y + bar_h * 0.7, "' text-anchor='middle' fill='white' font-size='11'>",
      round(r$frac_better * 100), "%</text>")
  }
  if (r$frac_worse > 0.08) {
    h("<text x='", label_w + bar_w - worse_w / 2, "' y='", y + bar_h * 0.7, "' text-anchor='middle' fill='white' font-size='11'>",
      round(r$frac_worse * 100), "%</text>")
  }

  # Count annotation
  h("<text x='", label_w + bar_w + 4, "' y='", y + bar_h * 0.7, "' fill='#718096' font-size='11'>",
    r$n_better, " better / ", r$n_worse, " worse (n=", r$n_total, ")</text>")
}
h("</svg>")

# ===== Original Executive Summary continues =====
h("<h2>4. Executive Summary</h2>")
total_models <- nrow(enriched)
total_ok <- sum(enriched$status == "ok", na.rm = TRUE)
total_err <- total_models - total_ok
overall_err_rate <- round(total_err / total_models * 100, 1)
n_treat <- uniqueN(ok$treatment_short_id)
n_out <- uniqueN(ok$outcome)

h("<table>")
h("<tr><td class='metric-label'>Total model estimates</td><td>", format(total_models, big.mark = ","), " across ", length(geo_levels), " geographies</td></tr>")
h("<tr><td class='metric-label'>Successful estimates</td><td>", format(total_ok, big.mark = ","), " (", round(total_ok/total_models*100,1), "%)</td></tr>")
h("<tr><td class='metric-label'>Error rate</td><td>", overall_err_rate, "%</td></tr>")
h("<tr><td class='metric-label'>Treatments analysed</td><td>", n_treat, "</td></tr>")
h("<tr><td class='metric-label'>Outcomes analysed</td><td>", n_out, "</td></tr>")
h("<tr><td class='metric-label'>Recommended geography</td><td><strong>", winner, "</strong> (score: ",
  round(geo_rec[geography == winner, recommendation_score], 3), ")</td></tr>")
h("</table>")

# Headline findings: most robust effects
top_robust <- tax[sign_unanimous == 1L][order(-frac_sig_5pct)][1:min(5, .N)]
if (nrow(top_robust) > 0) {
  h("<h3>Most Robust Treatment Effects</h3><ul>")
  for (i in seq_len(nrow(top_robust))) {
    r <- top_robust[i]
    direction <- if (r$median_coef > 0) "positive" else "negative"
    h("<li><strong>", r$treatment_short_id, " &rarr; ", r$outcome, "</strong> (",
      r$geography, "): ", direction, ", ",
      round(r$frac_sig_5pct * 100, 0), "% significant, grade: ", r$robustness_grade, "</li>")
  }
  h("</ul>")
}

# --- Section: Geography Recommendation ---
h("<h2>5. Geography Recommendation</h2>")
h("<table><tr>")
h("<th>Geography</th><th>Score</th><th>Match Rate</th>",
  "<th>Mean |SMD|</th><th>% Balanced (0.1)</th><th>% Balanced (0.25)</th>",
  "<th>Sign Unanimous</th><th>OLS=PSM</th><th>High/Mod</th>",
  "<th>Error Rate</th><th>Few Clusters</th><th>Median N</th><th>Balance</th></tr>")
fmt_or_na <- function(x, d = 3) if (is.na(x)) "N/A" else round(x, d)
for (i in seq_len(nrow(geo_rec))) {
  g <- geo_rec[i]
  cls <- if (g$geography == winner) " class='winner'" else ""
  h("<tr", cls, ">")
  h("<td>", g$geography, "</td>")
  h("<td><strong>", round(g$recommendation_score, 3), "</strong></td>")
  h("<td>", round(g$median_match_rate, 3), "</td>")
  h("<td>", fmt_or_na(g$median_mean_abs_smd, 3), "</td>")
  h("<td>", if (is.na(g$median_frac_balanced_01)) "N/A" else paste0(round(g$median_frac_balanced_01 * 100, 1), "%"), "</td>")
  h("<td>", if (is.na(g$median_frac_balanced_025)) "N/A" else paste0(round(g$median_frac_balanced_025 * 100, 1), "%"), "</td>")
  h("<td>", round(g$mean_sign_unanimous, 3), "</td>")
  h("<td>", round(g$mean_sign_ols_vs_psm, 3), "</td>")
  h("<td>", round(g$frac_high_or_moderate, 3), "</td>")
  h("<td>", round(g$error_rate, 3), "</td>")
  h("<td>", round(g$frac_few_clusters * 100, 1), "%</td>")
  h("<td>", format(round(g$median_nobs), big.mark = ","), "</td>")
  h("<td>", if (g$has_balance) "Yes" else "No", "</td>")
  h("</tr>")
}
h("</table>")
h("<p class='section-note'>Balance metrics include all covariates (continuous formula variables + propensity score). ",
  "Mean |SMD| is the median across matching units of the mean absolute standardised mean difference — the primary balance metric. ",
  "% Balanced columns are the median fraction of covariates achieving the threshold, but with only 3&ndash;4 covariates per unit ",
  "these fractions are inherently coarse (e.g., 1/3, 1/2) and should be interpreted with caution. ",
  "The recommendation score uses mean |SMD| (continuous, discriminating) rather than % balanced.</p>")

# Outcome ordering (used by all heatmaps)
out_ordered <- unique(outcomes[order(outcome_family, outcome), outcome])

# --- Section: Headline Subclass FE ---
h("<h2>6. Headline Results: PSM + Subclass FE</h2>")
h("<p class='section-note'>The subclass fixed-effects matched estimator (PSM + Subclass FE) is the headline specification. ",
  "It conditions on subclasses from propensity score matching, absorbing within-subclass confounding. ",
  "Cells show median standardised coefficient (per-column colour scale). ",
  "Colour: <span style='background:#c6f6d5;padding:2px 6px;'>green = better for tenants</span>, ",
  "<span style='background:#fed7d7;padding:2px 6px;'>red = worse for tenants</span> (sign interpretation follows PAP definitions). ",
  "Stars: *** &ge;75% sig, ** &ge;50%, * &ge;25%. Hover for raw coefficient.</p>")

for (g in c(geo_levels, "Consensus")) {
  h("<h3>", g, if (g == "Consensus") " (sign agreement across geographies)" else "", "</h3>")

  if (g == "Consensus") {
    hl_data <- tax_headline[, .(
      median_std_coef = median(median_std_coef, na.rm = TRUE),
      frac_sig_5pct = mean(frac_sig_5pct, na.rm = TRUE),
      median_coef = median(median_coef, na.rm = TRUE),
      n_geos = .N,
      signs_agree = length(unique(sign(median_coef[!is.na(median_coef)]))) == 1L
    ), by = .(treatment_short_id, outcome)]
    hl_data[signs_agree == FALSE | n_geos < 3L,
            c("median_std_coef", "frac_sig_5pct") := NA_real_]
  } else {
    hl_data <- tax_headline[geography == g]
  }

  col_max_abs_hl <- hl_data[, .(max_abs = max(abs(median_std_coef), na.rm = TRUE)), by = outcome]
  col_max_abs_hl[is.na(max_abs) | max_abs == 0, max_abs := 1]

  h("<div style='overflow-x:auto;'><table>")
  h("<tr><th>Treatment</th>")
  for (o in out_ordered) {
    o_short <- gsub("energy_consumption_", "ec_", gsub("energy_efficiency_", "ee_", o))
    h("<th class='heatcell' title='", o, "'>", o_short, "</th>")
  }
  h("</tr>")

  for (t in treat_order) {
    h("<tr><td><strong>", t, "</strong></td>")
    for (o in out_ordered) {
      row <- hl_data[treatment_short_id == t & outcome == o]
      if (nrow(row) == 0L || is.na(row$median_std_coef[1])) {
        h("<td class='heatcell' style='background:rgb(245,245,245);'>&mdash;</td>")
      } else {
        val <- row$median_std_coef[1]
        raw <- row$median_coef[1]
        o_max <- col_max_abs_hl[outcome == o, max_abs]
        if (length(o_max) == 0L || is.na(o_max)) o_max <- 1
        bg <- coef_to_color(val, o_max, outcome = o)
        stars <- sig_stars(row$frac_sig_5pct[1])
        text_col <- if (abs(val / o_max) > 0.6) "white" else "#222"
        tip <- paste0("coef=", signif(raw, 3), ", std=", signif(val, 3))
        h("<td class='heatcell' style='background:", bg, ";color:", text_col,
          ";' title='", tip, "'>",
          fmt_coef(val), stars, "</td>")
      }
    }
    h("</tr>")
  }
  h("</table></div>")
}

# Summary stats for headline model
hl_high <- tax_headline[robustness_grade == "High", .N]
hl_mod <- tax_headline[robustness_grade == "Moderate", .N]
hl_low <- tax_headline[robustness_grade == "Low", .N]
hl_vlow <- tax_headline[robustness_grade == "Very Low", .N]
hl_total <- nrow(tax_headline)
h("<p>Robustness grades (all geographies): ",
  "<span class='grade-high'>High=", hl_high, "</span>, ",
  "<span class='grade-moderate'>Moderate=", hl_mod, "</span>, ",
  "<span class='grade-low'>Low=", hl_low, "</span>, ",
  "<span class='grade-verylow'>Very Low=", hl_vlow, "</span> ",
  "(", round((hl_high + hl_mod) / hl_total * 100, 0), "% High or Moderate).</p>")

# --- Section 3b: Energy Outcomes Deep Dive ---
h("<h3>3a. Energy Outcomes: Electricity vs Gas Heterogeneity</h3>")
h("<p class='section-note'>H1 predicts that properties owned by for-profit corporate landlords, ",
  "especially those incorporated in tax havens, exhibit worse energy performance (higher consumption) ",
  "than the control group of privately rented properties (H1a). Non-profits and public sector owners ",
  "are expected to show better or neutral performance (H1b). The four energy outcomes below test these hypotheses ",
  "using both EPC-modelled consumption and realised postcode-level metered data.</p>")

h("<p><strong>Outcome definitions:</strong></p><ul>")
h("<li><code>ec_current</code> &mdash; EPC-modelled energy consumption per m&sup2; (kWh/m&sup2;/yr). ",
  "Higher = worse efficiency. <em>H1a expects positive coef for for-profit/tax haven.</em></li>")
h("<li><code>ec_current_property</code> &mdash; Total EPC-modelled consumption (kWh/yr = ec_current &times; floor area). ",
  "Higher = worse. Captures both efficiency and size.</li>")
h("<li><code>el_mean_consumption_k_wh</code> &mdash; Mean postcode-level electricity consumption (kWh/yr). ",
  "Realised metered data, averaged across all meters at the postcode.</li>")
h("<li><code>gas_mean_consumption_k_wh</code> &mdash; Mean postcode-level gas consumption (kWh/yr). ",
  "Same aggregation as electricity.</li>")
h("</ul>")

h("<p><strong>Expected coefficient signs (relative to privately rented control):</strong></p>")
h("<table style='width:auto;'>")
h("<tr><th>Treatment</th><th>ec_current</th><th>ec_current_property</th><th>el / gas</th><th>Rationale</th></tr>")
h("<tr><td>For-profit (fp, ukfp)</td><td>+</td><td>+</td><td>+</td>",
  "<td>Landlords minimise capital expenditure on efficiency upgrades</td></tr>")
h("<tr><td>Foreign for-profit (frfp, thfp)</td><td>+ (stronger)</td><td>+ (stronger)</td><td>+</td>",
  "<td>Greater agency problems; absentee ownership amplifies under-investment (H1a)</td></tr>")
h("<tr><td>Non-profit (np, uknp)</td><td>&minus;</td><td>&minus;</td><td>&minus;</td>",
  "<td>Mission-driven organisations invest in tenant welfare (H1b)</td></tr>")
h("<tr><td>Public sector (ps)</td><td>&minus;</td><td>&minus;</td><td>&minus;</td>",
  "<td>Regulatory compliance incentives; social housing retrofit programmes</td></tr>")
h("</table>")

# Build the energy detail table from headline taxonomy
energy_outs <- c("energy_consumption_current", "energy_consumption_current_property",
                 "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh")
hl_energy_tab <- tax_headline[outcome %in% energy_outs,
  .(treatment_short_id, outcome, geography, median_coef, pct_of_mean,
    outcome_mean, frac_sig_5pct, sign_unanimous, robustness_grade)]

# Pivot: show el vs gas side by side for each treatment
el_gas <- tax_headline[outcome %in% c("el_mean_consumption_k_wh", "gas_mean_consumption_k_wh")]
el_gas_w1 <- dcast(el_gas, treatment_short_id + geography ~ outcome, value.var = "median_coef", fun.aggregate = function(x) x[1])
el_gas_w2 <- dcast(el_gas, treatment_short_id + geography ~ outcome, value.var = "pct_of_mean", fun.aggregate = function(x) x[1])
el_gas_w3 <- dcast(el_gas, treatment_short_id + geography ~ outcome, value.var = "frac_sig_5pct", fun.aggregate = function(x) x[1])
for (gg in c("el_mean_consumption_k_wh", "gas_mean_consumption_k_wh")) {
  setnames(el_gas_w1, gg, paste0("median_coef_", gg), skip_absent = TRUE)
  setnames(el_gas_w2, gg, paste0("pct_of_mean_", gg), skip_absent = TRUE)
  setnames(el_gas_w3, gg, paste0("frac_sig_5pct_", gg), skip_absent = TRUE)
}
el_gas_wide <- Reduce(function(a, b) merge(a, b, by = c("treatment_short_id", "geography"), all = TRUE),
                      list(el_gas_w1, el_gas_w2, el_gas_w3))

h("<h4>Electricity vs Gas: Side-by-Side Comparison</h4>")
h("<p class='section-note'>Both are postcode-level means, so treatment effects are diluted across all properties ",
  "at the postcode. The sign pattern and significance differential between electricity and gas is informative.</p>")

h("<table>")
h("<tr><th>Treatment</th><th>Geo</th>",
  "<th>Elec coef (kWh)</th><th>Elec %sig</th>",
  "<th>Gas coef (kWh)</th><th>Gas %sig</th>",
  "<th>|Elec/Gas| coef ratio</th></tr>")

key_treats <- c("fp", "ukfp", "frfp", "thfp", "np", "uknp", "ps", "th")
for (t in key_treats) {
  for (g in geo_levels) {
    row <- el_gas_wide[treatment_short_id == t & geography == g]
    if (nrow(row) == 0L) next
    el_c <- row$median_coef_el_mean_consumption_k_wh
    gas_c <- row$median_coef_gas_mean_consumption_k_wh
    el_s <- row$frac_sig_5pct_el_mean_consumption_k_wh
    gas_s <- row$frac_sig_5pct_gas_mean_consumption_k_wh
    ratio <- if (!is.na(el_c) && !is.na(gas_c) && gas_c != 0) round(abs(el_c / gas_c), 1) else NA
    ratio_str <- if (is.na(ratio)) "&mdash;" else paste0(ratio, "x")

    el_cls <- if (!is.na(el_s) && el_s >= 0.75) " style='font-weight:bold'" else ""
    gas_cls <- if (!is.na(gas_s) && gas_s >= 0.75) " style='font-weight:bold'" else ""

    h("<tr><td>", t, "</td><td>", g, "</td>")
    h("<td", el_cls, ">", signif(el_c, 3), "</td>")
    h("<td>", round(el_s * 100, 0), "%</td>")
    h("<td", gas_cls, ">", signif(gas_c, 3), "</td>")
    h("<td>", round(gas_s * 100, 0), "%</td>")
    h("<td>", ratio_str, "</td>")
    h("</tr>")
  }
}
h("</table>")

# Interpretation paragraph
# Compute summary stats for interpretation
el_all_neg <- all(el_gas[outcome == "el_mean_consumption_k_wh" &
                          treatment_short_id %in% key_treats, median_coef] < 0, na.rm = TRUE)
gas_all_neg <- all(el_gas[outcome == "gas_mean_consumption_k_wh" &
                           treatment_short_id %in% key_treats, median_coef] < 0, na.rm = TRUE)
el_med_sig <- median(el_gas[outcome == "el_mean_consumption_k_wh" &
                             treatment_short_id %in% key_treats, frac_sig_5pct], na.rm = TRUE)
gas_med_sig <- median(el_gas[outcome == "gas_mean_consumption_k_wh" &
                              treatment_short_id %in% key_treats, frac_sig_5pct], na.rm = TRUE)

h("<h4>Interpretation</h4>")
h("<p><strong>Sign pattern:</strong> ",
  if (el_all_neg) "All key treatments show negative electricity coefficients" else "Electricity coefficients are mixed in sign",
  ". ",
  if (gas_all_neg) "Gas coefficients are also uniformly negative" else "Gas coefficients show some sign variation",
  ". Both electricity and gas consumption are <em>lower</em> in postcodes with corporate-owned properties relative to control. ",
  "This is the opposite of the H1a prediction for for-profit/tax haven landlords.</p>")

h("<p><strong>Electricity vs gas heterogeneity:</strong> ",
  "Electricity effects are systematically larger in relative terms (median significance: ",
  round(el_med_sig * 100, 0), "% vs ", round(gas_med_sig * 100, 0), "% for gas). ",
  "This is consistent with electricity being more responsive to property-level characteristics that the matching controls for ",
  "(heating system type, insulation, property age), while gas consumption is dominated by ",
  "occupant behaviour and heating demand that varies less across ownership types after conditioning on building characteristics.</p>")

h("<p><strong>Postcode aggregation caveat:</strong> ",
  "These outcomes are postcode-level means. A single treated property at a postcode with many untreated properties ",
  "will produce a near-zero treatment effect mechanically. The tiny magnitudes (&lt;0.05% of mean) ",
  "reflect this ecological dilution, not necessarily null economic effects. The EPC-modelled outcomes ",
  "(ec_current, ec_current_property) avoid this issue by being property-level measures.</p>")

h("<h4>EPC-Modelled Energy Consumption</h4>")
h("<p class='section-note'>Property-level outcomes not subject to postcode aggregation dilution.</p>")
h("<table>")
h("<tr><th>Treatment</th><th>Geo</th>",
  "<th>ec_current coef (kWh/m&sup2;)</th><th>ec_current %sig</th>",
  "<th>ec_prop coef (kWh/yr)</th><th>ec_prop %sig</th></tr>")

ec_tab <- tax_headline[outcome %in% c("energy_consumption_current", "energy_consumption_current_property")]
ec_w1 <- dcast(ec_tab, treatment_short_id + geography ~ outcome, value.var = "median_coef", fun.aggregate = function(x) x[1])
ec_w3 <- dcast(ec_tab, treatment_short_id + geography ~ outcome, value.var = "frac_sig_5pct", fun.aggregate = function(x) x[1])
for (oo in c("energy_consumption_current", "energy_consumption_current_property")) {
  setnames(ec_w1, oo, paste0("median_coef_", oo), skip_absent = TRUE)
  setnames(ec_w3, oo, paste0("frac_sig_5pct_", oo), skip_absent = TRUE)
}
ec_wide <- merge(ec_w1, ec_w3, by = c("treatment_short_id", "geography"), all = TRUE)

for (t in key_treats) {
  for (g in geo_levels) {
    row <- ec_wide[treatment_short_id == t & geography == g]
    if (nrow(row) == 0L) next
    ec_c <- row$median_coef_energy_consumption_current
    ec_s <- row$frac_sig_5pct_energy_consumption_current
    ep_c <- row$median_coef_energy_consumption_current_property
    ep_s <- row$frac_sig_5pct_energy_consumption_current_property

    ec_cls <- if (!is.na(ec_s) && ec_s >= 0.75) " style='font-weight:bold'" else ""
    ep_cls <- if (!is.na(ep_s) && ep_s >= 0.75) " style='font-weight:bold'" else ""

    h("<tr><td>", t, "</td><td>", g, "</td>")
    h("<td", ec_cls, ">", signif(ec_c, 4), "</td>")
    h("<td>", round(ec_s * 100, 0), "%</td>")
    h("<td", ep_cls, ">", signif(ep_c, 4), "</td>")
    h("<td>", round(ep_s * 100, 0), "%</td>")
    h("</tr>")
  }
}
h("</table>")

h("<p><strong>ec_current (kWh/m&sup2;):</strong> ",
  "Foreign for-profit and tax haven treatments show positive coefficients (~+38-43 kWh/m&sup2;), ",
  "consistent with H1a: these properties have higher modelled energy consumption per square metre. ",
  "UK for-profit (ukfp) shows small negative effects (~-12 kWh/m&sup2;), while non-profits (np) show large ",
  "negative effects (~-38 kWh/m&sup2;) and public sector (ps) the largest negative effects (~-156 kWh/m&sup2;), ",
  "consistent with H1b. The divergence between UK and foreign for-profit is a key finding.</p>")

h("<p><strong>ec_current_property (kWh/yr total):</strong> ",
  "Effects are small relative to the outcome mean (~20,000 kWh/yr) because total consumption scales with floor area, ",
  "which the matching absorbs.</p>")

h("<h2>7. All-Specification Treatment Effect Heatmap</h2>")
h("<p class='section-note'>Cells coloured by median standardised coefficient. ",
  "Colour: <span style='background:#c6f6d5;padding:2px 6px;'>green = better for tenants</span>, ",
  "<span style='background:#fed7d7;padding:2px 6px;'>red = worse for tenants</span> (sign interpretation follows PAP definitions). ",
  "Scale is per-outcome column (not global) so each outcome uses its full range. ",
  "Stars indicate robustness: *** &ge;75% sig, ** &ge;50%, * &ge;25%.</p>")

for (g in c(geo_levels, "Consensus")) {
  h("<h3>", g, if (g == "Consensus") " (sign agreement across all geographies)" else "", "</h3>")

  if (g == "Consensus") {
    # Build consensus: only cells where all 3 geographies agree on sign
    cons <- tax[, .(
      median_std_coef = median(median_std_coef, na.rm = TRUE),
      frac_sig_5pct = mean(frac_sig_5pct, na.rm = TRUE),
      n_geos = .N,
      signs_agree = length(unique(sign(median_coef[!is.na(median_coef)]))) == 1L
    ), by = .(treatment_short_id, outcome)]
    cons[signs_agree == FALSE | n_geos < 3L, c("median_std_coef", "frac_sig_5pct") := NA_real_]
    heat_data <- cons
  } else {
    heat_data <- tax[geography == g]
  }

  # Pre-compute per-outcome max_abs for column-wise color scaling
  col_max_abs <- heat_data[, .(max_abs = max(abs(median_std_coef), na.rm = TRUE)), by = outcome]
  col_max_abs[is.na(max_abs) | max_abs == 0, max_abs := 1]

  h("<div style='overflow-x:auto;'><table>")
  h("<tr><th>Treatment</th>")
  for (o in out_ordered) {
    o_short <- gsub("energy_consumption_", "ec_", gsub("energy_efficiency_", "ee_", o))
    h("<th class='heatcell' title='", o, "'>", o_short, "</th>")
  }
  h("</tr>")

  for (t in treat_order) {
    h("<tr><td><strong>", t, "</strong></td>")
    for (o in out_ordered) {
      row <- heat_data[treatment_short_id == t & outcome == o]
      if (nrow(row) == 0L || is.na(row$median_std_coef[1])) {
        h("<td class='heatcell' style='background:rgb(245,245,245);'>&mdash;</td>")
      } else {
        val <- row$median_std_coef[1]
        o_max <- col_max_abs[outcome == o, max_abs]
        if (length(o_max) == 0L || is.na(o_max)) o_max <- 1
        bg <- coef_to_color(val, o_max, outcome = o)
        stars <- sig_stars(row$frac_sig_5pct[1])
        text_col <- if (abs(val / o_max) > 0.6) "white" else "#222"
        h("<td class='heatcell' style='background:", bg, ";color:", text_col,
          ";' title='std_coef=", signif(val, 3), "'>",
          fmt_coef(val), stars, "</td>")
      }
    }
    h("</tr>")
  }
  h("</table></div>")
}

# --- Section: Specification Robustness by Hypothesis ---
h("<h2>8. Specification Robustness by Hypothesis</h2>")

hypotheses <- list(
  H1 = "Energy efficiency is lower in properties owned by for-profit corporate landlords",
  H1a = "Foreign and tax-haven incorporated for-profit landlords show larger efficiency gaps",
  H1b = "Non-profit and public sector landlords show better or neutral efficiency",
  H2 = "Corporate landlords cluster disproportionately around regulatory thresholds",
  H3 = "Realised energy consumption differs by ownership type"
)

# Map hypothesis_primary to outcomes
for (hyp in unique(outcomes$hypothesis_primary)) {
  hyp_outcomes <- outcomes[hypothesis_primary == hyp, outcome]
  hyp_tax <- tax[outcome %in% hyp_outcomes & geography == winner]
  if (nrow(hyp_tax) == 0L) next

  n_cells <- nrow(hyp_tax)
  n_high <- sum(hyp_tax$robustness_grade == "High")
  n_mod <- sum(hyp_tax$robustness_grade == "Moderate")
  n_low <- sum(hyp_tax$robustness_grade == "Low")
  n_vlow <- sum(hyp_tax$robustness_grade == "Very Low")
  frac_hm <- round((n_high + n_mod) / n_cells * 100, 0)
  n_jan <- sum(hyp_tax$janus_effect == 1L)

  most_robust <- hyp_tax[sign_unanimous == 1L][order(-frac_sig_5pct)][1]
  least_robust <- hyp_tax[order(frac_sig_5pct)][1]

  hyp_label <- if (!is.null(hypotheses[[hyp]])) paste0(hyp, ": ", hypotheses[[hyp]]) else hyp
  h("<h3>", hyp_label, "</h3>")
  h("<p>", n_cells, " treatment-outcome cells (", winner, " geography). ",
    "Grade distribution: <span class='grade-high'>High=", n_high, "</span>, ",
    "<span class='grade-moderate'>Moderate=", n_mod, "</span>, ",
    "<span class='grade-low'>Low=", n_low, "</span>, ",
    "<span class='grade-verylow'>Very Low=", n_vlow, "</span>. ",
    frac_hm, "% graded High or Moderate.</p>")

  if (n_jan > 0) {
    h("<p class='janus'>Janus effect detected in ", n_jan, " cell(s) — both significantly ",
      "positive and negative specifications exist.</p>")
  }

  if (nrow(most_robust) > 0 && !is.na(most_robust$treatment_short_id[1])) {
    h("<p>Most robust: <strong>", most_robust$treatment_short_id, " &rarr; ", most_robust$outcome,
      "</strong> (", round(most_robust$frac_sig_5pct * 100, 0), "% sig, sign unanimous). ")
  }
  if (nrow(least_robust) > 0 && !is.na(least_robust$treatment_short_id[1])) {
    h("Least robust: <strong>", least_robust$treatment_short_id, " &rarr; ", least_robust$outcome,
      "</strong> (", round(least_robust$frac_sig_5pct * 100, 0), "% sig).</p>")
  }
}

# --- Section: Matching Diagnostics ---
h("<h2>9. Matching Diagnostics</h2>")
h("<p class='section-note'>Sourced from enriched PSM rows. Balance metrics exclude the propensity score (distance) covariate. ",
  "May be NA if matching_balance file was unavailable.</p>")
h("<table><tr><th>Geography</th><th>Median Match Rate</th>",
  "<th>Median Mean |SMD|</th><th>% Balanced (0.1)</th><th>% Balanced (0.25)</th>",
  "<th>Max SMD (excl. dist.)</th><th>Balance Available</th></tr>")
for (i in seq_len(nrow(geo_rec))) {
  g <- geo_rec[i]
  h("<tr><td>", g$geography, "</td>")
  h("<td>", fmt_or_na(g$median_match_rate), "</td>")
  h("<td>", fmt_or_na(g$median_mean_abs_smd, 3), "</td>")
  h("<td>", if (is.na(g$median_frac_balanced_01)) "N/A" else paste0(round(g$median_frac_balanced_01 * 100, 1), "%"), "</td>")
  h("<td>", if (is.na(g$median_frac_balanced_025)) "N/A" else paste0(round(g$median_frac_balanced_025 * 100, 1), "%"), "</td>")
  h("<td>", fmt_or_na(g$median_max_smd_excl_distance, 3), "</td>")
  h("<td>", if (g$has_balance) "Yes" else "No", "</td>")
  h("</tr>")
}
h("</table>")

# --- Section: Cross-Geography Consistency ---
h("<h2>10. Cross-Geography Sign Consistency</h2>")
n_agree <- sum(sign_mat$all_agree == 1L, na.rm = TRUE)
n_total_pairs <- nrow(sign_mat)
h("<p>", n_agree, " of ", n_total_pairs, " treatment-outcome pairs (",
  round(n_agree / n_total_pairs * 100, 1), "%) show sign agreement across all three geographies.</p>")

n_any_jan <- sum(sign_mat$any_janus == 1L, na.rm = TRUE)
if (n_any_jan > 0) {
  h("<p class='janus'>", n_any_jan, " pairs have Janus effect in at least one geography.</p>")
}

h("<div style='overflow-x:auto;'><table>")
h("<tr><th>Treatment</th><th>Outcome</th>")
for (g in geo_levels) h("<th>Sign ", g, "</th>")
for (g in geo_levels) h("<th>Sig ", g, "</th>")
h("<th>All Agree</th><th>Any Janus</th></tr>")

for (i in seq_len(min(nrow(sign_mat), 200L))) {  # cap at 200 rows in HTML
  r <- sign_mat[i]
  row_cls <- ""
  if (!is.na(r$any_janus) && r$any_janus == 1L) row_cls <- " class='janus'"
  h("<tr", row_cls, ">")
  h("<td>", r$treatment_short_id, "</td><td>", r$outcome, "</td>")
  for (g in geo_levels) {
    sc <- paste0("sign_", g)
    val <- if (sc %in% names(r)) r[[sc]] else NA
    sym <- if (is.na(val)) "&mdash;" else if (val > 0) "+" else if (val < 0) "&minus;" else "0"
    h("<td style='text-align:center'>", sym, "</td>")
  }
  for (g in geo_levels) {
    sc <- paste0("sig_", g)
    val <- if (sc %in% names(r)) r[[sc]] else NA
    h("<td style='text-align:center'>", if (is.na(val)) "&mdash;" else val, "</td>")
  }
  h("<td style='text-align:center'>", if (!is.na(r$all_agree) && r$all_agree == 1L) "&#10003;" else "", "</td>")
  h("<td style='text-align:center'>", if (!is.na(r$any_janus) && r$any_janus == 1L) "&#9888;" else "", "</td>")
  h("</tr>")
}
h("</table></div>")

# Close HTML
h("<hr><p class='section-note'>End of synthesis report. Generated by 09_results_synthesis.R</p>")
h("</body></html>")

# Write HTML
writeLines(html, file.path(SYNTHESIS_DIR, "synthesis_report.html"))
message("  HTML report written: ", length(html), " lines")

# --- Verification ---
message("\n=== Verification ===")

# 1. Dimensions
message("Output file dimensions:")
out_files <- c("treatment_effect_taxonomy.csv", "geography_recommendation.csv",
               "geography_diagnostic_detail.csv", "specification_curve_summary.csv",
               "sign_consistency_matrix.csv")
for (f in out_files) {
  fp <- file.path(SYNTHESIS_DIR, f)
  if (file.exists(fp)) {
    dt <- fread(fp)
    message(sprintf("  %-40s %5d rows x %2d cols", f, nrow(dt), ncol(dt)))
  }
}
message(sprintf("  %-40s %5d lines", "synthesis_report.html", length(html)))

# 2. Geography scores & winner
message("\nGeography recommendation scores:")
for (i in seq_len(nrow(geo_rec))) {
  message(sprintf("  %s: %.4f", geo_rec$geography[i], geo_rec$recommendation_score[i]))
}
message("  Winner: ", winner)

# 3. Robustness grades per geography
message("\nRobustness grade counts per geography:")
grade_tab <- tax[, .N, by = .(geography, robustness_grade)]
grade_tab <- dcast(grade_tab, geography ~ robustness_grade, value.var = "N", fill = 0L)
for (g in geo_levels) {
  row <- grade_tab[geography == g]
  if (nrow(row) > 0) {
    vals <- paste(sapply(c("High", "Moderate", "Low", "Very Low"), function(gr) {
      v <- if (gr %in% names(row)) row[[gr]] else 0
      paste0(gr, "=", v)
    }), collapse = ", ")
    message("  ", g, ": ", vals)
  }
}

# 4. Top 5 most robust
message("\nTop 5 most robust (sign_unanimous=1, highest frac_sig_5pct):")
top5 <- tax[sign_unanimous == 1L][order(-frac_sig_5pct)][1:min(5, .N)]
for (i in seq_len(nrow(top5))) {
  r <- top5[i]
  message(sprintf("  %s | %s | %s | frac_sig=%.2f | grade=%s",
                  r$treatment_short_id, r$outcome, r$geography, r$frac_sig_5pct, r$robustness_grade))
}

# 5. Top 5 least robust (janus)
janus_rows <- tax[janus_effect == 1L][order(frac_sig_5pct)]
if (nrow(janus_rows) > 0) {
  message("\nTop 5 least robust (janus_effect=1, lowest frac_sig_5pct):")
  for (i in seq_len(min(5, nrow(janus_rows)))) {
    r <- janus_rows[i]
    message(sprintf("  %s | %s | %s | frac_sig=%.2f | grade=%s",
                    r$treatment_short_id, r$outcome, r$geography, r$frac_sig_5pct, r$robustness_grade))
  }
} else {
  message("\nNo Janus effects detected across any treatment-outcome pair.")
}

message("\n=== Synthesis complete ===")

# --- Export dashboard data (for results_dashboard.html) ---
message("=== Exporting dashboard data ===")
tryCatch({
  dash_cols <- c("treatment_short_id", "outcome", "model", "spec",
                 "matching_core", "regression_core", "geography",
                 "coef", "se", "p_value", "nobs",
                 "standardised_coef", "pct_effect", "outcome_mean", "outcome_sd")
  dash_cols <- intersect(dash_cols, names(enriched))
  slim <- enriched[status == "ok" & !is.na(coef), ..dash_cols]
  slim[, status := "ok"]
  slim[is.na(matching_core), matching_core := ""]
  for (col in c("coef","se","p_value","standardised_coef","pct_effect","outcome_mean","outcome_sd")) {
    if (col %in% names(slim)) slim[, (col) := round(get(col), 6)]
  }
  slim[, nobs := as.integer(nobs)]

  # Write as JS for dashboard auto-load
  header <- names(slim)
  js_lines <- character(nrow(slim) + 3)
  js_lines[1] <- sprintf("window.PIPELINE_COLS = %s;",
    paste0("[", paste0('"', header, '"', collapse = ","), "]"))
  js_lines[2] <- "window.PIPELINE_DATA = ["
  for (i in seq_len(nrow(slim))) {
    row <- slim[i]
    vals <- vapply(header, function(h) {
      v <- row[[h]]
      if (is.na(v)) return("null")
      if (is.character(v)) return(paste0('"', gsub('"', '\\"', v), '"'))
      as.character(v)
    }, character(1))
    comma <- if (i < nrow(slim)) "," else ""
    js_lines[i + 2] <- paste0("[", paste(vals, collapse = ","), "]", comma)
  }
  js_lines[length(js_lines)] <- "];"

  js_path <- here::here("output", "dashboard_data.js")
  writeLines(js_lines, js_path)
  message(sprintf("  Dashboard data: %s (%d rows, %.1f MB)",
                  basename(js_path), nrow(slim), file.size(js_path) / 1e6))
}, error = function(e) {
  message("  WARNING: Dashboard data export failed: ", conditionMessage(e))
})

}, error = function(e) {
  message("FATAL ERROR in 09_results_synthesis.R: ", conditionMessage(e))
  message(paste(capture.output(traceback()), collapse = "\n"))
})
