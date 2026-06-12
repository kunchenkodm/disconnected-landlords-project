# Script: 10_vintage_signflip_table.R
# Purpose: Project 1 (Vintage of Buyer / Offshore Ownership), Task 1.1.
#          Document where adding year-of-transaction (and price paid) to the
#          covariate set flips the sign of the offshore / foreign treatment
#          effect, AND where it moves the estimate in or out of statistical
#          significance.
#
#          Outputs (under output/project1_vintage/, all suffixed with _core
#          when the default core-outcomes filter is active):
#            - signflip_pairs_<GEO>[_core].csv      pair-level table
#            - signflip_summary_<GEO>[_core].csv    per (treatment, outcome)
#            - signflip_note_<GEO>[_core].md        short markdown note
#
# Default scope (PROJECT1_CORE_ONLY != "FALSE"):
#   Core outcome families — Energy Efficiency, Energy Use, Realised Energy —
#   PLUS the two regulatory-bound bad-EPC indicators bad_epc_c (main, below C =
#   incoming MEES) and bad_epc_e (below E = present minimum), pulled in via
#   CORE_EXTRA_OUTCOMES even though they live in the Regulatory Compliance
#   family. They are clean binaries with an unambiguous worse-for-tenants
#   direction, so unlike the gap/bunching diagnostics they support a credible
#   sign. The remaining diagnostic outcomes (Efficiency Gap, Bunching, and the
#   gap/bunching members of Regulatory Compliance) stay dropped because their
#   effects are too imprecise — flips there are dominated by identification
#   fragility rather than substantive change.
#
# Year and price are bundled in the existing spec set:
#   Baseline (no year, no price)         <-> Price Paid (year + price, no tax_band)
#   Council Tax (no year, no price)      <-> Council Tax + Price Paid (year + price)
# So each "pair" holds model, treatment, outcome, regression_core,
# matching_core, and tax_band level fixed; only year+price differ.

rm(list = setdiff(ls(), c("script", "pipeline.start.time"))); gc()

library(data.table)
library(here)

source(here::here("scripts", "00_setup.R"))

wc_suffix <- if (WITHIN_CORPORATE) "_wc" else ""

message("===================================================================")
message("  10_vintage_signflip_table.R - Project 1, Task 1.1")
message(sprintf("  Started: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

OUT_DIR <- here::here("output", "project1_vintage")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Offshore-relevant treatments. Subset of treatment_metadata defined in
# treatment_definitions.R.
OFFSHORE_TREATMENTS <- c("thfp", "thnp", "frfp", "frnp",
                         "th", "bh", "eh", "ch", "oh")

# Core outcome families: where the headline retrofit / consumption story
# lives. Diagnostic families (Efficiency Gap, Regulatory Compliance,
# Bunching) are excluded by default because year+price flips there are
# dominated by precision-fragility rather than substantive change.
CORE_OUTCOME_FAMILIES <- c("Energy Efficiency", "Energy Use", "Realised Energy")

# Extra core outcomes kept regardless of family: the mainline bad-EPC indicators
# at the two regulatory bounds. Clean binaries with an unambiguous worse-for-
# tenants direction (bad_epc_c/e = +1 in outcome_worse_sign), so they belong in
# the core scope even though their family is "Regulatory Compliance".
CORE_EXTRA_OUTCOMES <- c("bad_epc_c", "bad_epc_e")

# Significance threshold (alpha for the two-tailed coefficient test).
SIG_ALPHA <- as.numeric(Sys.getenv("PROJECT1_SIG_ALPHA", unset = "0.05"))
if (!is.finite(SIG_ALPHA) || SIG_ALPHA <= 0 || SIG_ALPHA >= 1) SIG_ALPHA <- 0.05


# Spec -> (has_year, has_price, has_tax_band) lookup.
# Mirrors spec_configs in model_specifications.R.
spec_features <- data.table(
  spec            = c("Baseline", "Council Tax", "Price Paid", "Council Tax + Price Paid"),
  has_year        = c(0L, 0L, 1L, 1L),
  has_price       = c(0L, 0L, 1L, 1L),
  has_tax_band    = c(0L, 1L, 0L, 1L)
)

# Pair definition: same tax_band level, year+price toggled.
spec_pair_table <- data.table(
  spec_no_year = c("Baseline", "Council Tax"),
  spec_year    = c("Price Paid", "Council Tax + Price Paid"),
  tax_band     = c(0L, 1L)
)

# Outcome direction: +1 if positive coef means "worse retrofitting / worse
# for tenants", -1 otherwise. Lifted from 09_results_synthesis.R:404-419.
outcome_worse_sign <- c(
  current_energy_efficiency            = -1L,
  energy_consumption_current           = +1L,
  energy_consumption_current_property  = +1L,
  el_mean_consumption_k_wh             = +1L,
  gas_mean_consumption_k_wh            = +1L,
  energy_consumption_gap               = -1L,
  energy_consumption_gap_property      = -1L,
  energy_efficiency_potential_gap      = -1L,
  energy_efficiency_bad_epc_gap        = -1L,
  energy_efficiency_worse_epc_gap      = -1L,
  borderline_good_epc                  = +1L,
  borderline_better_epc                = +1L,
  # Regulatory-bound variants (C = incoming MEES, E = present minimum); mirror generic directions.
  bad_epc_c                            = +1L,
  bad_epc_e                            = +1L,
  borderline_good_epc_c                = +1L,
  borderline_good_epc_e                = +1L,
  energy_efficiency_c_gap              = -1L,
  energy_efficiency_e_gap              = -1L
)


# Geography selection -----------------------------------------------------
WINNER_GEO <- Sys.getenv("PROJECT1_GEO", unset = "LA")
if (!nzchar(WINNER_GEO)) WINNER_GEO <- "LA"

# Core-only scope (default TRUE): restrict to the three energy-performance
# families that anchor the offshore hypothesis (PAP H1a, Y^(2)). Diagnostic
# families (Efficiency Gap, Regulatory Compliance, Bunching) are dropped.
CORE_ONLY   <- Sys.getenv("PROJECT1_CORE_ONLY", unset = "TRUE") != "FALSE"
core_suffix <- if (CORE_ONLY) "_core" else ""

geo_suffix <- paste0("_", WINNER_GEO, wc_suffix, core_suffix)
message(sprintf("  Geography:        %s", WINNER_GEO))
message(sprintf("  Core-outcomes:    %s", if (CORE_ONLY)
  paste0("TRUE (", paste(CORE_OUTCOME_FAMILIES, collapse = ", "), ")") else
  "FALSE (all families)"))
message(sprintf("  Significance alpha: %.3f", SIG_ALPHA))


# Inputs ------------------------------------------------------------------

enriched_path <- file.path(SUMMARY_TABLES_DIR,
                           paste0("results_enriched_", WINNER_GEO, wc_suffix, ".csv"))
if (!file.exists(enriched_path)) {
  stop("Enriched results not found: ", enriched_path,
       "\n  Run 06_run_regressions.R + 06b_enrich_results.R for ", WINNER_GEO, " first.")
}
enriched <- fread(enriched_path, na.strings = c("NA", ""))
message(sprintf("  Loaded enriched results: %d rows", nrow(enriched)))

taxonomy_path <- file.path(SUMMARY_TABLES_DIR, "treatment_taxonomy.csv")
taxonomy <- if (file.exists(taxonomy_path)) {
  fread(taxonomy_path, na.strings = c("NA", ""))
} else {
  data.table(treatment_short_id = OFFSHORE_TREATMENTS)
}

outcomes_path <- file.path(SUMMARY_TABLES_DIR, "outcome_metadata.csv")
outcomes_meta <- if (file.exists(outcomes_path)) {
  fread(outcomes_path, na.strings = c("NA", ""))
} else {
  data.table(outcome = names(outcome_worse_sign), outcome_family = NA_character_)
}

# Per-band outcome family fallback (06b regenerates outcome_metadata.csv from
# the generic outcome list, so per-band variants may be missing a family).
per_band_family_fallback <- c(
  bad_epc_c               = "Regulatory Compliance",
  bad_epc_e               = "Regulatory Compliance",
  energy_efficiency_c_gap = "Efficiency Gap",
  energy_efficiency_e_gap = "Efficiency Gap",
  borderline_good_epc_c   = "Bunching",
  borderline_good_epc_e   = "Bunching"
)


# Filter + augment --------------------------------------------------------

ok <- enriched[status == "ok" & !is.na(coef) & treatment_short_id %in% OFFSHORE_TREATMENTS]
ok <- merge(ok, spec_features, by = "spec", all.x = TRUE)
ok <- ok[!is.na(has_year)]    # drop rows whose spec is not in our four-spec lookup

# Attach outcome_family for filtering and labelling.
if ("outcome_family" %in% names(outcomes_meta)) {
  ok <- merge(ok, outcomes_meta[, .(outcome, outcome_family)],
              by = "outcome", all.x = TRUE, sort = FALSE)
} else {
  ok[, outcome_family := NA_character_]
}
ok[is.na(outcome_family) | outcome_family == "", outcome_family := "Unknown"]
ok[outcome_family == "Unknown" & outcome %in% names(per_band_family_fallback),
   outcome_family := per_band_family_fallback[outcome]]

# Core-only scope: keep the three energy-performance families plus the mainline
# bad-EPC regulatory-bound indicators (CORE_EXTRA_OUTCOMES).
if (CORE_ONLY) {
  n_before_core <- nrow(ok)
  ok <- ok[outcome_family %in% CORE_OUTCOME_FAMILIES |
             outcome %in% CORE_EXTRA_OUTCOMES]
  message(sprintf("  Core-only filter: %d -> %d rows (%s; + %s)",
                  n_before_core, nrow(ok),
                  paste(CORE_OUTCOME_FAMILIES, collapse = ", "),
                  paste(CORE_EXTRA_OUTCOMES, collapse = ", ")))
}

if (nrow(ok) == 0L) {
  stop("No usable rows after filtering. Check that offshore treatments ran for ",
       WINNER_GEO, " and (if CORE_ONLY) that core outcomes were estimated.")
}

# Coerce matching_core "" / NA to a sentinel so merge keys round-trip.
ok[, matching_core_key := fifelse(is.na(matching_core) | matching_core == "",
                                  "NA_mc", matching_core)]


# Build pair-level table --------------------------------------------------

pair_long <- ok[, .(model, spec, has_year, has_tax_band,
                    treatment_short_id, outcome, outcome_family,
                    regression_core, matching_core, matching_core_key,
                    coef, se, p_value, nobs, standardised_coef)]

pair_long <- pair_long[spec %in% spec_pair_table$spec_no_year |
                       spec %in% spec_pair_table$spec_year]

no_year <- pair_long[has_year == 0L,
                     .(model, treatment_short_id, outcome, outcome_family,
                       regression_core, matching_core_key, has_tax_band,
                       spec_no_year   = spec,
                       coef_no_year   = coef,
                       se_no_year     = se,
                       p_no_year      = p_value,
                       n_no_year      = nobs,
                       std_no_year    = standardised_coef,
                       matching_core  = matching_core)]

with_year <- pair_long[has_year == 1L,
                       .(model, treatment_short_id, outcome,
                         regression_core, matching_core_key, has_tax_band,
                         spec_year    = spec,
                         coef_year    = coef,
                         se_year      = se,
                         p_year       = p_value,
                         n_year       = nobs,
                         std_year     = standardised_coef)]

pairs <- merge(no_year, with_year,
               by = c("model", "treatment_short_id", "outcome",
                      "regression_core", "matching_core_key", "has_tax_band"),
               all = FALSE)

if (nrow(pairs) == 0L) {
  stop("No pairs constructed. Check that both no-year and with-year specs ran ",
       "on the offshore treatments at this geography.")
}


# Sign + significance diagnostics -----------------------------------------

# Sign-flipped: the two coefs differ in sign and neither is exactly zero.
pairs[, sign_flipped := as.integer(
  !is.na(coef_no_year) & !is.na(coef_year) &
    sign(coef_no_year) != sign(coef_year) &
    coef_no_year != 0 & coef_year != 0
)]

# Worse-direction flag using outcome_worse_sign.
pairs[, ws := outcome_worse_sign[outcome]]
pairs[, sign_no_year_worse := as.integer(!is.na(coef_no_year) & !is.na(ws) &
                                           sign(coef_no_year) * ws > 0)]
pairs[, sign_year_worse    := as.integer(!is.na(coef_year)    & !is.na(ws) &
                                           sign(coef_year)    * ws > 0)]
pairs[, sign_flipped_to_worse := as.integer(sign_flipped == 1L &
                                              sign_year_worse == 1L)]

# Significance flags. Prefer p_value when available; fall back to |t|.
pairs[, sig_no_year := as.integer(!is.na(p_no_year) & p_no_year < SIG_ALPHA)]
pairs[, sig_year    := as.integer(!is.na(p_year)    & p_year    < SIG_ALPHA)]
# Defensive: if p was missing but coef + se are present, derive |t| > 1.96-ish.
crit_t <- abs(qnorm(SIG_ALPHA / 2))
pairs[is.na(p_no_year) & !is.na(coef_no_year) & !is.na(se_no_year) & se_no_year > 0,
      sig_no_year := as.integer(abs(coef_no_year / se_no_year) >= crit_t)]
pairs[is.na(p_year) & !is.na(coef_year) & !is.na(se_year) & se_year > 0,
      sig_year := as.integer(abs(coef_year / se_year) >= crit_t)]

# Pair classification — five mutually exclusive states:
#   both_sig_opposite_sign : both significant, signs disagree (STRONGEST flip)
#   both_sig_same_sign     : both significant, signs agree    (robust effect)
#   gain_sig_year          : year+price brings the estimate into significance
#   lose_sig_year          : year+price knocks it out of significance
#   neither_sig            : both p >= alpha
pairs[, pair_class := fcase(
  sig_no_year == 1L & sig_year == 1L & sign_flipped == 1L, "both_sig_opposite_sign",
  sig_no_year == 1L & sig_year == 1L & sign_flipped == 0L, "both_sig_same_sign",
  sig_no_year == 0L & sig_year == 1L,                       "gain_sig_year",
  sig_no_year == 1L & sig_year == 0L,                       "lose_sig_year",
  default = "neither_sig"
)]

# Direction flags for the gain/lose-sig transitions.
pairs[, gain_sig_to_worse  := as.integer(pair_class == "gain_sig_year"  & sign_year_worse    == 1L)]
pairs[, gain_sig_to_better := as.integer(pair_class == "gain_sig_year"  & sign_year_worse    == 0L)]
pairs[, lose_sig_from_worse  := as.integer(pair_class == "lose_sig_year" & sign_no_year_worse == 1L)]
pairs[, lose_sig_from_better := as.integer(pair_class == "lose_sig_year" & sign_no_year_worse == 0L)]

# A "credible" flip is one where both pair members are significant and the
# signs disagree, regardless of direction. A worse-directed credible flip
# is the strongest evidence that year+price reveals an offshore-worse-for-
# tenants effect.
pairs[, credible_flip          := as.integer(pair_class == "both_sig_opposite_sign")]
pairs[, credible_flip_to_worse := as.integer(credible_flip == 1L & sign_year_worse == 1L)]

pairs[, coef_delta := coef_year - coef_no_year]
pairs[, pct_change := fifelse(coef_no_year != 0,
                              (coef_year - coef_no_year) / abs(coef_no_year),
                              NA_real_)]
pairs[, geography := WINNER_GEO]

# Slice IDs for back-reference into the dashboard / archive.
pairs[, slice_no_year := paste(model, spec_no_year, matching_core_key,
                               regression_core, sep = "|")]
pairs[, slice_year    := paste(model, spec_year, matching_core_key,
                               regression_core, sep = "|")]

setcolorder(pairs, c(
  "geography", "treatment_short_id", "outcome", "outcome_family", "model",
  "regression_core", "matching_core", "matching_core_key", "has_tax_band",
  "spec_no_year", "spec_year",
  "coef_no_year", "se_no_year", "p_no_year", "n_no_year", "std_no_year",
  "coef_year", "se_year", "p_year", "n_year", "std_year",
  "sig_no_year", "sig_year", "pair_class",
  "sign_flipped", "sign_no_year_worse", "sign_year_worse",
  "sign_flipped_to_worse", "credible_flip", "credible_flip_to_worse",
  "gain_sig_to_worse", "gain_sig_to_better",
  "lose_sig_from_worse", "lose_sig_from_better",
  "coef_delta", "pct_change",
  "slice_no_year", "slice_year"
))

# Attach treatment metadata for downstream readers.
if ("legal_form" %in% names(taxonomy)) {
  pairs <- merge(pairs,
                 taxonomy[, .(treatment_short_id, legal_form, jurisdiction)],
                 by = "treatment_short_id", all.x = TRUE, sort = FALSE)
}

pairs_path <- file.path(OUT_DIR, paste0("signflip_pairs", geo_suffix, ".csv"))
fwrite(pairs, pairs_path)
message(sprintf("  Wrote pair-level table: %s (%d rows)",
                basename(pairs_path), nrow(pairs)))


# Per (treatment, outcome) summary ----------------------------------------

summary_dt <- pairs[, .(
  n_pairs                  = .N,
  # Sign behaviour
  n_flipped                = sum(sign_flipped == 1L, na.rm = TRUE),
  frac_flipped             = mean(sign_flipped == 1L, na.rm = TRUE),
  n_flipped_to_worse       = sum(sign_flipped_to_worse == 1L, na.rm = TRUE),
  frac_flipped_to_worse    = mean(sign_flipped_to_worse == 1L, na.rm = TRUE),
  # Significance behaviour
  n_sig_no_year            = sum(sig_no_year == 1L, na.rm = TRUE),
  n_sig_year               = sum(sig_year    == 1L, na.rm = TRUE),
  n_credible_flip          = sum(credible_flip == 1L, na.rm = TRUE),
  n_credible_flip_to_worse = sum(credible_flip_to_worse == 1L, na.rm = TRUE),
  n_gain_sig_to_worse      = sum(gain_sig_to_worse == 1L, na.rm = TRUE),
  n_gain_sig_to_better     = sum(gain_sig_to_better == 1L, na.rm = TRUE),
  n_lose_sig_from_worse    = sum(lose_sig_from_worse == 1L, na.rm = TRUE),
  n_lose_sig_from_better   = sum(lose_sig_from_better == 1L, na.rm = TRUE),
  # Magnitudes
  median_coef_no_year      = median(coef_no_year, na.rm = TRUE),
  median_coef_year         = median(coef_year, na.rm = TRUE),
  median_std_no_year       = median(std_no_year, na.rm = TRUE),
  median_std_year          = median(std_year, na.rm = TRUE),
  median_p_no_year         = median(p_no_year, na.rm = TRUE),
  median_p_year            = median(p_year, na.rm = TRUE)
), by = .(treatment_short_id, outcome)]

if ("legal_form" %in% names(taxonomy)) {
  summary_dt <- merge(summary_dt,
                      taxonomy[, .(treatment_short_id, legal_form, jurisdiction)],
                      by = "treatment_short_id", all.x = TRUE, sort = FALSE)
}
if ("outcome_family" %in% names(outcomes_meta)) {
  summary_dt <- merge(summary_dt,
                      outcomes_meta[, .(outcome, outcome_family)],
                      by = "outcome", all.x = TRUE, sort = FALSE)
  summary_dt[is.na(outcome_family) | outcome_family == "" &
               outcome %in% names(per_band_family_fallback),
             outcome_family := per_band_family_fallback[outcome]]
}
summary_dt[, geography := WINNER_GEO]

# Sort: credible flips first (strongest evidence), then gain-to-worse, then
# raw flipped-to-worse fraction, then unsigned flip fraction.
setorder(summary_dt,
         -n_credible_flip_to_worse, -n_credible_flip,
         -n_gain_sig_to_worse, -frac_flipped_to_worse, -frac_flipped,
         treatment_short_id, outcome)

summary_path <- file.path(OUT_DIR, paste0("signflip_summary", geo_suffix, ".csv"))
fwrite(summary_dt, summary_path)
message(sprintf("  Wrote summary table: %s (%d rows)",
                basename(summary_path), nrow(summary_dt)))


# Markdown note -----------------------------------------------------------

# Pair-class headline counts.
class_counts <- pairs[, .N, by = pair_class][order(-N)]

n_total            <- nrow(pairs)
n_flip             <- sum(pairs$sign_flipped == 1L, na.rm = TRUE)
n_flip_worse       <- sum(pairs$sign_flipped_to_worse == 1L, na.rm = TRUE)
n_credflip         <- sum(pairs$credible_flip == 1L, na.rm = TRUE)
n_credflip_worse   <- sum(pairs$credible_flip_to_worse == 1L, na.rm = TRUE)
n_gain_sig_worse   <- sum(pairs$gain_sig_to_worse == 1L, na.rm = TRUE)
n_gain_sig_better  <- sum(pairs$gain_sig_to_better == 1L, na.rm = TRUE)
n_lose_sig_worse   <- sum(pairs$lose_sig_from_worse == 1L, na.rm = TRUE)
n_lose_sig_better  <- sum(pairs$lose_sig_from_better == 1L, na.rm = TRUE)

top_n <- min(15L, nrow(summary_dt))
top   <- summary_dt[seq_len(top_n)]

note <- character()
note <- c(note, sprintf("# Sign-flip note (Project 1, Task 1.1) — %s", WINNER_GEO))
note <- c(note, "")
note <- c(note, sprintf("_Generated: %s_", format(Sys.time(), "%Y-%m-%d %H:%M")))
note <- c(note, "")
note <- c(note, paste0("Definition: a *pair* is two regression rows that share ",
                       "model, treatment, outcome, regression core, matching ",
                       "core, and tax-band level, and differ only in whether ",
                       "year-of-transaction + price-paid are added as covariates."))
note <- c(note, "")
note <- c(note,
  paste0("Pair classes (alpha = ", SIG_ALPHA, "):  "),
  "- `both_sig_opposite_sign` — both pair members significant, signs disagree (**credible sign flip**)",
  "- `both_sig_same_sign`     — both significant, signs agree (**robust effect**)",
  "- `gain_sig_year`          — year+price *brings* the estimate into significance",
  "- `lose_sig_year`          — year+price *knocks* the estimate out of significance",
  "- `neither_sig`            — both insignificant (fragile near-zero)")
note <- c(note, "")
note <- c(note, sprintf("Source: `%s`", basename(enriched_path)))
note <- c(note, sprintf("Scope:  %s", if (CORE_ONLY)
  paste0("core outcome families (", paste(CORE_OUTCOME_FAMILIES, collapse = ", "), ")") else
  "all outcome families"))
note <- c(note, sprintf("Offshore treatments: %s",
                        paste(OFFSHORE_TREATMENTS, collapse = ", ")))
note <- c(note, "")

note <- c(note, "## Headline counts")
note <- c(note, "")
note <- c(note, sprintf("Total pairs: **%d**.", n_total))
note <- c(note, "")
note <- c(note, "| metric | n | % of pairs |")
note <- c(note, "|---|---:|---:|")
note <- c(note, sprintf("| sign-flipped (any direction) | %d | %.1f%% |", n_flip,             100 * n_flip            / n_total))
note <- c(note, sprintf("| sign-flipped to worse-for-tenants | %d | %.1f%% |", n_flip_worse,       100 * n_flip_worse      / n_total))
note <- c(note, sprintf("| **credible sign-flip** (both sig, opposite sign) | %d | %.1f%% |", n_credflip,         100 * n_credflip        / n_total))
note <- c(note, sprintf("| **credible sign-flip to worse-for-tenants** | %d | %.1f%% |", n_credflip_worse,   100 * n_credflip_worse  / n_total))
note <- c(note, sprintf("| year+price gains significance toward worse | %d | %.1f%% |", n_gain_sig_worse,   100 * n_gain_sig_worse  / n_total))
note <- c(note, sprintf("| year+price gains significance toward better | %d | %.1f%% |", n_gain_sig_better,  100 * n_gain_sig_better / n_total))
note <- c(note, sprintf("| year+price loses significance from worse | %d | %.1f%% |", n_lose_sig_worse,   100 * n_lose_sig_worse  / n_total))
note <- c(note, sprintf("| year+price loses significance from better | %d | %.1f%% |", n_lose_sig_better,  100 * n_lose_sig_better / n_total))
note <- c(note, "")
note <- c(note, "Pair-class distribution:")
note <- c(note, "")
note <- c(note, "| pair_class | n | % |")
note <- c(note, "|---|---:|---:|")
for (i in seq_len(nrow(class_counts))) {
  r <- class_counts[i]
  note <- c(note, sprintf("| %s | %d | %.1f%% |", r$pair_class, r$N,
                          100 * r$N / n_total))
}
note <- c(note, "")

note <- c(note, sprintf("## Top %d (treatment, outcome) cells by credible-flip-to-worse", top_n))
note <- c(note, "")
note <- c(note, "| treatment | outcome | n_pairs | n_cred_flip | n_cred_flip_worse | n_gain_sig_worse | frac_flipped_to_worse |")
note <- c(note, "|---|---|---:|---:|---:|---:|---:|")
for (i in seq_len(top_n)) {
  r <- top[i]
  note <- c(note, sprintf("| %s | %s | %d | %d | %d | %d | %.2f |",
                          r$treatment_short_id, r$outcome, r$n_pairs,
                          r$n_credible_flip, r$n_credible_flip_to_worse,
                          r$n_gain_sig_to_worse, r$frac_flipped_to_worse))
}
note <- c(note, "")
note <- c(note, "## Notes")
note <- c(note, "")
note <- c(note, paste0("Year-of-transaction (`ppd_year_transfer`) and price-paid ",
                       "(`ppd_price_sqm`) are bundled in the existing spec set ",
                       "(see `model_specifications.R`); this script cannot ",
                       "isolate the year-only or price-only contribution. To ",
                       "disentangle, add two new specs to `spec_configs` and ",
                       "rerun `06_run_regressions.R` on this geography only."))
note <- c(note, "")

note_path <- file.path(OUT_DIR, paste0("signflip_note", geo_suffix, ".md"))
writeLines(note, note_path)
message(sprintf("  Wrote markdown note: %s", basename(note_path)))


# Console summary ---------------------------------------------------------

message("\n  --- Headline counts ---")
message(sprintf("    total pairs                       : %4d", n_total))
message(sprintf("    sign-flipped (any)                : %4d (%.1f%%)", n_flip,           100 * n_flip            / n_total))
message(sprintf("    sign-flipped to worse-for-tenants : %4d (%.1f%%)", n_flip_worse,     100 * n_flip_worse      / n_total))
message(sprintf("    CREDIBLE sign-flip (both sig)     : %4d (%.1f%%)", n_credflip,       100 * n_credflip        / n_total))
message(sprintf("    CREDIBLE sign-flip to worse       : %4d (%.1f%%)", n_credflip_worse, 100 * n_credflip_worse  / n_total))
message(sprintf("    gain-sig toward worse             : %4d (%.1f%%)", n_gain_sig_worse,  100 * n_gain_sig_worse  / n_total))
message(sprintf("    gain-sig toward better            : %4d (%.1f%%)", n_gain_sig_better, 100 * n_gain_sig_better / n_total))
message(sprintf("    lose-sig from worse               : %4d (%.1f%%)", n_lose_sig_worse,  100 * n_lose_sig_worse  / n_total))
message(sprintf("    lose-sig from better              : %4d (%.1f%%)", n_lose_sig_better, 100 * n_lose_sig_better / n_total))

message("\n  --- Top 10 (treatment, outcome) cells ---")
for (i in seq_len(min(10L, nrow(top)))) {
  r <- top[i]
  message(sprintf("    %-5s | %-40s | n_cred_flip %2d, n_cred_flip_worse %2d, n_gain_sig_worse %2d, frac_flip_worse %.2f",
                  r$treatment_short_id, r$outcome,
                  r$n_credible_flip, r$n_credible_flip_to_worse,
                  r$n_gain_sig_to_worse, r$frac_flipped_to_worse))
}

message(sprintf("\n  10_vintage_signflip_table.R complete [%s].",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
