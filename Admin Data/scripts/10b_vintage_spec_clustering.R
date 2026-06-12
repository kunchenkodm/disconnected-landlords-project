# Script: 10b_vintage_spec_clustering.R
# Purpose: Project 1 (Vintage of Buyer / Offshore Ownership), Task 1.2.
#          Decision trees + hierarchical clustering over the offshore
#          year+price specifications, on core outcomes. Each tree asks which
#          specification choices (especially has_ppd = year + price) separate
#          worse- from better-signed offshore coefficients.
#
#          Tree targets (grown to cp = 0 with K-fold CV, default 10, then
#          pruned at the 1-SE cp):
#            worse_for_tenants  row level; sign(coef)*outcome_worse_sign > 0,
#                               normalised so 1 = offshore-worse across outcomes.
#            sig_dir3           row level, 3-class: sig_better / insignificant /
#                               sig_worse (keeps the insignificant mass distinct).
#            sign_flip          pair level; opposite signs across the year+price
#                               toggle (no significance requirement).
#            ppd_direction      pair level, all treatments; adding year+price
#                               widens the worse-for-tenants direction.
#            ppd_lose_sig       pair level, all treatments; adding year+price
#                               knocks the estimate out of significance.
#            <target>_<family>  per-family worse_for_tenants / sign_flip.
#
#          LEAN analysis script: produces the writeup figures plus a single
#          fitted-model store. All dashboard serialisation lives downstream in
#          10c_generate_dashboard.R, which reads the store and refits NOTHING.
#
# Default scope (PROJECT1_CORE_ONLY != "FALSE"): core outcome families
#   (Energy Efficiency, Energy Use, Realised Energy) PLUS the mainline bad-EPC
#   regulatory-bound indicators bad_epc_c (below C, incoming MEES) and bad_epc_e
#   (below E, present minimum) via CORE_EXTRA_OUTCOMES. The remaining diagnostic
#   outcomes (Efficiency Gap, Bunching, gap/bunching members of Regulatory
#   Compliance) are dropped to focus on substantive rather than precision-
#   fragility variation.
#
# Inputs (output/...):
#   summary_tables/results_enriched_<GEO>.csv          (from 06b)
#   project1_vintage/signflip_pairs_<GEO>[_core].csv   (from script 10)
# Writeup outputs (output/project1_vintage/, suffixed _core in default mode):
#   feature_importance_<target>_<GEO>[_core].csv , ppd_summary_<GEO>[_core].csv
#   figures/{decision_tree,cv_curve,ppd_scatter,spec_cluster_dendrogram,
#            cluster_scatter}_*_<GEO>[_core].pdf
# Model store (handoff to 10c, not a deliverable):
#   vintage_models_<GEO>[_core].rds  (fitted rpart trees + both hclust objects
#                                     + dashboard-only tables)
#
# One-time setup (manual): renv::install(c("rpart.plot", "ggdendro"))

rm(list = setdiff(ls(), c("script", "pipeline.start.time"))); gc()

library(data.table)
library(here)
library(rpart)
library(ggplot2)

source(here::here("scripts", "00_setup.R"))

wc_suffix <- if (WITHIN_CORPORATE) "_wc" else ""

message("===================================================================")
message("  10b_vintage_spec_clustering.R - Project 1, Task 1.2")
message(sprintf("  Started: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

OUT_DIR <- here::here("output", "project1_vintage")
FIG_DIR <- file.path(OUT_DIR, "figures")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

# Offshore-relevant treatments (mirror of 10_vintage_signflip_table.R).
OFFSHORE_TREATMENTS <- c("thfp", "thnp", "frfp", "frnp",
                         "th", "bh", "eh", "ch", "oh")

CORE_OUTCOME_FAMILIES <- c("Energy Efficiency", "Energy Use", "Realised Energy")

# Mainline bad-EPC indicators kept in core scope regardless of family (mirror of
# script 10). Clean binaries with an unambiguous worse-for-tenants direction.
CORE_EXTRA_OUTCOMES <- c("bad_epc_c", "bad_epc_e")

SIG_ALPHA <- as.numeric(Sys.getenv("PROJECT1_SIG_ALPHA", unset = "0.05"))
if (!is.finite(SIG_ALPHA) || SIG_ALPHA <= 0 || SIG_ALPHA >= 1) SIG_ALPHA <- 0.05


spec_features_lookup <- data.table(
  spec            = c("Baseline", "Council Tax", "Price Paid", "Council Tax + Price Paid"),
  has_year        = c(0L, 0L, 1L, 1L),
  has_price       = c(0L, 0L, 1L, 1L),
  has_ppd         = c(0L, 0L, 1L, 1L),  # year + price always toggle together — merged feature
  has_tax_band    = c(0L, 1L, 0L, 1L)
)

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
# families anchoring the offshore hypothesis (PAP H1a). Must match script 10's
# setting so the signflip_pairs file is found.
CORE_ONLY   <- Sys.getenv("PROJECT1_CORE_ONLY", unset = "TRUE") != "FALSE"
core_suffix <- if (CORE_ONLY) "_core" else ""
geo_suffix  <- paste0("_", WINNER_GEO, wc_suffix, core_suffix)

message(sprintf("  Geography:        %s", WINNER_GEO))
message(sprintf("  Core-outcomes:    %s", if (CORE_ONLY)
  paste0("TRUE (", paste(CORE_OUTCOME_FAMILIES, collapse = ", "), ")") else
  "FALSE (all families)"))
message(sprintf("  Significance alpha: %.3f", SIG_ALPHA))

# Model store -------------------------------------------------------------
# This script is the SINGLE place the rpart trees and hclust objects are fit.
# Dashboard serialisation (tree-node JSON, dendrogram JSON, the bundle, the
# self-contained HTML) lives in 10c_generate_dashboard.R, which must NOT refit
# anything. We therefore accumulate every fitted object + the dashboard-only
# tables here and dump them to a suffix-specific .rds at the end; 10c reads that
# store and emits all dashboard artefacts from the exact same objects.
MODELS <- list(
  trees      = list(),    # target_short -> list(tree_full, tree_pruned, cptable)
  dendro     = NULL,      # main spec-grid clustering bundle (or NULL if skipped)
  dendro_ppd = NULL,      # PPD-collapsed clustering bundle  (or NULL if skipped)
  tables     = list(),    # dashboard-only data tables (spec_features, ppd_contrast, ...)
  meta       = list()     # filled in below once K_FOLDS / SEED exist
)


# Inputs ------------------------------------------------------------------

enriched_path <- file.path(SUMMARY_TABLES_DIR,
                           paste0("results_enriched_", WINNER_GEO, wc_suffix, ".csv"))
if (!file.exists(enriched_path)) {
  stop("Enriched results not found: ", enriched_path,
       "\n  Run 06_run_regressions.R + 06b_enrich_results.R for ", WINNER_GEO, " first.")
}
enriched <- fread(enriched_path, na.strings = c("NA", ""))
message(sprintf("  Loaded enriched results: %d rows", nrow(enriched)))

outcomes_path <- file.path(SUMMARY_TABLES_DIR, "outcome_metadata.csv")
outcomes_meta <- if (file.exists(outcomes_path)) {
  fread(outcomes_path, na.strings = c("NA", ""))
} else {
  data.table(outcome = names(outcome_worse_sign),
             outcome_family = NA_character_)
}


# Build feature + target matrix -------------------------------------------

ok <- enriched[status == "ok" & !is.na(coef) &
                 treatment_short_id %in% OFFSHORE_TREATMENTS]
ok <- merge(ok, spec_features_lookup, by = "spec", all.x = TRUE)
ok <- ok[!is.na(has_year)]

if (nrow(ok) == 0L) {
  stop("No usable rows after filtering. Check that offshore treatments ran for ",
       WINNER_GEO, ".")
}

per_band_family_fallback <- c(
  bad_epc_c               = "Regulatory Compliance",
  bad_epc_e               = "Regulatory Compliance",
  energy_efficiency_c_gap = "Efficiency Gap",
  energy_efficiency_e_gap = "Efficiency Gap",
  borderline_good_epc_c   = "Bunching",
  borderline_good_epc_e   = "Bunching"
)

apply_family_fallback <- function(dt) {
  if (!"outcome_family" %in% names(dt)) dt[, outcome_family := "Unknown"]
  dt[is.na(outcome_family) | outcome_family == "",
     outcome_family := "Unknown"]
  dt[outcome_family == "Unknown" &
       outcome %in% names(per_band_family_fallback),
     outcome_family := per_band_family_fallback[outcome]]
  invisible(dt)
}

if ("outcome_family" %in% names(outcomes_meta)) {
  ok <- merge(ok, outcomes_meta[, .(outcome, outcome_family)],
              by = "outcome", all.x = TRUE, sort = FALSE)
}
apply_family_fallback(ok)

# Core-only scope: keep the three energy-performance families plus the mainline
# bad-EPC regulatory-bound indicators (CORE_EXTRA_OUTCOMES) for the row-level /
# sign-flip trees and the main dendrogram.
if (CORE_ONLY) {
  n_before_core <- nrow(ok)
  ok <- ok[outcome_family %in% CORE_OUTCOME_FAMILIES |
             outcome %in% CORE_EXTRA_OUTCOMES]
  message(sprintf("  Core-only filter (row-level): %d -> %d rows (%s; + %s)",
                  n_before_core, nrow(ok),
                  paste(CORE_OUTCOME_FAMILIES, collapse = ", "),
                  paste(CORE_EXTRA_OUTCOMES, collapse = ", ")))
}



# Model family from the model string. Order matters: check more-specific tags
# first ("Subclass FE" implies PSM regardless of caliper).
classify_model <- function(model_str) {
  fcase(
    grepl("OLS Additive",        model_str), "OLS_additive",
    grepl("OLS Interactive",     model_str), "OLS_interactive",
    grepl("Subclass FE",         model_str) & grepl("PS<=0\\.1",  model_str), "PSM_subclass_caliper_01",
    grepl("Subclass FE",         model_str) & grepl("PS<=0\\.2",  model_str), "PSM_subclass_caliper_02",
    grepl("Subclass FE",         model_str), "PSM_subclass",
    grepl("PS<=0\\.1",           model_str), "PSM_caliper_01",
    grepl("PS<=0\\.2",           model_str), "PSM_caliper_02",
    grepl("PSM \\(Matched\\)",   model_str), "PSM_basic",
    default = "Other"
  )
}

# Short, distinct model-family codes for tree SPLIT LABELS only (keeps the long
# names everywhere else: data columns, dendrogram keys, importance CSVs are
# unaffected). Applied to the rpart factor levels just before plotting so the
# wide categorical splits stay readable.
model_family_short <- c(
  OLS_additive = "OLS-A", OLS_interactive = "OLS-I", PSM_basic = "PSM",
  PSM_caliper_01 = "PSMc1", PSM_caliper_02 = "PSMc2", PSM_subclass = "PSMs",
  PSM_subclass_caliper_01 = "PSMs1", PSM_subclass_caliper_02 = "PSMs2",
  Other = "Oth")
abbr_mf <- function(f) {
  f  <- as.factor(f)
  lv <- levels(f)
  levels(f) <- ifelse(lv %in% names(model_family_short),
                      model_family_short[lv], lv)
  f
}

ok[, model_family := classify_model(model)]

# Worse-direction sign target.
ok[, ws := outcome_worse_sign[outcome]]
ok[, sign_pos       := as.integer(coef > 0)]
ok[, sign_pos_worse := as.integer(!is.na(ws) & sign(coef) * ws > 0)]

# Row-level significance flag (feeds the 3-class sig_dir3 target).
ok[, is_sig := as.integer(!is.na(p_value) & p_value < SIG_ALPHA)]
crit_t <- abs(qnorm(SIG_ALPHA / 2))
ok[is.na(p_value) & !is.na(coef) & !is.na(se) & se > 0,
   is_sig := as.integer(abs(coef / se) >= crit_t)]
ok[, sig_pos_worse := as.integer(is_sig == 1L & sign_pos_worse == 1L)]

# Three-class significance/direction target (worse-oriented):
#   "sig_worse"     = significant AND in the worse-for-tenants direction
#   "sig_better"    = significant AND in the better-for-tenants direction
#   "insignificant" = not significant at alpha (sign uninformative)
# Levels ordered better -> insignificant -> worse so a diverging palette reads
# blue -> grey -> red.
ok[, sig_dir3 := fcase(
  is_sig == 1L & sign_pos_worse == 1L, "sig_worse",
  is_sig == 1L & sign_pos_worse == 0L, "sig_better",
  default = "insignificant"
)]

# matching_core sentinel for OLS rows.
ok[, matching_core_key := fifelse(is.na(matching_core) | matching_core == "",
                                  "NA_mc", matching_core)]

feat_cols <- c("has_ppd", "has_tax_band",
               "model_family", "regression_core", "matching_core_key",
               "treatment_short_id", "outcome_family")

feat <- ok[, c(feat_cols,
               "coef", "standardised_coef", "p_value", "is_sig",
               "sign_pos", "sign_pos_worse", "sig_pos_worse", "sig_dir3",
               "outcome", "spec", "model"), with = FALSE]
feat[, geography := WINNER_GEO]

# spec_features is a dashboard-only table: store it for 10c rather than writing.
MODELS$tables$spec_features <- copy(feat)
message(sprintf("  Built feature matrix: %d rows (-> model store)", nrow(feat)))


# Decision trees ----------------------------------------------------------

K_FOLDS <- as.integer(Sys.getenv("PROJECT1_CV_K", unset = "10"))
SEED    <- 42L

build_tree_pair <- function(target_name, formula, data, seed = SEED) {
  set.seed(seed)
  tree_full <- rpart(formula, data = data, method = "class",
                     control = rpart.control(cp        = 0,
                                             minsplit  = 20L,
                                             minbucket = 10L,
                                             maxdepth  = 30L,
                                             xval      = K_FOLDS))

  cptab <- as.data.table(tree_full$cptable)
  setnames(cptab, c("CP", "nsplit", "rel_error", "xerror", "xstd"))
  cptab[, target := target_name]

  if (nrow(cptab) == 0L || all(is.na(cptab$xerror))) {
    return(list(tree_full = tree_full, tree_pruned = tree_full,
                best_cp = NA_real_, min_xerror_cp = NA_real_,
                cptable = cptab))
  }

  min_idx       <- which.min(cptab$xerror)
  one_se_thresh <- cptab$xerror[min_idx] + cptab$xstd[min_idx]
  best_cp_idx   <- which(cptab$xerror <= one_se_thresh)[1L]
  best_cp       <- cptab$CP[best_cp_idx]
  min_xerr_cp   <- cptab$CP[min_idx]

  tree_pruned <- prune(tree_full, cp = best_cp)

  list(tree_full      = tree_full,
       tree_pruned    = tree_pruned,
       best_cp        = best_cp,         # 1-SE rule
       min_xerror_cp  = min_xerr_cp,     # min CV error
       cptable        = cptab)
}

plot_tree2 <- function(model, path_pdf, path_png = NULL,
                       title = "Decision tree", palette = NULL) {
  has_rpartplot <- requireNamespace("rpart.plot", quietly = TRUE)
  is_stump      <- is.null(model$splits) || nrow(model$splits) == 0L
  nclass        <- length(attr(model, "ylevels"))
  if (is.na(nclass) || nclass < 2L) nclass <- 2L
  # Sign-meaningful diverging palette for the binary worse/better and flip
  # targets; an "auto" qualitative palette for the 3-class significance target.
  box_pal <- if (!is.null(palette)) palette else if (nclass > 2L) "auto" else "RdBu"
  draw <- function() {
    if (is_stump) {
      par(mar = c(2, 2, 4, 2))
      plot.new(); title(main = title)
      yval <- model$frame$yval[1]
      n_total <- model$frame$n[1]
      text(0.5, 0.5,
           sprintf("Root-only tree (n=%d, predict class %s)\nCV did not support a single split.",
                   n_total, yval),
           cex = 1.0)
      return(invisible(NULL))
    }
    if (has_rpartplot) {
      # type=5 prints the split VARIABLE NAME inside the interior node box and
      # the split levels/values on the branches below it — the clearest way to
      # see "which variable splits here". Leaves show the predicted class with
      # extra=106 (P(2nd class) + obs%) for binary, 104 (all class probs) for
      # the 3-class target. faclen/varlen=0 keep full names; model_family levels
      # are pre-abbreviated (OLS-A, PSMc1, ...) so branch labels stay compact.
      extra_code <- if (nclass > 2L) 104L else 106L
      rpart.plot::rpart.plot(model, type = 5L, extra = extra_code,
                             box.palette = box_pal, branch = 0.3,
                             under = TRUE, fallen.leaves = TRUE,
                             faclen = 0, varlen = 0,
                             tweak = 1.0, nn = FALSE, shadow.col = 0,
                             cex = NULL, main = title)
    } else {
      par(mar = c(2, 2, 4, 2))
      plot(model, uniform = TRUE, branch = 0.6, margin = 0.12, main = title)
      text(model, all = TRUE, use.n = TRUE, fancy = FALSE,
           pretty = 0, cex = 0.55, xpd = NA)
      legend("bottomleft",
             legend = c("Each node: predicted class (top), n0/n1 (bottom)",
                        "Splits read left-to-right (yes / no)"),
             bty = "n", cex = 0.7, xpd = NA)
    }
  }
  # A4-landscape aspect (~1.5) so the figure fills an A4 landscape page in the
  # writeup with no distortion.
  pdf(path_pdf, width = 13.6, height = 9); draw(); dev.off()
  if (!is.null(path_png)) {
    png(path_png, width = 2040, height = 1350, res = 130); draw(); dev.off()
  }
}

plot_cv_curve <- function(model, path_pdf, title = "CV error vs cp") {
  pdf(path_pdf, width = 8, height = 6)
  par(mar = c(4.5, 4.5, 3, 1))
  plotcp(model)
  title(main = title, line = 2)
  dev.off()
}

# NOTE: write_tree_json() (tree-node JSON serialisation) now lives in
# 10c_generate_dashboard.R — this analysis script no longer emits any JSON.

# emit_tree_artifacts() has been split so this analysis script stays lean:
#   - emit_tree_writeup(): the figures + CSVs the LaTeX writeup cites (PDF trees,
#     CV curve, feature-importance CSV). PNGs are dropped (the dashboard renders
#     trees from JSON, not raster, and nothing else consumes them).
#   - register_tree(): stashes the fitted rpart objects + cptable into MODELS so
#     10c_generate_dashboard.R can serialise the tree-node JSON + cptable CSV
#     from the identical objects (byte-identical to the previous inline output).
emit_tree_writeup <- function(result, target_short, geo_suffix) {
  prefix_fig <- file.path(FIG_DIR, paste0("decision_tree_", target_short, "_"))

  imp_full <- if (!is.null(result$tree_full$variable.importance)) {
    data.table(variable   = names(result$tree_full$variable.importance),
               importance = as.numeric(result$tree_full$variable.importance))
  } else {
    data.table(variable = character(0), importance = numeric(0))
  }
  setorder(imp_full, -importance)
  fwrite(imp_full,
         file.path(OUT_DIR, paste0("feature_importance_", target_short,
                                   geo_suffix, ".csv")))

  plot_tree2(result$tree_full,
             paste0(prefix_fig, "full",   geo_suffix, ".pdf"),
             path_png = NULL,
             title = sprintf("%s — full tree — %s",
                             target_short, WINNER_GEO))
  plot_tree2(result$tree_pruned,
             paste0(prefix_fig, "pruned", geo_suffix, ".pdf"),
             path_png = NULL,
             title = sprintf("%s — pruned (%d-fold CV, 1-SE rule) — %s",
                             target_short, K_FOLDS, WINNER_GEO))
  plot_cv_curve(result$tree_full,
                file.path(FIG_DIR, paste0("cv_curve_", target_short,
                                          geo_suffix, ".pdf")),
                title = sprintf("%s — %d-fold CV error — %s",
                                target_short, K_FOLDS, WINNER_GEO))
}

# Stash a fitted tree into the model store for downstream dashboard emission.
register_tree <- function(result, target_short) {
  MODELS$trees[[target_short]] <<- list(
    tree_full   = result$tree_full,
    tree_pruned = result$tree_pruned,
    cptable     = result$cptable
  )
}

# Convenience: do both (writeup figures + store) in the order the call sites used.
emit_tree_artifacts <- function(result, target_short, geo_suffix) {
  emit_tree_writeup(result, target_short, geo_suffix)
  register_tree(result, target_short)
}

# Row-level trees: worse-for-tenants + significance direction -------------
# Each enriched-results row is one observation. The target is normalised to a
# common "worse-for-tenants" direction (sign(coef) x outcome_worse_sign > 0),
# so that 1 = offshore-worse regardless of which way the raw coefficient
# points for that outcome. This lets has_ppd (year + price) emerge as the
# separator between worse and better offshore coefficients, which is exactly
# what Task 1.2 asks the tree to show.
#
# worse_for_tenants: target = sign_pos_worse (binary, worse-direction).
# sig_dir3:          target = 3-class significance direction (see below).

feat_tree <- copy(feat)
for (col in c("model_family", "regression_core", "matching_core_key",
              "treatment_short_id", "outcome_family")) {
  feat_tree[[col]] <- factor(feat_tree[[col]])
}
feat_tree[, model_family := abbr_mf(model_family)]   # short codes for split labels
# Factor binary indicators so rpart emits categorical splits with readable labels.
feat_tree[, has_ppd      := factor(has_ppd,      levels = c(0L, 1L))]
feat_tree[, has_tax_band := factor(has_tax_band, levels = c(0L, 1L))]
feat_tree[, sign_pos        := factor(sign_pos,       levels = c(0L, 1L))]
feat_tree[, sign_pos_worse := factor(sign_pos_worse, levels = c(0L, 1L))]
feat_tree[, sig_pos_worse  := factor(sig_pos_worse,  levels = c(0L, 1L))]
feat_tree[, sig_dir3       := factor(sig_dir3,
                                     levels = c("sig_better", "insignificant", "sig_worse"))]

# worse_for_tenants (row level)
wft_formula <- as.formula(paste("sign_pos_worse ~",
                                paste(feat_cols, collapse = " + ")))
n_worse <- sum(feat_tree$sign_pos_worse == "1")
message(sprintf("  Growing worse-for-tenants tree (%d-fold CV) on %d rows; worse=%d (%.1f%%)...",
                K_FOLDS, nrow(feat_tree), n_worse,
                100 * n_worse / nrow(feat_tree)))
result_wft <- build_tree_pair("worse_for_tenants", wft_formula, feat_tree)
message(sprintf("    full primary nsplit=%d ; pruned primary nsplit=%d ; best_cp=%.5f (1-SE) min_xerr_cp=%.5f",
                max(result_wft$tree_full$cptable[, "nsplit"]),
                max(result_wft$tree_pruned$cptable[, "nsplit"]),
                result_wft$best_cp, result_wft$min_xerror_cp))
emit_tree_artifacts(result_wft, "worse_for_tenants", geo_suffix)

# significant_worse retired 2026-06-01: not shown in the writeup and
# subsumed by the 3-class sig_dir3 tree below (its "sig_worse" class).

# sig_dir3 — three-class significance/direction (sig_better / insignificant /
# sig_worse). Richer than the binary targets: it keeps the large insignificant
# mass as its own class instead of folding it into "better", so the tree can
# show which choices move estimates between significant-worse, significant-
# better, and noise. Directly visualises the Task-1.1 finding that year+price
# mostly pushes estimates into the insignificant region.
f3_formula <- as.formula(paste("sig_dir3 ~", paste(feat_cols, collapse = " + ")))
f3_tab <- table(feat_tree$sig_dir3)
message(sprintf("  Growing 3-class sig-direction tree (%d-fold CV) on %d rows; %s ...",
                K_FOLDS, nrow(feat_tree),
                paste(sprintf("%s=%d", names(f3_tab), as.integer(f3_tab)), collapse = ", ")))
if (sum(f3_tab > 0L) >= 2L) {
  result_f3 <- build_tree_pair("sig_dir3", f3_formula, feat_tree)
  message(sprintf("    full primary nsplit=%d ; pruned primary nsplit=%d ; best_cp=%.5f",
                  max(result_f3$tree_full$cptable[, "nsplit"]),
                  max(result_f3$tree_pruned$cptable[, "nsplit"]),
                  result_f3$best_cp))
  emit_tree_artifacts(result_f3, "sig_dir3", geo_suffix)
} else {
  message("  [SKIP] sig_dir3 tree: fewer than 2 non-empty classes.")
}


# Pair-level tree: sign-flip on signflip pairs ----------------------------

pairs_path <- file.path(OUT_DIR, paste0("signflip_pairs", geo_suffix, ".csv"))
if (!file.exists(pairs_path)) {
  stop("signflip pairs CSV not found: ", pairs_path,
       "\n  Run 10_vintage_signflip_table.R first (with the matching PROJECT1_CORE_ONLY setting).")
}
pairs <- fread(pairs_path, na.strings = c("NA", ""))
message(sprintf("  Loaded sign-flip pair table: %d rows", nrow(pairs)))

# outcome_family + model_family
pairs[, model_family := classify_model(model)]
if (!"outcome_family" %in% names(pairs)) {
  if ("outcome_family" %in% names(outcomes_meta)) {
    pairs <- merge(pairs, outcomes_meta[, .(outcome, outcome_family)],
                   by = "outcome", all.x = TRUE, sort = FALSE)
  } else {
    pairs[, outcome_family := NA_character_]
  }
}
apply_family_fallback(pairs)

# credible_flip already computed in script 10 if present; else derive.
if (!"credible_flip" %in% names(pairs)) {
  if (all(c("sig_no_year","sig_year") %in% names(pairs))) {
    pairs[, credible_flip := as.integer(sig_no_year == 1L & sig_year == 1L &
                                        sign_flipped == 1L)]
  } else {
    stop("Pair table is missing significance columns; rerun script 10 with the new version.")
  }
}

sf_feat_cols <- c("has_tax_band", "model_family", "regression_core",
                  "matching_core_key", "treatment_short_id", "outcome_family")

build_pair_factor_dt <- function(target_col) {
  dt <- pairs[!is.na(get(target_col)),
              .(target_col_value = factor(get(target_col), levels = c(0L, 1L)),
                has_tax_band       = factor(has_tax_band, levels = c(0L, 1L)),
                model_family       = abbr_mf(model_family),
                regression_core    = factor(regression_core),
                matching_core_key  = factor(matching_core_key),
                treatment_short_id = factor(treatment_short_id),
                outcome_family     = factor(outcome_family))]
  setnames(dt, "target_col_value", target_col)
  dt
}

# sign_flip (pair level): coefs have opposite signs across the toggle
sf_data <- build_pair_factor_dt("sign_flipped")
sf_formula <- as.formula(paste("sign_flipped ~",
                               paste(sf_feat_cols, collapse = " + ")))
message(sprintf("  Growing sign-flip tree (%d-fold CV) on %d pairs...",
                K_FOLDS, nrow(sf_data)))
result_sf <- build_tree_pair("sign_flip", sf_formula, sf_data)
message(sprintf("    full primary nsplit=%d ; pruned primary nsplit=%d ; best_cp=%.5f",
                max(result_sf$tree_full$cptable[, "nsplit"]),
                max(result_sf$tree_pruned$cptable[, "nsplit"]),
                result_sf$best_cp))
emit_tree_artifacts(result_sf, "sign_flip", geo_suffix)

# credible_flip retired 2026-06-01: not estimable in the core scope
# (zero pairs with both members significant AND opposite signs ⇒ always a stump).
# The "zero credible flips" result is reported from the signflip counts, not a tree.


# PPD-centric layer (transaction-info presence) ---------------------------
# Lift the (no-PPD vs with-PPD) toggle to ALL treatments (not just offshore),
# build a contrast table + per-family summary, fit a PPD-only decision tree,
# and (further below) cluster on a PPD-collapsed spec axis.

ppd_ok <- enriched[status == "ok" & !is.na(coef)]
ppd_ok <- merge(ppd_ok, spec_features_lookup, by = "spec", all.x = TRUE)
ppd_ok <- ppd_ok[!is.na(has_ppd)]

if ("outcome_family" %in% names(outcomes_meta) &&
    !"outcome_family" %in% names(ppd_ok)) {
  ppd_ok <- merge(ppd_ok, outcomes_meta[, .(outcome, outcome_family)],
                  by = "outcome", all.x = TRUE, sort = FALSE)
}
apply_family_fallback(ppd_ok)

# Restrict to core outcome families plus the mainline bad-EPC indicators — the
# PPD analysis cannot interpret direction credibly on the other diagnostic
# outcomes (Efficiency Gap, Bunching) because their "worse-for-tenants" sign is
# ambiguous, but bad_epc_c/e have a clean direction so they are retained.
n_before_core <- nrow(ppd_ok)
ppd_ok <- ppd_ok[outcome_family %in% CORE_OUTCOME_FAMILIES |
                   outcome %in% CORE_EXTRA_OUTCOMES]
message(sprintf("  PPD analysis restricted to core outcomes: %d -> %d rows (%s; + %s)",
                n_before_core, nrow(ppd_ok),
                paste(CORE_OUTCOME_FAMILIES, collapse = ", "),
                paste(CORE_EXTRA_OUTCOMES, collapse = ", ")))

ppd_ok[, model_family := classify_model(model)]
ppd_ok[, ws := outcome_worse_sign[outcome]]
ppd_ok[, matching_core_key := fifelse(is.na(matching_core) | matching_core == "",
                                      "NA_mc", matching_core)]
ppd_ok[, is_sig := as.integer(!is.na(p_value) & p_value < SIG_ALPHA)]
ppd_ok[is.na(p_value) & !is.na(coef) & !is.na(se) & se > 0,
       is_sig := as.integer(abs(coef / se) >= crit_t)]
ppd_ok[, sign_pos_worse := as.integer(!is.na(ws) & sign(coef) * ws > 0)]
ppd_ok[, sig_pos_worse  := as.integer(is_sig == 1L & sign_pos_worse == 1L)]

# PPD contrast: with-PPD vs no-PPD pairs (all treatments) -----------------
pair_key <- c("model", "treatment_short_id", "outcome", "regression_core",
              "matching_core_key", "has_tax_band")

no_ppd <- ppd_ok[has_ppd == 0L,
                 c(pair_key,
                   coef_no_ppd = "coef", se_no_ppd = "se",
                   p_no_ppd = "p_value", std_no_ppd = "standardised_coef",
                   sign_pos_worse_no_ppd = "sign_pos_worse",
                   sig_pos_worse_no_ppd  = "sig_pos_worse",
                   is_sig_no_ppd = "is_sig",
                   "outcome_family", "ws", matching_core_no_ppd = "matching_core",
                   spec_no_ppd = "spec"),
                 with = FALSE]
setnames(no_ppd,
         old = c("coef","se","p_value","standardised_coef",
                 "sign_pos_worse","sig_pos_worse","is_sig",
                 "matching_core","spec"),
         new = c("coef_no_ppd","se_no_ppd","p_no_ppd","std_no_ppd",
                 "sign_pos_worse_no_ppd","sig_pos_worse_no_ppd","is_sig_no_ppd",
                 "matching_core_no_ppd","spec_no_ppd"),
         skip_absent = TRUE)

with_ppd <- ppd_ok[has_ppd == 1L,
                   c(pair_key,
                     "coef","se","p_value","standardised_coef",
                     "sign_pos_worse","sig_pos_worse","is_sig","spec"),
                   with = FALSE]
setnames(with_ppd,
         old = c("coef","se","p_value","standardised_coef",
                 "sign_pos_worse","sig_pos_worse","is_sig","spec"),
         new = c("coef_ppd","se_ppd","p_ppd","std_ppd",
                 "sign_pos_worse_ppd","sig_pos_worse_ppd","is_sig_ppd",
                 "spec_ppd"))

ppd_contrast <- merge(no_ppd, with_ppd, by = pair_key, all = FALSE)

ppd_contrast[, sign_flipped := as.integer(
  !is.na(coef_no_ppd) & !is.na(coef_ppd) &
    sign(coef_no_ppd) != sign(coef_ppd) &
    coef_no_ppd != 0 & coef_ppd != 0
)]
ppd_contrast[, credible_flip := as.integer(
  is_sig_no_ppd == 1L & is_sig_ppd == 1L & sign_flipped == 1L
)]
ppd_contrast[, sig_transition := fcase(
  is_sig_no_ppd == 1L & is_sig_ppd == 1L,                          "stable_sig",
  is_sig_no_ppd == 0L & is_sig_ppd == 1L,                          "gain",
  is_sig_no_ppd == 1L & is_sig_ppd == 0L,                          "lose",
  default = "stable_insig"
)]
ppd_contrast[, delta_std := std_ppd - std_no_ppd]
ppd_contrast[, ppd_widens_worse := as.integer(
  !is.na(ws) & !is.na(std_no_ppd) & !is.na(std_ppd) &
    (ws * std_ppd) > (ws * std_no_ppd)
)]
ppd_contrast[, geography := WINNER_GEO]

# Dashboard-only table: store a snapshot HERE (before lose_sig/gain_sig and the
# spec_block helper columns are added below) so 10c writes the same columns the
# bundle previously carried.
MODELS$tables$ppd_contrast <- copy(ppd_contrast)
message(sprintf("  Built PPD contrast table: %d rows (-> model store)",
                nrow(ppd_contrast)))

# PPD summary by (treatment, outcome family) ------------------------------
ppd_summary <- ppd_contrast[, .(
  n_pairs                  = .N,
  n_flipped                = sum(sign_flipped == 1L, na.rm = TRUE),
  n_credible_flip          = sum(credible_flip == 1L, na.rm = TRUE),
  n_credible_flip_to_worse = sum(credible_flip == 1L & sig_pos_worse_ppd == 1L, na.rm = TRUE),
  n_gain_sig               = sum(sig_transition == "gain", na.rm = TRUE),
  n_gain_sig_to_worse      = sum(sig_transition == "gain" & sig_pos_worse_ppd == 1L, na.rm = TRUE),
  n_lose_sig               = sum(sig_transition == "lose", na.rm = TRUE),
  median_delta_std         = median(delta_std, na.rm = TRUE),
  share_ppd_widens_worse   = mean(ppd_widens_worse == 1L, na.rm = TRUE)
), by = .(treatment_short_id, outcome_family)]
setorder(ppd_summary,
         -n_credible_flip_to_worse, -share_ppd_widens_worse,
         treatment_short_id, outcome_family)
ppd_summary[, geography := WINNER_GEO]

ppd_summary_path <- file.path(OUT_DIR,
  paste0("ppd_summary", geo_suffix, ".csv"))
fwrite(ppd_summary, ppd_summary_path)
message(sprintf("  Wrote PPD summary table: %s (%d rows)",
                basename(ppd_summary_path), nrow(ppd_summary)))

# Pair-level PPD trees: widens-worse + loses-significance -----------------
# ppd_direction   target = ppd_widens_worse (81% positive, core)
# ppd_lose_sig    target = sig_transition == "lose"    (24%)
# Both are pair-level (one row per PPD toggle), so has_ppd is not a
# predictor — every observation IS a toggle. Predictors are the conditioning
# categoricals; the tree identifies the (treatment, outcome family, model,
# core) cells in which PPD presence is most consequential.

ppd_contrast[, lose_sig := as.integer(sig_transition == "lose")]
ppd_contrast[, gain_sig := as.integer(sig_transition == "gain")]

feat_pair_base <- ppd_contrast[, .(model_family       = abbr_mf(classify_model(model)),
                                   treatment_short_id = factor(treatment_short_id),
                                   outcome_family     = factor(outcome_family),
                                   has_tax_band       = factor(has_tax_band, levels = c(0L, 1L)),
                                   regression_core    = factor(regression_core),
                                   matching_core_key  = factor(matching_core_key),
                                   ppd_widens_worse,
                                   lose_sig,
                                   gain_sig)]

pair_feat_cols <- c("treatment_short_id", "outcome_family", "model_family",
                    "has_tax_band", "regression_core", "matching_core_key")

# ppd_direction
feat_dir <- copy(feat_pair_base)[!is.na(ppd_widens_worse)]
feat_dir[, ppd_widens_worse := factor(ppd_widens_worse, levels = c(0L, 1L))]
n_pos_dir <- sum(feat_dir$ppd_widens_worse == "1")
message(sprintf("  Growing PPD-direction tree (%d-fold CV) on %d pairs; widens_worse=%d (%.1f%%)...",
                K_FOLDS, nrow(feat_dir), n_pos_dir,
                100 * n_pos_dir / max(1L, nrow(feat_dir))))
if (n_pos_dir > 0L && n_pos_dir < nrow(feat_dir)) {
  fA <- as.formula(paste("ppd_widens_worse ~", paste(pair_feat_cols, collapse = " + ")))
  result_dir <- build_tree_pair("ppd_direction", fA, feat_dir)
  message(sprintf("    full nsplit=%d ; pruned nsplit=%d ; best_cp=%.5f",
                  max(result_dir$tree_full$cptable[, "nsplit"]),
                  max(result_dir$tree_pruned$cptable[, "nsplit"]),
                  result_dir$best_cp))
  emit_tree_artifacts(result_dir, "ppd_direction", geo_suffix)
}

# ppd_lose_sig: significance attrition
feat_lose <- copy(feat_pair_base)[!is.na(lose_sig)]
feat_lose[, lose_sig := factor(lose_sig, levels = c(0L, 1L))]
n_pos_lose <- sum(feat_lose$lose_sig == "1")
message(sprintf("  Growing PPD-lose-sig tree (%d-fold CV) on %d pairs; lose_sig=%d (%.1f%%)...",
                K_FOLDS, nrow(feat_lose), n_pos_lose,
                100 * n_pos_lose / max(1L, nrow(feat_lose))))
if (n_pos_lose > 0L && n_pos_lose < nrow(feat_lose)) {
  fB <- as.formula(paste("lose_sig ~", paste(pair_feat_cols, collapse = " + ")))
  result_lose <- build_tree_pair("ppd_lose_sig", fB, feat_lose)
  message(sprintf("    full nsplit=%d ; pruned nsplit=%d ; best_cp=%.5f",
                  max(result_lose$tree_full$cptable[, "nsplit"]),
                  max(result_lose$tree_pruned$cptable[, "nsplit"]),
                  result_lose$best_cp))
  emit_tree_artifacts(result_lose, "ppd_lose_sig", geo_suffix)
}


# PPD toggle scatterplots -------------------------------------------------
# Generated as standalone figures for the writeup (and mirrored by the
# dashboard's interactive scatter).

# Helper for short outcome family labels in titles
fam_short <- c("Energy Efficiency" = "EE", "Energy Use" = "EU", "Realised Energy" = "RE")

# Sign-coloured palette used throughout
trans_pal <- c(stable_sig   = "#16a34a",   # green
               stable_insig = "#94a3b8",   # grey
               gain         = "#3b82f6",   # blue
               lose         = "#f97316")   # orange

# Scatter 1: coef_no_ppd vs coef_ppd, identity line, per outcome family.
sc1_pdf <- file.path(FIG_DIR, paste0("ppd_scatter_coef", geo_suffix, ".pdf"))
draw_sc1 <- function() {
  fams <- intersect(CORE_OUTCOME_FAMILIES, unique(ppd_contrast$outcome_family))
  op <- par(mfrow = c(1, length(fams)), mar = c(4.2, 4.2, 3, 1), mgp = c(2.3, 0.7, 0))
  on.exit(par(op))
  for (fam in fams) {
    d <- ppd_contrast[outcome_family == fam]
    lim <- range(c(d$std_no_ppd, d$std_ppd), na.rm = TRUE)
    plot(d$std_no_ppd, d$std_ppd, pch = 20, cex = 0.55,
         col = trans_pal[d$sig_transition],
         xlim = lim, ylim = lim, asp = 1,
         xlab = "standardised coef, PPD absent",
         ylab = "standardised coef, PPD present",
         main = sprintf("%s (n=%d pairs)", fam, nrow(d)))
    abline(0, 1, lty = 2, col = "grey50")
    abline(h = 0, v = 0, lty = 3, col = "grey70")
  }
  par(mfrow = c(1, 1), new = TRUE, mar = c(0, 0, 0, 0))
  plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
  legend("bottom", horiz = TRUE,
         legend = names(trans_pal), pch = 20, pt.cex = 1.4,
         col = trans_pal, bty = "n", cex = 0.95)
}
pdf(sc1_pdf, width = 14, height = 5.2); draw_sc1(); dev.off()
message(sprintf("  Wrote PPD scatter (coef): %s", basename(sc1_pdf)))

# Scatter 2: signed-worse delta (ws * delta_std) by treatment, faceted by family.
# Includes the bad-EPC Regulatory Compliance family (bad_epc_c/e) alongside the three
# energy-performance families; an adaptive grid keeps each panel readable (2x2 for four
# families, a single row otherwise).
sc2_pdf <- file.path(FIG_DIR, paste0("ppd_scatter_delta_by_treatment", geo_suffix, ".pdf"))
sc2_fams <- intersect(c(CORE_OUTCOME_FAMILIES, "Regulatory Compliance"),
                      unique(ppd_contrast$outcome_family))
sc2_nr <- if (length(sc2_fams) <= 3L) 1L else 2L
sc2_nc <- ceiling(length(sc2_fams) / sc2_nr)
draw_sc2 <- function() {
  fam_title <- c("Regulatory Compliance" = "Bad EPC (C & E)")
  op <- par(mfrow = c(sc2_nr, sc2_nc), mar = c(6, 4.2, 3, 1), mgp = c(2.3, 0.7, 0))
  on.exit(par(op))
  for (fam in sc2_fams) {
    d <- ppd_contrast[outcome_family == fam & !is.na(ws) & !is.na(delta_std)]
    d[, signed_worse_delta := ws * delta_std]
    d[, treatment_short_id := factor(treatment_short_id,
                                     levels = sort(unique(treatment_short_id)))]
    ttl <- if (fam %in% names(fam_title)) fam_title[[fam]] else fam
    boxplot(signed_worse_delta ~ treatment_short_id, data = d,
            las = 2, cex.axis = 0.8, ylab = expression(omega * Delta * tilde(beta)),
            xlab = "", outline = TRUE, col = "white", border = "grey30",
            main = sprintf("%s (n=%d)", ttl, nrow(d)))
    abline(h = 0, lty = 2, col = "firebrick", lwd = 1.5)
    grid(nx = NA, ny = NULL, lty = 3, col = "grey85")
  }
}
pdf(sc2_pdf, width = sc2_nc * 4.8, height = sc2_nr * 4.6); draw_sc2(); dev.off()
message(sprintf("  Wrote PPD scatter (delta by treatment): %s", basename(sc2_pdf)))

# Scatter 3: signed-worse delta vs starting position, colour = sig transition
sc3_pdf <- file.path(FIG_DIR, paste0("ppd_scatter_delta_vs_start", geo_suffix, ".pdf"))
draw_sc3 <- function() {
  d <- ppd_contrast[!is.na(ws) & !is.na(std_no_ppd) & !is.na(delta_std)]
  d[, signed_worse_start := ws * std_no_ppd]
  d[, signed_worse_delta := ws * delta_std]
  op <- par(mar = c(4.2, 4.2, 3, 1), mgp = c(2.3, 0.7, 0))
  on.exit(par(op))
  plot(d$signed_worse_start, d$signed_worse_delta, pch = 20, cex = 0.55,
       col = trans_pal[d$sig_transition],
       xlab = expression("Starting worse-direction projection " * omega * tilde(beta)^{"no ppd"}),
       ylab = expression("PPD-induced shift " * omega * Delta * tilde(beta)),
       main = sprintf("All %d pairs (core families) - coloured by sig_transition",
                      nrow(d)))
  abline(h = 0, v = 0, lty = 2, col = "grey50")
  legend("topright", legend = names(trans_pal), pch = 20, pt.cex = 1.2,
         col = trans_pal, bty = "n", cex = 0.9)
}
pdf(sc3_pdf, width = 8.5, height = 6); draw_sc3(); dev.off()
message(sprintf("  Wrote PPD scatter (delta vs start): %s", basename(sc3_pdf)))


# Per-family trees (worse-for-tenants + sign-flip) ------------------------
# In core-only mode: 3 families (Energy Efficiency / Energy Use / Realised
# Energy). Family budget is spent on treatment / model / spec / cores.

slugify <- function(x) gsub("[^a-zA-Z0-9]+", "_", tolower(trimws(x)))
fams <- sort(unique(as.character(feat_tree$outcome_family)))
fams <- fams[fams != "Unknown"]

WFT_MIN_ROWS <- 200L
SF_MIN_ROWS  <-  60L

per_family_summary <- list()

for (fam in fams) {
  fam_slug <- slugify(fam)
  message(sprintf("\n  --- Family: %s ---", fam))
  feat_fam <- droplevels(feat_tree[outcome_family == fam])
  fam_feat_cols <- setdiff(feat_cols, "outcome_family")
  wft_fam_formula <- as.formula(paste("sign_pos_worse ~",
                                      paste(fam_feat_cols, collapse = " + ")))

  # Worse-for-tenants tree (direction-normalised, 1 = offshore-worse)
  n_worse_fam <- sum(feat_fam$sign_pos_worse == "1")
  message(sprintf("  Growing WFT-%s tree on %d rows; worse=%d (%.1f%%)...",
                  fam, nrow(feat_fam), n_worse_fam,
                  100 * n_worse_fam / nrow(feat_fam)))
  if (n_worse_fam == 0L || n_worse_fam == nrow(feat_fam)) {
    message(sprintf("    [SKIP] WFT %s: single-class (worse=%d/%d).",
                    fam, n_worse_fam, nrow(feat_fam)))
  } else {
    result_wft_fam <- build_tree_pair(paste0("worse_for_tenants_", fam_slug),
                                      wft_fam_formula, feat_fam)
    wft_full   <- max(result_wft_fam$tree_full$cptable[, "nsplit"])
    wft_pruned <- max(result_wft_fam$tree_pruned$cptable[, "nsplit"])
    message(sprintf("    full primary nsplit=%d ; pruned primary nsplit=%d ; best_cp=%.5f",
                    wft_full, wft_pruned, result_wft_fam$best_cp))
    emit_tree_artifacts(result_wft_fam,
                        paste0("worse_for_tenants_", fam_slug), geo_suffix)
    per_family_summary[[length(per_family_summary) + 1L]] <- data.table(
      target = "worse_for_tenants", family = fam, n = nrow(feat_fam),
      full_nsplit = wft_full, pruned_nsplit = wft_pruned,
      best_cp = result_wft_fam$best_cp, min_xerror_cp = result_wft_fam$min_xerror_cp
    )
  }

  # Sign-flip subset for this family
  sf_fam <- droplevels(sf_data[outcome_family == fam])
  if (nrow(sf_fam) < SF_MIN_ROWS) {
    message(sprintf("  [SKIP] SF %s: %d pairs < %d threshold",
                    fam, nrow(sf_fam), SF_MIN_ROWS))
  } else {
    sf_fam_feat_cols <- setdiff(sf_feat_cols, "outcome_family")
    sf_fam_formula   <- as.formula(paste("sign_flipped ~",
                                         paste(sf_fam_feat_cols, collapse = " + ")))

    message(sprintf("  Growing SF-%s tree (%d-fold CV) on %d pairs...",
                    fam, K_FOLDS, nrow(sf_fam)))
    result_sf_fam <- build_tree_pair(paste0("sign_flip_", fam_slug),
                                     sf_fam_formula, sf_fam)
    full_n   <- max(result_sf_fam$tree_full$cptable[, "nsplit"])
    pruned_n <- max(result_sf_fam$tree_pruned$cptable[, "nsplit"])
    message(sprintf("    full primary nsplit=%d ; pruned primary nsplit=%d ; best_cp=%.5f",
                    full_n, pruned_n, result_sf_fam$best_cp))
    emit_tree_artifacts(result_sf_fam,
                        paste0("sign_flip_", fam_slug), geo_suffix)
    per_family_summary[[length(per_family_summary) + 1L]] <- data.table(
      target = "sign_flip", family = fam, n = nrow(sf_fam),
      full_nsplit = full_n, pruned_nsplit = pruned_n,
      best_cp = result_sf_fam$best_cp, min_xerror_cp = result_sf_fam$min_xerror_cp
    )
  }
}

if (length(per_family_summary) > 0L) {
  per_family_dt <- rbindlist(per_family_summary, fill = TRUE)
  setorder(per_family_dt, target, family)
  MODELS$tables$per_family_tree_summary <- copy(per_family_dt)  # dashboard-only
  message(sprintf("\n  Built per-family tree summary: %d rows (-> model store)",
                  nrow(per_family_dt)))
}


# Per-outcome coef_sign trees retired 2026-06-01: the raw per-outcome sign tree
# (Tree E) is a diagnostic not shown in the writeup, and the dashboard's tree
# "focus" is served by the per-family worse_for_tenants_/sign_flip_ trees above.
# (per_outcome_tree_summary table retired with it.)


# Hierarchical clustering on standardised coefficients --------------------

# Diverging blue-white-red colour for a signed value; intensity ~ |value|.
diverging_col <- function(x, max_abs) {
  if (!is.finite(max_abs) || max_abs <= 0) max_abs <- 1
  pal <- grDevices::colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(201L)
  idx <- round(pmin(pmax(x / max_abs, -1), 1) * 100) + 101L
  pal[idx]
}

# Short outcome codes for dendrogram leaf labels (display only; the underlying
# cluster_membership / dendrogram JSON keep the full "treatment | outcome").
outcome_short <- c(
  current_energy_efficiency           = "EPC score",
  energy_consumption_current          = "energy/m2",
  energy_consumption_current_property = "energy tot",
  el_mean_consumption_k_wh            = "elec kWh",
  gas_mean_consumption_k_wh           = "gas kWh",
  bad_epc_c                           = "below C",
  bad_epc_e                           = "below E")
shorten_leaf <- function(x) {
  vapply(strsplit(x, " \\| "), function(p) {
    if (length(p) == 2L && p[2] %in% names(outcome_short))
      paste(p[1], outcome_short[[p[2]]], sep = " | ") else paste(p, collapse = " | ")
  }, character(1L))
}

# Dendrogram + leaf labels + aligned median-coefficient lollipop, in three
# stacked panels that share the SAME horizontal coordinate range and the same
# left/right margins, so the leaf labels line up exactly with both the
# dendrogram's terminal nodes (above) and the lollipop stems (below).
#   (1) dendrogram with cluster rectangles + a median-coef colour strip;
#   (2) leaf labels, written vertically BETWEEN the two charts;
#   (3) lollipop of each leaf's median standardised coefficient (hue = sign,
#       intensity = magnitude; stems coloured by cluster).
# The old cluster-id square band is dropped (redundant with the rectangles).
draw_dendro_panel <- function(hc, cluster_id, cluster_pal, row_med, K,
                              main_title, sub_title = "") {
  ord     <- hc$order
  labs    <- hc$labels
  lab_txt <- shorten_leaf(labs[ord])
  med_ord <- row_med[labs[ord]]
  max_abs <- max(abs(row_med), na.rm = TRUE)
  cl_ord  <- cluster_id[labs[ord]]
  n       <- length(ord)
  LM <- 4.5; RM <- 1.5      # identical L/R margins across panels => aligned x
  # Text sized for legibility once the figure is scaled onto an A4 landscape
  # page (the canvas is drawn at display size, so cex ~ point size).
  cex_lab <- if (n <= 50L) 0.85 else max(0.55, 42 / n)

  op <- par(no.readonly = TRUE); on.exit(par(op))
  # The middle (label) band is sized to the labels, not left oversized, so there
  # is no dead space between the dendrogram and the lollipop.
  layout(matrix(c(1L, 2L, 3L), nrow = 3L), heights = c(2.7, 1.0, 1.6))

  # (1) dendrogram (leaf labels suppressed; shown in panel 2 instead)
  par(mar = c(0.2, LM, 3.6, RM), cex.main = 1.05, cex.lab = 1.05, cex.axis = 0.95)
  plot(hc, hang = -1, labels = FALSE, main = main_title,
       xlab = "", sub = "", ylab = "Ward distance")
  if (nzchar(sub_title)) mtext(sub_title, side = 3, line = 0.2, cex = 0.82)
  rect.hclust(hc, k = K, border = cluster_pal)
  du <- par("usr")[1:2]     # exact x-range used for the leaves (1..n + padding)
  points(seq_len(n), rep(par("usr")[3], n), pch = 15, cex = 1.4, xpd = TRUE,
         col = diverging_col(med_ord, max_abs))

  # (2) leaf labels, vertical, packed directly under the dendrogram and over the
  #     lollipop; same x-range so each label lines up with both.
  par(mar = c(0, LM, 0, RM))
  plot.new(); plot.window(xlim = du, ylim = c(0, 1), xaxs = "i")
  text(seq_len(n), rep(1, n), labels = lab_txt, srt = 90,
       adj = c(1, 0.5), cex = cex_lab, xpd = TRUE, col = cluster_pal[cl_ord])

  # (3) lollipop of median std coef, same x-range, stems coloured by cluster
  par(mar = c(0.4, LM, 0.3, RM), cex.lab = 1.05, cex.axis = 0.95)
  plot(seq_len(n), med_ord, type = "n", xaxt = "n", xaxs = "i",
       xlim = du, ylim = c(-max_abs, max_abs) * 1.08,
       xlab = "", ylab = "median std. coef", las = 1)
  abline(h = 0, col = "grey55")
  segments(seq_len(n), 0, seq_len(n), med_ord, col = cluster_pal[cl_ord], lwd = 2.4)
  points(seq_len(n), med_ord, pch = 21, cex = 1.3, col = "grey25",
         bg = diverging_col(med_ord, max_abs))
}

# 2-D MDS scatter of the clustered (treatment x outcome) cells: classical MDS of
# the same distance matrix, points coloured by cluster (ring) and median coef
# (fill), with a convex hull per cluster. Shows cluster separation directly.
draw_cluster_scatter <- function(d, cluster_id, cluster_pal, row_med,
                                  main_title, path_pdf) {
  mds <- stats::cmdscale(d, k = 2)
  if (is.null(rownames(mds))) rownames(mds) <- attr(d, "Labels")
  rn      <- rownames(mds)
  ci      <- cluster_id[rn]
  max_abs <- max(abs(row_med), na.rm = TRUE)
  ks      <- sort(unique(ci))
  pdf(path_pdf, width = 9, height = 7)
  par(mar = c(4.5, 4.5, 4, 1), cex.lab = 1.1, cex.axis = 1.0, cex.main = 1.15)
  plot(mds[, 1], mds[, 2], type = "n",
       xlab = "MDS dimension 1", ylab = "MDS dimension 2", main = main_title)
  abline(h = 0, v = 0, col = "grey85", lty = 3)
  for (k in ks) {
    pts <- mds[ci == k, , drop = FALSE]
    if (nrow(pts) >= 3L) {
      h <- grDevices::chull(pts)
      polygon(pts[c(h, h[1]), , drop = FALSE], border = cluster_pal[k],
              col = adjustcolor(cluster_pal[k], 0.10), lwd = 1.8)
    }
  }
  points(mds[, 1], mds[, 2], pch = 21, cex = 2.0, lwd = 2.2,
         bg = diverging_col(row_med[rn], max_abs), col = cluster_pal[ci])
  legend("topright", title = "Ward cluster",
         legend = paste("Cluster", ks), pch = 21, pt.cex = 1.6, pt.lwd = 2.2,
         col = cluster_pal[ks], pt.bg = "white", bty = "n")
  legend("bottomright", title = "point fill = median std coef",
         legend = c("worse (>0)", "~0", "better (<0)"), pch = 21, pt.cex = 1.4,
         pt.bg = c("#b2182b", "#f7f7f7", "#2166ac"), col = "grey40", bty = "n")
  dev.off()
}

# Cluster ALL treatments (not just offshore) so this dendrogram and the
# PPD-collapsed one below share the same leaf set and differ ONLY in the
# feature axis (full spec grid here vs has_ppd x model_family there).
ppd_ok[, spec_block := paste0(
  "y", has_year, "p", has_price, "tb", has_tax_band, "_",
  model_family, "_rc_", regression_core
)]

mat_long <- ppd_ok[, .(med_std = median(standardised_coef, na.rm = TRUE)),
               by = .(treatment_short_id, outcome, spec_block)]
mat_wide <- dcast(mat_long, treatment_short_id + outcome ~ spec_block,
                  value.var = "med_std")

row_labels <- mat_wide[, paste(treatment_short_id, outcome, sep = " | ")]
mat <- as.matrix(mat_wide[, -c("treatment_short_id", "outcome"), with = FALSE])
rownames(mat) <- row_labels

col_keep <- vapply(seq_len(ncol(mat)),
                   function(j) any(!is.na(mat[, j])),
                   logical(1L))
mat <- mat[, col_keep, drop = FALSE]
for (j in seq_len(ncol(mat))) {
  na_mask <- is.na(mat[, j])
  if (any(na_mask)) {
    mat[na_mask, j] <- median(mat[, j], na.rm = TRUE)
  }
}
keep_rows <- rowSums(is.na(mat)) == 0L
mat <- mat[keep_rows, , drop = FALSE]

if (nrow(mat) >= 2L && ncol(mat) >= 1L) {
  d  <- dist(mat, method = "euclidean")
  hc <- hclust(d, method = "ward.D2")

  row_med  <- apply(mat, 1, median, na.rm = TRUE)
  row_sign <- ifelse(row_med > 0, "positive",
                     ifelse(row_med < 0, "negative", "zero"))

  K_CLUSTERS  <- min(4L, max(2L, nrow(mat) %/% 5L))
  cluster_id  <- cutree(hc, k = K_CLUSTERS)
  cluster_pal <- c("#377EB8", "#4DAF4A", "#E41A1C", "#984EA3",
                   "#FF7F00", "#A65628", "#F781BF", "#999999")[seq_len(K_CLUSTERS)]

  cluster_summary <- data.table(
    leaf_label   = names(cluster_id),
    cluster_id   = unname(cluster_id),
    median_coef  = unname(row_med[names(cluster_id)])
  )
  cluster_summary[, c("treatment_short_id", "outcome") :=
                    tstrsplit(leaf_label, " \\| ", fixed = FALSE)]
  setcolorder(cluster_summary,
              c("cluster_id", "treatment_short_id", "outcome",
                "median_coef", "leaf_label"))
  setorder(cluster_summary, cluster_id, treatment_short_id, outcome)

  cluster_meta <- cluster_summary[, .(
    n           = .N,
    med_overall = median(median_coef, na.rm = TRUE),
    n_pos       = sum(median_coef > 0, na.rm = TRUE),
    n_neg       = sum(median_coef < 0, na.rm = TRUE)
  ), by = cluster_id]
  setorder(cluster_meta, cluster_id)

  # cluster_membership CSV + dendrogram JSON are dashboard artefacts: stash the
  # fitted hclust object and its derived tables for 10c to serialise.
  MODELS$dendro <- list(
    hc              = hc,
    k_clusters      = K_CLUSTERS,
    cluster_pal     = cluster_pal,
    row_med         = row_med,
    row_sign        = row_sign,
    cluster_id      = cluster_id,
    cluster_summary = copy(cluster_summary),
    cluster_meta    = copy(cluster_meta)
  )
  message(sprintf("  Built spec-grid clustering: %d leaves, k=%d (-> model store)",
                  nrow(cluster_summary), K_CLUSTERS))

  dend_pdf <- file.path(FIG_DIR,
                        paste0("spec_cluster_dendrogram", geo_suffix, ".pdf"))
  pdf(dend_pdf, width = 10.1, height = 6.69)   # A4 landscape text area (scale ~1)
  draw_dendro_panel(
    hc, cluster_id, cluster_pal, row_med, K_CLUSTERS,
    main_title = sprintf("(treatment x outcome) clustered by std coef across the full spec grid - %s (k=%d Ward clusters)",
                         WINNER_GEO, K_CLUSTERS),
    sub_title  = "Strip & lollipop colour = median std coef (blue<0 better, red>0 worse); lollipop stems coloured by cluster")
  dev.off()
  message(sprintf("  Wrote cluster dendrogram: %s", basename(dend_pdf)))

  scat_pdf <- file.path(FIG_DIR, paste0("cluster_scatter", geo_suffix, ".pdf"))
  draw_cluster_scatter(
    d, cluster_id, cluster_pal, row_med,
    sprintf("(treatment x outcome) cluster MDS, full spec grid - %s (k=%d Ward clusters)",
            WINNER_GEO, K_CLUSTERS),
    scat_pdf)
  message(sprintf("  Wrote cluster scatter: %s", basename(scat_pdf)))
} else {
  message("  Skipping dendrogram: fewer than 2 (treatment, outcome) rows after pivoting.")
}


# PPD-collapsed dendrogram ------------------------------------------------
# Columns reduced to has_ppd x model_family — visual axis is exactly the PPD
# toggle, holding tax-band/regression-core averaged within cell.

ppd_ok[, spec_block_ppd := paste0("ppd", has_ppd, "_", model_family)]
mat_long_p <- ppd_ok[, .(med_std = median(standardised_coef, na.rm = TRUE)),
                     by = .(treatment_short_id, outcome, spec_block_ppd)]
mat_wide_p <- dcast(mat_long_p, treatment_short_id + outcome ~ spec_block_ppd,
                    value.var = "med_std")

row_labels_p <- mat_wide_p[, paste(treatment_short_id, outcome, sep = " | ")]
mat_p <- as.matrix(mat_wide_p[, -c("treatment_short_id", "outcome"), with = FALSE])
rownames(mat_p) <- row_labels_p

col_keep_p <- vapply(seq_len(ncol(mat_p)),
                     function(j) any(!is.na(mat_p[, j])), logical(1L))
mat_p <- mat_p[, col_keep_p, drop = FALSE]
for (j in seq_len(ncol(mat_p))) {
  na_mask <- is.na(mat_p[, j])
  if (any(na_mask)) mat_p[na_mask, j] <- median(mat_p[, j], na.rm = TRUE)
}
keep_rows_p <- rowSums(is.na(mat_p)) == 0L
mat_p <- mat_p[keep_rows_p, , drop = FALSE]

if (nrow(mat_p) >= 2L && ncol(mat_p) >= 1L) {
  d_p  <- dist(mat_p, method = "euclidean")
  hc_p <- hclust(d_p, method = "ward.D2")

  row_med_p  <- apply(mat_p, 1, median, na.rm = TRUE)
  row_sign_p <- ifelse(row_med_p > 0, "positive",
                       ifelse(row_med_p < 0, "negative", "zero"))

  K_CLUSTERS_P <- min(4L, max(2L, nrow(mat_p) %/% 5L))
  cluster_id_p <- cutree(hc_p, k = K_CLUSTERS_P)
  cluster_pal_p <- c("#377EB8", "#4DAF4A", "#E41A1C", "#984EA3",
                     "#FF7F00", "#A65628", "#F781BF", "#999999")[seq_len(K_CLUSTERS_P)]

  cluster_summary_p <- data.table(
    leaf_label   = names(cluster_id_p),
    cluster_id   = unname(cluster_id_p),
    median_coef  = unname(row_med_p[names(cluster_id_p)])
  )
  cluster_summary_p[, c("treatment_short_id", "outcome") :=
                      tstrsplit(leaf_label, " \\| ", fixed = FALSE)]
  setcolorder(cluster_summary_p,
              c("cluster_id", "treatment_short_id", "outcome",
                "median_coef", "leaf_label"))
  setorder(cluster_summary_p, cluster_id, treatment_short_id, outcome)

  cluster_meta_p <- cluster_summary_p[, .(
    n           = .N,
    med_overall = median(median_coef, na.rm = TRUE),
    n_pos       = sum(median_coef > 0, na.rm = TRUE),
    n_neg       = sum(median_coef < 0, na.rm = TRUE)
  ), by = cluster_id]
  setorder(cluster_meta_p, cluster_id)

  # PPD-collapsed dendrogram JSON is a dashboard artefact: stash for 10c.
  MODELS$dendro_ppd <- list(
    hc              = hc_p,
    k_clusters      = K_CLUSTERS_P,
    cluster_pal     = cluster_pal_p,
    row_med         = row_med_p,
    row_sign        = row_sign_p,
    cluster_id      = cluster_id_p,
    cluster_summary = copy(cluster_summary_p),
    cluster_meta    = copy(cluster_meta_p),
    spec_axis       = "has_ppd x model_family"
  )
  message(sprintf("  Built PPD-collapsed clustering: %d leaves, k=%d (-> model store)",
                  nrow(cluster_summary_p), K_CLUSTERS_P))

  dend_ppd_pdf <- file.path(FIG_DIR,
    paste0("spec_cluster_dendrogram_ppd", geo_suffix, ".pdf"))
  pdf(dend_ppd_pdf, width = 10.1, height = 6.69)   # A4 landscape text area (scale ~1)
  draw_dendro_panel(
    hc_p, cluster_id_p, cluster_pal_p, row_med_p, K_CLUSTERS_P,
    main_title = sprintf("(treatment x outcome) clustered on PPD-collapsed spec blocks (has_ppd x model_family) - %s (k=%d)",
                         WINNER_GEO, K_CLUSTERS_P),
    sub_title  = "Strip & lollipop colour = median std coef (blue<0 better, red>0 worse); lollipop stems coloured by cluster")
  dev.off()
  message(sprintf("  Wrote PPD cluster dendrogram: %s", basename(dend_ppd_pdf)))

  scat_ppd_pdf <- file.path(FIG_DIR, paste0("cluster_scatter_ppd", geo_suffix, ".pdf"))
  draw_cluster_scatter(
    d_p, cluster_id_p, cluster_pal_p, row_med_p,
    sprintf("PPD-collapsed (treatment x outcome) cluster MDS - %s (k=%d)",
            WINNER_GEO, K_CLUSTERS_P),
    scat_ppd_pdf)
  message(sprintf("  Wrote PPD cluster scatter: %s", basename(scat_ppd_pdf)))
} else {
  message("  Skipping PPD dendrogram: fewer than 2 (treatment, outcome) rows.")
}


# Model store -------------------------------------------------------------
# Single handoff to 10c_generate_dashboard.R. Holds every fitted rpart tree and
# both hclust objects so the dashboard tree-node / dendrogram JSON is serialised
# from the identical objects (byte-identical to the previous inline output), plus
# the dashboard-only tables. Compressed; it is an intermediate, not a deliverable.
MODELS$meta <- list(
  geo              = WINNER_GEO,
  within_corporate = WITHIN_CORPORATE,
  core_only        = CORE_ONLY,
  geo_suffix       = geo_suffix,
  k_folds          = K_FOLDS,
  seed             = SEED,
  sig_alpha        = SIG_ALPHA,
  generated        = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)
models_path <- file.path(OUT_DIR, paste0("vintage_models", geo_suffix, ".rds"))
saveRDS(MODELS, models_path, compress = TRUE)
message(sprintf("\n  Wrote model store: %s (%d trees; dendro=%s, dendro_ppd=%s)",
                basename(models_path), length(MODELS$trees),
                if (is.null(MODELS$dendro)) "no" else "yes",
                if (is.null(MODELS$dendro_ppd)) "no" else "yes"))


# Console summary ---------------------------------------------------------

print_top_imp <- function(target_short, label) {
  imp_path <- file.path(OUT_DIR, paste0("feature_importance_", target_short,
                                        geo_suffix, ".csv"))
  if (!file.exists(imp_path)) return(invisible(NULL))
  imp <- fread(imp_path)
  if (nrow(imp) == 0L) {
    message(sprintf("  %s: no variable importance (root-only full tree)", label))
    return(invisible(NULL))
  }
  message(sprintf("  --- %s: top 5 by full-tree variable importance ---", label))
  top <- imp[seq_len(min(5L, nrow(imp)))]
  for (i in seq_len(nrow(top))) {
    message(sprintf("    %-25s %.2f", top$variable[i], top$importance[i]))
  }
}
message("")
print_top_imp("worse_for_tenants",  "worse-for-tenants (row level)")
print_top_imp("sig_dir3",           "3-class sig direction (better/insig/worse)")
print_top_imp("sign_flip",          "sign-flip (pair-level)")
print_top_imp("ppd_direction",      "PPD widens worse (pair-level)")
print_top_imp("ppd_lose_sig",       "PPD loses significance (pair-level)")

message(sprintf("\n  10b_vintage_spec_clustering.R complete [%s].",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
