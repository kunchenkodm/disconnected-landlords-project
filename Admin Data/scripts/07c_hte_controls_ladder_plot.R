# Script: 07c_hte_controls_ladder_plot.R
# Purpose: Forest chart v2 (single panel). X-axis = the headline forest
#          treatments; y-axis = coefficient with 95% CI; three series per
#          treatment:
#            1. Pipeline ATT   - the headline "PSM (Matched) + Subclass FE"
#                                coefficient from results_table_<GEO>.csv,
#            2. Modal-archetype CATE (A1) - the overall most common control-pool
#                                cell's pair-difference CATE (script 06e),
#            3. Reweighted ATT - tau_rw over whitelist cells with FIXED base-core
#                                control-pool weights (script 06e).
#          All three series are shown for ONE control type: the rich price-paid
#          specification ("Council Tax + Price Paid") with the equivalent
#          matching core ("ppd_counciltax"), so the price-paid controls enter
#          both the matched pairs (script 05 matching covariates / sample
#          restriction) and the regression control set.
#          One PDF per outcome (bad_epc_c, current_energy_efficiency).
#
# Inputs:  output/summary_tables/hte_controls_ladder_<GEO>.csv   (script 06e)
#          output/summary_tables/results_table_<GEO>.csv         (script 06)
#          output/summary_tables/hte_archetype_definitions_<GEO>.csv (script 06c)
# Output:  figures/hte_forest_treatments_<outcome>.pdf
#
# Authors: Dmytro Kunchenko
# Date: July 12, 2026.
rm(list = setdiff(ls(), c("script", "pipeline.start.time")))

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "archetype_names.R"))  # make_archetype_names()

library(data.table)
library(ggplot2)

output_dir <- FIGURES_DIR
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Headline control type: rich price-paid controls in BOTH the regression spec
# and the matched pairs (matching core). Overridable via env vars.
HEADLINE_SPEC <- Sys.getenv("FOREST_SPEC", unset = "Council Tax + Price Paid")
HEADLINE_CORE <- Sys.getenv("FOREST_CORE", unset = "ppd_counciltax")
if (!HEADLINE_CORE %in% spec_core_pairs[[HEADLINE_SPEC]]) {
  stop(sprintf("Invalid spec x core combo: '%s' x '%s'", HEADLINE_SPEC, HEADLINE_CORE))
}

ladder_path  <- file.path(SUMMARY_TABLES_DIR, paste0("hte_controls_ladder_", MATCHING_GEOGRAPHY, ".csv"))
results_path <- file.path(SUMMARY_TABLES_DIR, paste0("results_table_", MATCHING_GEOGRAPHY, ".csv"))
defs_path    <- file.path(SUMMARY_TABLES_DIR, paste0("hte_archetype_definitions_", MATCHING_GEOGRAPHY, ".csv"))

for (p in c(ladder_path, results_path)) {
  if (!file.exists(p)) stop("Required input missing: ", p)
}

ladder <- fread(ladder_path, na.strings = c("NA", ""))
res    <- fread(results_path, na.strings = c("NA", ""))

plot_outcomes <- c("bad_epc_c", "current_energy_efficiency")
outcome_labs  <- c(bad_epc_c = "share below EPC C",
                   current_energy_efficiency = "EPC score (SAP points)")
outcome_files <- c(bad_epc_c = "bad_epc_c",
                   current_energy_efficiency = "epc_score")
treat_labs <- c(fp = "For-profit", ukfp = "UK for-profit",
                frfp = "Foreign for-profit", np = "Non-profit",
                th = "Tax haven", ps = "Public sector")

# Modal-archetype pretty name (A1) for the subtitle
modal_desc <- "modal control-pool cell"
if (file.exists(defs_path)) {
  d <- fread(defs_path, na.strings = c("NA", ""))[arch_rank == 1L]
  if (nrow(d) == 1L) {
    modal_desc <- paste0(make_archetype_names(d), " (", d$floor_tercile, " floor area)")
  }
}

# --- Series 1: pipeline ATT from the mainline results table -------------------
pipe <- res[model == "PSM (Matched) + Subclass FE" & status == "ok" &
              spec == HEADLINE_SPEC &
              matching_core == HEADLINE_CORE & regression_core == HEADLINE_CORE &
              treatment_short_id %in% names(treat_labs) &
              outcome %in% plot_outcomes,
            .(treatment_short_id, outcome, estimate = coef, se = se)]
pipe <- unique(pipe, by = c("treatment_short_id", "outcome"))
pipe[, series := "Pipeline ATT (PSM + subclass FE)"]

# --- Series 2 & 3: modal-archetype CATE and reweighted ATT (script 06e) -------
lsub <- ladder[spec == HEADLINE_SPEC & matching_core == HEADLINE_CORE]
if (nrow(lsub) == 0L) {
  stop(sprintf("06e output has no rows for '%s' x '%s' — run 06e first.",
               HEADLINE_SPEC, HEADLINE_CORE))
}

modal <- lsub[!is.na(modal_beta),
              .(treatment_short_id, outcome,
                estimate = modal_beta, se = modal_se)]
modal[, series := "Modal archetype CATE (A1)"]

rw <- lsub[!is.na(tau_rw_full),
           .(treatment_short_id, outcome,
             estimate = tau_rw_full, se = tau_rw_full_se)]
rw[, series := "Reweighted ATT (fixed composition)"]

fdat <- rbindlist(list(pipe, modal, rw), use.names = TRUE)
fdat <- fdat[treatment_short_id %in% names(treat_labs) & outcome %in% plot_outcomes]
if (nrow(fdat) == 0L) stop("No plottable rows — has 06e produced output yet?")

series_levels <- c("Pipeline ATT (PSM + subclass FE)",
                   "Modal archetype CATE (A1)",
                   "Reweighted ATT (fixed composition)")
series_cols   <- setNames(c("#2a78d6", "#1baf7a", "#4a3aa7"), series_levels)
series_shapes <- setNames(c(16, 17, 15), series_levels)

fdat[, series := factor(series, levels = series_levels)]
fdat[, treat_lab := factor(treat_labs[treatment_short_id], levels = treat_labs)]
fdat[, ci_lo := estimate - 1.96 * se]
fdat[, ci_hi := estimate + 1.96 * se]

for (oc in plot_outcomes) {
  pdat <- fdat[outcome == oc]
  if (nrow(pdat) == 0L) { message("Skipping (no rows): ", oc); next }

  p <- ggplot(pdat, aes(x = treat_lab, y = estimate,
                        colour = series, shape = series)) +
    geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.35) +
    geom_linerange(aes(ymin = ci_lo, ymax = ci_hi),
                   position = position_dodge(width = 0.55), linewidth = 0.5) +
    geom_point(position = position_dodge(width = 0.55), size = 1.9) +
    scale_colour_manual(values = series_cols, name = NULL) +
    scale_shape_manual(values = series_shapes, name = NULL) +
    labs(
      title = sprintf("Treatment effects on %s", outcome_labs[oc]),
      subtitle = paste0("Council Tax + Price Paid specification, matched on price-paid controls\n",
                        "Modal archetype A1 = ", modal_desc,
                        "; reweighted ATT holds the base-core control-pool composition fixed"),
      x = NULL,
      y = sprintf("Effect on %s (95%% CI, LA-clustered)", outcome_labs[oc]),
      caption = paste0("Specification: Council Tax + Price Paid; matching core: council tax + price paid ",
                       "(matched pairs formed within the PPD sample on price-paid covariates).\n",
                       "Pipeline ATT from results_table (script 06); archetype CATE and reweighted ATT ",
                       "from pair-difference estimation (script 06e), cell system fixed from 06c.")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#e1e0d9", linewidth = 0.3),
      plot.caption = element_text(colour = "grey40", size = 6.5),
      plot.subtitle = element_text(size = 8.5)
    )

  out_file <- file.path(output_dir,
                        paste0("hte_forest_treatments_", outcome_files[oc], ".pdf"))
  ggsave(out_file, p, width = 8, height = 5, limitsize = FALSE)
  message("Saved: ", out_file)
}
