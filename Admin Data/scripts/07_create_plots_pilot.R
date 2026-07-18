# Script: 07_create_plots_pilot.R
# Purpose: Pilot exhibits for the estimability-gate fix (see
#          06c_heterogeneous_effects_pilot.R). Two figures:
#   Fig P1 - Support vs weight. One point per treatment's MODAL cell:
#            x = control-pool share w_c (log; the reweighting weight),
#            y = modal-cell matched pairs (log; the estimability support),
#            colour = status (ok/suspect/fail), size = distinct matched LAs.
#            The 50-pair support floor is a horizontal line; the top-100
#            control-pool cutoff is a vertical line on x. Shows the whitelist
#            gated the WRONG axis: offshore havens sit at low weight / high
#            support (estimable), oh sits below the floor (fail).
#   Fig P2 - Contribution decomposition for frfp (focal) and np (contrast):
#            raw ATT -> tau_rw build-up, calling out the A1 and offshore-flat
#            w_c*beta_c contributions and the composition gap.
#
# Sources: hte_pilot_estimability_LA.csv, hte_pilot_summary_LA.csv,
#          hte_pilot_cells_LA.csv (script 06c_..._pilot). Style mirrors
#          07_create_plots.R (dl palette, theme_minimal). Standalone.
# Authors: Dmytro Kunchenko
# Date: July 10, 2026.

library(data.table)
library(ggplot2)
library(here)
source(here::here("scripts", "00_setup.R"))

output_dir <- FIGURES_DIR
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# dl palette (mirrors the progress-pack / 07 house style)
dlnavy   <- "#123456"
dlaccent <- "#B02A37"
col_ok      <- "#1b9e77"   # green  = estimable (ok)
col_suspect <- "#d95f02"   # orange = suspect (few clusters)
col_fail    <- "#7570b3"   # purple = fail (below floor)
status_cols <- c(ok = col_ok, suspect = col_suspect, fail = col_fail)

est_path  <- file.path(SUMMARY_TABLES_DIR, "hte_pilot_estimability_LA.csv")
summ_path <- file.path(SUMMARY_TABLES_DIR, "hte_pilot_summary_LA.csv")
cells_path <- file.path(SUMMARY_TABLES_DIR, "hte_pilot_cells_LA.csv")
defs_path <- file.path(SUMMARY_TABLES_DIR, "hte_archetype_definitions_LA.csv")

stopifnot(file.exists(est_path), file.exists(summ_path))
est  <- fread(est_path,  na.strings = c("NA", ""))
summ <- fread(summ_path, na.strings = c("NA", ""))
summ_cee <- summ[outcome == "current_energy_efficiency"]

# ---- Fig P1: support vs weight (all 14 modal cells) ----
# w_c for each treatment's modal cell is carried on the estimability sweep row.
p1 <- copy(est)
setnames(p1, "modal_w", "w_modal")
p1[, status := factor(status, levels = c("ok", "suspect", "fail"))]
# treatments that failed to build pairs have w_modal NA; drop from the scatter
# (they carry no matched support to plot) but note them in the caption.
p1_plot <- p1[!is.na(w_modal) & modal_cell_pairs > 0L]

# top-100 control-pool cutoff on x = share of the rank-100 cell.
top100_cut <- NA_real_
if (file.exists(defs_path)) {
  d <- fread(defs_path, na.strings = c("NA", ""))
  if (all(c("rank", "share_control_pool") %in% names(d))) {
    top100_cut <- d[rank == max(rank[rank <= 100L], na.rm = TRUE), share_control_pool][1L]
  }
}

# short labels for points
short_lab <- c(fp="fp", ukfp="ukfp", frfp="frfp", thfp="thfp", np="np",
               uknp="uknp", frnp="frnp", thnp="thnp", ps="ps", th="th",
               bh="bh", eh="eh", ch="ch", oh="oh")
p1_plot[, lab := short_lab[treatment_short_id]]

# Point-label layer: use ggrepel if available, else nudged geom_text.
have_repel <- requireNamespace("ggrepel", quietly = TRUE)
label_layer <- if (have_repel) {
  ggrepel::geom_text_repel(aes(label = lab, colour = status), size = 3,
                           show.legend = FALSE, max.overlaps = 20, seed = 1)
} else {
  geom_text(aes(label = lab, colour = status), size = 3, show.legend = FALSE,
            vjust = -0.9, hjust = 0.5)
}

figP1 <- ggplot(p1_plot, aes(x = w_modal, y = modal_cell_pairs)) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = dlaccent, linewidth = 0.5) +
  annotate("text", x = min(p1_plot$w_modal, na.rm = TRUE), y = 50,
           label = "50-pair support floor", hjust = 0, vjust = -0.5,
           size = 2.8, colour = dlaccent) +
  {if (!is.na(top100_cut)) geom_vline(xintercept = top100_cut, linetype = "dotted",
                                      colour = dlnavy, linewidth = 0.5)} +
  {if (!is.na(top100_cut)) annotate("text", x = top100_cut, y = max(p1_plot$modal_cell_pairs),
           label = "top-100\ncontrol-pool cutoff", hjust = -0.05, vjust = 1,
           size = 2.8, colour = dlnavy)} +
  geom_point(aes(colour = status, size = modal_cell_las), alpha = 0.85) +
  label_layer +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_manual(values = status_cols, name = "Estimability status",
                      drop = FALSE) +
  scale_size_continuous(name = "Matched LAs", range = c(1.5, 6)) +
  labs(
    title = "Matched support versus reweighting weight, by treatment modal cell",
    subtitle = "Each point is a treatment's modal covariate cell, coloured by estimability status.",
    x = expression("Control-pool share " * w[c] * " (log scale) — the reweighting weight"),
    y = "Modal-cell matched pairs (log scale) — the estimability support",
    caption = "Support floor: n_pairs ≥ 50 AND matched LAs ≥ 20. Offshore havens (frfp/thfp/th/bh/eh) cluster at low weight but high support; oh falls below the floor."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", colour = dlnavy))

figP1_file <- file.path(output_dir, "hte_pilot_support_vs_weight.pdf")
ggsave(figP1_file, figP1, width = 9.5, height = 6.0)
message("Saved Fig P1 to: ", figP1_file)

# ---- Fig P2: contribution decomposition (frfp focal, np contrast) ----
foc <- summ_cee[treatment_short_id %in% c("frfp", "np")]
treat_lab <- c(frfp = "Foreign for-profit (frfp)", np = "Non-profit (np)")

build_decomp <- function(r) {
  tau   <- r$tau_rw_support
  a1    <- r$contrib_A1
  modal <- r$contrib_modal
  other <- tau - sum(c(a1, modal), na.rm = TRUE)
  data.table(
    treatment_short_id = r$treatment_short_id,
    component = c("Raw ATT", "A1 contribution (w*beta)",
                  "Offshore-flat contribution (w*beta)", "Other cells (w*beta)",
                  "Reweighted tau_rw", "Composition (ATT - tau_rw)"),
    value = c(r$att, a1, modal, other, tau, r$att - tau),
    kind = c("att", "contrib", "contrib", "contrib", "tau", "composition"))
}
decomp <- rbindlist(lapply(seq_len(nrow(foc)), function(i) build_decomp(foc[i])))
decomp[, treat_lab := treat_lab[treatment_short_id]]
comp_levels <- c("Raw ATT", "Composition (ATT - tau_rw)", "Reweighted tau_rw",
                 "Other cells (w*beta)", "Offshore-flat contribution (w*beta)",
                 "A1 contribution (w*beta)")
decomp[, component := factor(component, levels = rev(comp_levels))]
kind_cols <- c(att = dlnavy, tau = col_ok, composition = dlaccent, contrib = "#7BA0C4")

figP2 <- ggplot(decomp, aes(x = value, y = component, fill = kind)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = ifelse(abs(value) < 0.05, sprintf("%+.3f", value),
                               sprintf("%+.2f", value))),
            hjust = ifelse(decomp$value >= 0, -0.15, 1.15), size = 3) +
  facet_wrap(~ treat_lab, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = kind_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.2))) +
  labs(
    title = "Decomposition of the raw ATT into reweighted effect and composition",
    subtitle = "Raw ATT = reweighted tau_rw (sum of w_c x beta_c over estimated cells) + composition gap",
    x = "Effect on EPC score (SAP points)", y = NULL,
    caption = "Contributions use coverage-renormalised weights and sum to tau_rw. The offshore flat is a small-weight, high-baseline cell: reweighting to common stock shrinks the raw premium."
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", colour = dlnavy),
        plot.title = element_text(face = "bold", colour = dlnavy))

figP2_file <- file.path(output_dir, "hte_pilot_contribution_decomp.pdf")
ggsave(figP2_file, figP2, width = 9.0, height = 6.5)
message("Saved Fig P2 to: ", figP2_file)

message("\n07_create_plots_pilot.R complete.")
