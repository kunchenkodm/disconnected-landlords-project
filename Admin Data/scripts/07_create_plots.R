# Script: 07_create_plots.R
# Purpose: Create diagnostic and visualisation plots.
#          Streams per-LA Parquet files to accumulate histogram data
#          without loading the full dataset into memory.
# Authors: Dmytro Kunchenko
# Date: October 8, 2025. Last Updated: Febuary 20, 2026.
rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()

# Runtime ----------------------------------------------------------------
start.time <- Sys.time()

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "archetype_names.R"))  # make_archetype_names()

### Requirements ###
library(data.table)
library(ggplot2)
library(arrow)

# Inputs -----------------------------------------------------------------
la_refined_dir <- EPC_LA_REFINED_DIR
output_dir <- FIGURES_DIR

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# Stream per-LA parquets to collect histogram data -------------------------
la_files <- list.files(la_refined_dir, pattern = "\\.parquet$", full.names = TRUE)

if (length(la_files) == 0L) {
  stop("No per-LA Parquet files found in: ", la_refined_dir)
}

message("Streaming per-LA Parquet files for density plot...")

# Accumulators: histogram bins (0-110), sufficient stats for Gaussian overlay
bins <- rep(0L, 111L)  # bins indexed 1..111 represent values 0..110
global_n <- 0L
global_sum <- 0
global_sum_sq <- 0

for (f in la_files) {
  dt <- as.data.table(arrow::read_parquet(f, col_select = "current_energy_efficiency"))
  vals <- dt$current_energy_efficiency[!is.na(dt$current_energy_efficiency)]

  global_n <- global_n + length(vals)
  global_sum <- global_sum + sum(vals)
  global_sum_sq <- global_sum_sq + sum(vals^2)

  # Accumulate histogram counts (integer bins, binwidth = 1)
  idx <- pmax(1L, pmin(111L, as.integer(floor(vals)) + 1L))
  tab <- tabulate(idx, nbins = 111L)
  bins <- bins + tab

  rm(dt, vals)
  gc()
}

# Compute Gaussian parameters
mu <- global_sum / global_n
sigma <- sqrt((global_sum_sq - global_n * mu^2) / (global_n - 1))
message(sprintf("Histogram data: n=%d, mean=%.2f, sd=%.2f", global_n, mu, sigma))


# Build density plot from accumulated histogram ----------------------------
hist_dt <- data.table(
  x = 0:110,
  count = bins,
  density = bins / (global_n * 1)  # binwidth = 1
)

epc_cutoff <- 69

bands <- data.frame(
  cut = c(1, 21, 39, 55, 69, 81, 92),
  lab = c(
    "Band G: 1-20",
    "Band F: 21-38",
    "Band E: 39-54",
    "Band D: 55-68",
    "Band C: 69-80",
    "Band B: 81-91",
    "Band A: 92+"
  ),
  x = c(11, 30, 47, 62, 75, 86, 96)
)

density_plot <- ggplot(hist_dt, aes(x = x, y = density)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 1) +
  geom_function(
    fun = function(x) dnorm(x, mean = mu, sd = sigma),
    color = "darkgreen",
    linewidth = 1,
    n = 500
  ) +
  geom_vline(
    data = bands,
    aes(xintercept = cut),
    color = "red",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_text(
    data = bands,
    aes(x = x, y = Inf, label = lab),
    vjust = 1.2,
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  coord_cartesian(xlim = c(0, 110), clip = "off") +
  scale_x_continuous(
    name = "Energy Efficiency",
    breaks = seq(0, 110, by = 10)
  ) +
  labs(
    title = "Density of Energy Efficiency by EPC band",
    y = "Density"
  ) +
  theme_minimal()

density_output_file <- file.path(output_dir, "energy_efficiency_density_plot.png")
ggsave(filename = density_output_file, plot = density_plot, width = 15, height = 10, dpi = 300)
message("Saved density plot to: ", density_output_file)


# HTE archetype forest plot (PI feedback, workstream 1) ----------------------
# Figure H1: archetype CATEs (10 archetypes x headline treatments) for
# bad_epc_c and current_energy_efficiency, with the pooled ATT reference line.
# Sourced from hte_cells_LA.csv / hte_reweighted_LA.csv (script 06c).
hte_cells_path <- file.path(SUMMARY_TABLES_DIR, "hte_cells_LA.csv")
hte_rw_path    <- file.path(SUMMARY_TABLES_DIR, "hte_reweighted_LA.csv")

if (file.exists(hte_cells_path) && file.exists(hte_rw_path)) {
  message("Building HTE archetype forest plot...")
  hte_cells <- fread(hte_cells_path, na.strings = c("NA", ""))
  hte_rw    <- fread(hte_rw_path, na.strings = c("NA", ""))

  forest_treats <- c("fp", "ukfp", "frfp", "np", "th", "ps")
  forest_outcomes <- c("bad_epc_c", "current_energy_efficiency")
  outcome_labs <- c(bad_epc_c = "Below EPC C (share)",
                    current_energy_efficiency = "EPC score (SAP points)")
  treat_labs <- c(fp = "For-profit", ukfp = "UK for-profit",
                  frfp = "Foreign for-profit", np = "Non-profit",
                  th = "Tax haven", ps = "Public sector")

  fdat <- hte_cells[is_archetype == TRUE & treatment_short_id %in% forest_treats &
                      outcome %in% forest_outcomes & !is.na(beta)]

  # Join the diverse-archetype metadata (arch_rank + fuel/era bucket) from the
  # definitions file so the forest rows order and label by the narrative set.
  hte_defs_path <- file.path(SUMMARY_TABLES_DIR, "hte_archetype_definitions_LA.csv")
  arch_meta <- if (file.exists(hte_defs_path)) {
    d <- fread(hte_defs_path, na.strings = c("NA", ""))
    d <- d[is_archetype == TRUE]
    if (all(c("arch_rank", "fuel_class", "era", "property_type") %in% names(d))) {
      d[, arch_desc := make_archetype_names(d)]
      d[, .(cell_id, arch_rank, arch_desc)]
    } else NULL
  } else NULL

  if (nrow(fdat) > 0L) {
    if (!is.null(arch_meta)) {
      fdat <- merge(fdat, arch_meta, by = "cell_id", all.x = TRUE)
      arch_order <- unique(fdat[order(arch_rank), .(cell_id, arch_rank, arch_desc)])
      arch_order[, arch_lab := paste0("A", arch_rank, ": ", substr(arch_desc, 1, 40))]
    } else {
      # Fallback: rank by control-pool weight, largest first
      arch_order <- unique(fdat[order(-w_control_pool), .(cell_id, w_control_pool)])
      arch_order[, arch_lab := paste0("A", .I, ": ",
                                      substr(gsub(" | ", ", ", cell_id, fixed = TRUE), 1, 48))]
    }
    fdat <- merge(fdat, arch_order[, .(cell_id, arch_lab)], by = "cell_id")
    fdat[, arch_lab := factor(arch_lab, levels = rev(arch_order$arch_lab))]
    fdat[, treat_lab := factor(treat_labs[treatment_short_id], levels = treat_labs[forest_treats])]
    fdat[, outcome_lab := outcome_labs[outcome]]
    fdat[, ci_lo := beta - 1.96 * se]
    fdat[, ci_hi := beta + 1.96 * se]

    att_ref <- hte_rw[treatment_short_id %in% forest_treats & outcome %in% forest_outcomes,
                      .(treatment_short_id, outcome, att)]
    att_ref[, treat_lab := factor(treat_labs[treatment_short_id], levels = treat_labs[forest_treats])]
    att_ref[, outcome_lab := outcome_labs[outcome]]

    hte_forest <- ggplot(fdat, aes(x = beta, y = arch_lab)) +
      geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
      geom_vline(data = att_ref, aes(xintercept = att),
                 colour = "red", linetype = "dashed", linewidth = 0.5) +
      geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25,
                     colour = "steelblue4") +
      geom_point(aes(size = w_control_pool), colour = "steelblue4") +
      scale_size_continuous(name = "Control-pool share", range = c(1, 3.5)) +
      facet_grid(treat_lab ~ outcome_lab, scales = "free_x") +
      labs(
        title = "Archetype-level treatment effects (matched-pair CATEs)",
        subtitle = "Diverse narrative archetypes (property type, fuel/era); dashed red line = pooled ATT",
        x = "Pair-difference CATE (95% CI, LA-clustered)",
        y = NULL
      ) +
      theme_minimal(base_size = 9) +
      theme(strip.text.y = element_text(angle = 0),
            legend.position = "bottom")

    hte_forest_file <- file.path(output_dir, "hte_archetype_forest.pdf")
    ggsave(hte_forest_file, hte_forest,
           width = 11, height = 1.2 + 1.6 * length(unique(fdat$treat_lab)), limitsize = FALSE)
    message("Saved HTE forest plot to: ", hte_forest_file)
  } else {
    message("HTE forest plot skipped: no archetype rows with estimates.")
  }
} else {
  message("HTE forest plot skipped: hte_cells_LA.csv / hte_reweighted_LA.csv not found (run 06c).")
}


# HTE archetype x ownership matrix heatmap (interpretable baseline + effect) ----
# Rows = representative archetypes (short names) + council-stock references;
# columns = ownership types; fill = matched-pair CATE on EPC score (green = more
# efficient). Suspect cells dashed, no-support greyed. Sources hte_archetype_
# matrix_LA.csv + hte_cell_baselines_LA.csv + defs/reference (script 06c).
hte_matrix_path    <- file.path(SUMMARY_TABLES_DIR, "hte_archetype_matrix_LA.csv")
hte_baselines_path <- file.path(SUMMARY_TABLES_DIR, "hte_cell_baselines_LA.csv")
hte_defs_path2     <- file.path(SUMMARY_TABLES_DIR, "hte_archetype_definitions_LA.csv")
hte_ref_path       <- file.path(SUMMARY_TABLES_DIR, "hte_reference_cells_LA.csv")
if (all(file.exists(c(hte_matrix_path, hte_baselines_path, hte_defs_path2)))) {
  message("Building HTE archetype x ownership matrix heatmap...")
  mx <- fread(hte_matrix_path, na.strings = c("NA", ""))
  bl <- fread(hte_baselines_path, na.strings = c("NA", ""))
  df <- fread(hte_defs_path2, na.strings = c("NA", ""))[is_archetype == TRUE]

  m_treats  <- c("fp", "np", "ps", "th", "frfp")
  treat_lab <- c(fp = "For-profit", np = "Non-profit", ps = "Public",
                 th = "Tax haven", frfp = "Foreign FP")

  df[, short := make_archetype_names(df)]
  arch <- merge(df[, .(cell_id, arch_rank, short)],
                bl[, .(cell_id, base = baseline_current_energy_efficiency)], by = "cell_id")
  arch[, rowlab := paste0("A", arch_rank, ": ", short, "  (base ", round(base), ")")]
  arch[, grp := "Representative archetypes"]

  rows <- arch[, .(cell_id, arch_rank, rowlab, grp)]
  if (file.exists(hte_ref_path)) {
    rf <- fread(hte_ref_path, na.strings = c("NA", ""))
    rf <- merge(rf, bl[, .(cell_id, base = baseline_current_energy_efficiency)], by = "cell_id")
    rf[, short := fifelse(ref_type == "council_flat", "Council flat", "Council semi")]
    rf[, rowlab := paste0(short, "  (base ", round(base), ")")]
    rf[, arch_rank := 100L + seq_len(.N)][, grp := "Council reference"]
    rows <- rbind(rows, rf[, .(cell_id, arch_rank, rowlab, grp)])
  }

  hm <- mx[outcome == "current_energy_efficiency" & treatment_short_id %in% m_treats &
             cell_id %in% rows$cell_id, .(cell_id, treatment_short_id, beta, support)]
  hm <- merge(hm, rows, by = "cell_id")
  if (nrow(hm) > 0L) {
    hm[, owner := factor(treat_lab[treatment_short_id], levels = treat_lab[m_treats])]
    hm[, rowlab := factor(rowlab, levels = rev(unique(hm[order(arch_rank)]$rowlab)))]
    hm[, grp := factor(grp, levels = c("Representative archetypes", "Council reference"))]
    hm[, fillbeta := ifelse(support == "none", NA_real_, beta)]
    hm[, txt := ifelse(support == "none", "n/s",
                ifelse(support == "suspect", paste0(sprintf("%+.1f", beta), "†"),
                       sprintf("%+.1f", beta)))]

    hte_matrix_plot <- ggplot(hm, aes(owner, rowlab, fill = fillbeta)) +
      geom_tile(colour = "grey90") +
      geom_tile(data = hm[support == "suspect"], colour = "grey20",
                linewidth = 0.5, linetype = "dashed", fill = NA) +
      geom_text(aes(label = txt), size = 2.7) +
      facet_grid(grp ~ ., scales = "free_y", space = "free_y") +
      scale_fill_gradient2(low = "#8073ac", mid = "grey96", high = "#1b9e77", midpoint = 0,
                           na.value = "grey88", name = "CATE (SAP pts)\n+ = more efficient") +
      labs(title = "Archetype x ownership treatment effects (matched-pair CATEs)",
           subtitle = "Effect on EPC score holding the property fixed; baseline = control-pool mean",
           x = NULL, y = NULL,
           caption = "dagger = suspect (thin/few-cluster); n/s = no support. See Table 1 for archetype covariates.") +
      theme_minimal(base_size = 9) +
      theme(axis.text.y = element_text(size = 7), panel.grid = element_blank(),
            legend.position = "right",
            strip.text.y = element_text(angle = 0, size = 8, face = "bold"))

    hte_matrix_file <- file.path(output_dir, "hte_archetype_matrix_LA.pdf")
    ggsave(hte_matrix_file, hte_matrix_plot, width = 9.2, height = 6.0)
    message("Saved HTE matrix heatmap to: ", hte_matrix_file)
  } else {
    message("HTE matrix heatmap skipped: no matrix rows for the selected treatments.")
  }
} else {
  message("HTE matrix heatmap skipped: matrix / baselines / defs not found (run 06c).")
}
