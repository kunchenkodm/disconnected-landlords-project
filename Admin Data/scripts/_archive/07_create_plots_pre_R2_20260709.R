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

  if (nrow(fdat) > 0L) {
    # Short archetype labels: rank by control-pool weight, largest first
    arch_order <- unique(fdat[order(-w_control_pool), .(cell_id, w_control_pool)])
    arch_order[, arch_lab := paste0("A", .I, ": ",
                                    substr(gsub(" | ", ", ", cell_id, fixed = TRUE), 1, 48))]
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
        subtitle = "Ten most common control-pool covariate archetypes; dashed red line = pooled ATT",
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
