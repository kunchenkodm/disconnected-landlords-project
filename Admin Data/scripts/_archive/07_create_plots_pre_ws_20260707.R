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
