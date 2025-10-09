# Script: 07_create_plots.R 
# Purpose: Create diagnostic and visualisation plots
# Authors: Dmytro Kunchenko
# Date: October 8, 2025. Last Updated: October 8, 2025.
rm(list=ls())

# DIAGNOSTICS: RUNTIME
start.time <- Sys.time()

# Source global setup script for paths and configurations
source(here::here("scripts", "00_setup.R"))

### Requirements ###
library(data.table)
library(fixest)
library(ggplot2)

#### SETUP: INPUTS REQUIRED ####
ccod_version <- CCOD_VERSION
processed_data_dir <- PROCESSED_DATA_DIR
matched_data_dir <- MATCHED_DATA_DIR
summary_dir <- file.path(getwd(), "output", "summary_tables")
output_dir <- file.path(getwd(), "output", "figures")

if (!dir.exists(summary_dir)) {
  dir.create(summary_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Helper function to expand matched data
# -----------------------------------------------------------------------------
expand_matched_data <- function(matched_list, full_data) {
  lapply(matched_list, function(matched_dt) {
    if (!is.null(matched_dt) && nrow(matched_dt) > 0) {
      full_data[matched_dt[!is.na(uprn)], on = 'uprn', nomatch = 0]
    } else { NULL }
  })
}

# -----------------------------------------------------------------------------
# Load and prepare data
# -----------------------------------------------------------------------------
input_file <- file.path(processed_data_dir, paste0("epc_matched_refined_", CCOD_VERSION, ".RData"))
load(input_file)
source(here::here("scripts", "treatment_definitions.R"))
EPC_matched_combined <- define_treatments(EPC_matched_combined)
EPC_matched_combined[, energy_cons_curr_per_floor_area := ifelse(total_floor_area == 0, NA_real_, energy_consumption_current / total_floor_area)]


# -----------------------------------------------------------------------------
# Kernel Density Plot
# -----------------------------------------------------------------------------
epc_cutoff <- 69
density_data <- EPC_matched_combined[current_energy_efficiency <= epc_cutoff]
# Or adjust for your selected variable and cutoff direction
# Example density plot of energy efficiency
# Compute mean and SD for Gaussian

mu <- mean(EPC_matched_combined$current_energy_efficiency, na.rm = TRUE)
sigma <- sd(EPC_matched_combined$current_energy_efficiency, na.rm = TRUE)


bands <- data.frame(
  cut = c(1, 21, 39, 55, 69, 81, 92),
  lab = c(
    "Band G: 1–20",
    "Band F: 21–38",
    "Band E: 39–54",
    "Band D: 55–68",
    "Band C: 69–80",
    "Band B: 81–91",
    "Band A: 92+"
  ),
  x = c(11, 30, 47, 62, 75, 86, 96)
)

density_plot <- ggplot(EPC_matched_combined, aes(current_energy_efficiency)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1,
    fill = "skyblue",
    color = "black",
    boundary = 0,
    closed = "right"
  ) +
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

density_plot
density_output_file <- file.path(output_dir, "energy_efficiency_density_plot.png")
ggsave(filename = density_output_file, plot = density_plot, width = 15, height = 10, dpi = 300)


