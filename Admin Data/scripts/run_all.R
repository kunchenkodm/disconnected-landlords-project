# Main script to run entire analysis pipeline
# Author: Dmytro Kunchenko
# Date: August 15, 2025, Last Updated Last Updated: Febuary 9, 2026.

rm(list=ls())
pipeline.start.time <- Sys.time()
  
message("Starting Disconnected Landlords Analysis Pipeline...")
library(here)

# Source scripts in order
scripts <- c(
  "00_setup.R",
  "01_data_sourcing.R",
  "02_compute_global_stats.R",
  "03_build_analysis_dataset.R",
  "05_create_matched_pairs.R",
  "06_run_regressions.R",
  "07_create_plots.R",
  "07b_plot_covariates.R",
  "08_validation_exercices.R"
)

# Main Loop
for (script in scripts) {
  script_path <- here::here("scripts", script)
  if (file.exists(script_path)) {
    message(sprintf("Running: %s", script))
    tryCatch({
      source(script_path)
      message(sprintf("Completed: %s", script))
    }, error = function(e) {
      stop(sprintf("Error in %s: %s", script, e$message))
    })
  } else {
    warning(sprintf("Script not found: %s", script_path))
  }
}
pipeline.end.time <- Sys.time()
pipeline.time.taken <- pipeline.end.time - pipeline.start.time
message("####====Pipeline completed.====####")

message(sprintf("Total Runtime: %.2f %s.",
                as.numeric(pipeline.time.taken), units(pipeline.time.taken)))
