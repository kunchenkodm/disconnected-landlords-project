# Master script to run entire analysis pipeline
# Author: Dmytro Kunchenko
# Date: August 15, 2025

rm(list=ls())

message("Starting Disconnected Landlords Analysis Pipeline...")
start_time <- Sys.time()

library(here)

# Source scripts in order
scripts <- c(
  "00_setup.R",
  "01_data_sourcing.R", 
  "02_data_merging_pipeline.R",
  "03_feature_enhancement.R",
  "04_feature_refinement.R",
  "05_create_matched_pairs.R",
  "06_run_regressions.R"
)

# Main Loop
for (script in scripts) {
  script_path <- file.path("scripts", script)
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


total_time <- Sys.time() - start_time
message(sprintf("Pipeline completed in %.2f %s", 
                as.numeric(total_time), 
                attr(total_time, "units")))