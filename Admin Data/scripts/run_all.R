#!/usr/bin/env Rscript
# Master script to run entire analysis pipeline
# Author: [Your name]
# Date: [Date]
library(here)


message("Starting Disconnected Landlords Analysis Pipeline...")
start_time <- Sys.time()

# Check if running from correct directory
if (!file.exists("config/config.yml")) {
  stop("Please run this script from the project root directory (where config/config.yml exists)")
}

# Load environment variables
if (file.exists(".env")) {
  readr::read_lines(".env") |>
    stringr::str_subset("^[A-Z]") |>
    stringr::str_split_fixed("=", 2) |>
    apply(1, function(x) Sys.setenv(setNames(x[2], x[1])))
}

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

# Use lapply to correctly scope the 'script' variable in the error handler
invisible(lapply(scripts, function(script) {
  script_path <- here::here("scripts", script) 
  if (file.exists(script_path)) {
    message(sprintf("Running: %s", script))
    tryCatch({
      source(script_path, local = TRUE)
      message(sprintf("Completed: %s", script))
    }, error = function(e) {
      # This will now correctly find 'script' and report the error
      stop(sprintf("Error in %s: %s", script, e$message))
    })
  } else {
    warning(sprintf("Script not found: %s", script_path))
  }
}))

total_time <- Sys.time() - start_time
message(sprintf("Pipeline completed in %.2f %s", 
                as.numeric(total_time), 
                attr(total_time, "units")))