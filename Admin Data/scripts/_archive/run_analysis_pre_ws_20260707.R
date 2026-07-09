# Queue Script: run_analysis.R
# Runs script 05 (matching) and/or script 06 (regressions) for all specified
# geographies. Running all matching first ensures that matched-pair files
# are complete before any regression starts.
#
# CRASH RESILIENCE: Each step is executed as a separate Rscript subprocess via
# system2(). If a child process crashes (segfault, OOM kill, etc.), system2()
# returns a non-zero exit code and the parent continues with the next step.
library(here)
library(data.table)

# Initial setup — must run before any path or env var is needed.
source(here::here("scripts", "00_setup.R"))

# Rscript binary — platform-independent, always matches the running R installation.
RSCRIPT_BIN <- file.path(R.home("bin"), "Rscript")


# What to run ---------------------------------------------------------------
# Set to FALSE to skip a phase entirely.
RUN_MATCHING    <- as.logical(Sys.getenv("RUN_MATCHING",    unset = "FALSE"))
RUN_REGRESSIONS <- as.logical(Sys.getenv("RUN_REGRESSIONS", unset = "TRUE"))
RUN_BALANCE     <- as.logical(Sys.getenv("RUN_BALANCE",     unset = "TRUE"))
RUN_ENRICHMENT  <- as.logical(Sys.getenv("RUN_ENRICHMENT",  unset = "TRUE"))
RUN_SYNTHESIS   <- as.logical(Sys.getenv("RUN_SYNTHESIS",   unset = "TRUE"))
RUN_WITHIN_CORPORATE <- as.logical(Sys.getenv("RUN_WITHIN_CORPORATE", unset = "FALSE"))

# Overwrite control ---------------------------------------------------------
# FALSE (default): skip already-completed steps (crash-resume mode).
# TRUE: delete existing outputs before running, forcing a full rerun.
OVERWRITE_MATCHING    <- FALSE
OVERWRITE_REGRESSIONS <- FALSE


# Run configuration -------------------------------------------------------

run_id <- format(Sys.time(), "run_%Y%m%d_%H%M%S")
Sys.setenv(PIPELINE_RUN_ID = run_id)
message(sprintf("Pipeline run ID: %s", run_id))

if (RUN_WITHIN_CORPORATE) Sys.setenv(WITHIN_CORPORATE_OVERRIDE = "TRUE")

on.exit({
  Sys.unsetenv("MATCHING_GEOGRAPHY_OVERRIDE")
  Sys.unsetenv("MATCHING_N_WORKERS_OVERRIDE")
  Sys.unsetenv("PIPELINE_RUN_ID")
  Sys.unsetenv("WITHIN_CORPORATE_OVERRIDE")
}, add = TRUE)

# Geography queue.
geo_levels <- c("LA")

# Parallel worker counts for matching (script 05). Not used by script 06.
workers_per_geo <- c(LA = 4L, ITL3 = 3L, ITL2 = 2L, ITL1 = 1L)

# Pipeline log (written incrementally).
pipeline_log_path <- file.path(SUMMARY_TABLES_DIR,
                               paste0("pipeline_log_", run_id, ".csv"))
fwrite(data.table(
  run_id = character(0), geography = character(0), step = character(0),
  status = character(0), started = character(0), finished = character(0),
  duration_s = numeric(0)
), pipeline_log_path)

message(sprintf("RUN_MATCHING: %s | RUN_REGRESSIONS: %s | RUN_BALANCE: %s | RUN_ENRICHMENT: %s | RUN_SYNTHESIS: %s | WITHIN_CORPORATE: %s",
                RUN_MATCHING, RUN_REGRESSIONS, RUN_BALANCE, RUN_ENRICHMENT, RUN_SYNTHESIS, RUN_WITHIN_CORPORATE))
message(sprintf("OVERWRITE_MATCHING: %s | OVERWRITE_REGRESSIONS: %s",
                OVERWRITE_MATCHING, OVERWRITE_REGRESSIONS))

# Track pass/fail for the final console summary.
run_log <- list()


# Helpers ------------------------------------------------------------------

run_step <- function(label, script_path) {
  message(sprintf("--- %s [subprocess] ---", label))
  exit_code <- tryCatch(
    system2(RSCRIPT_BIN, args = shQuote(normalizePath(script_path)),
            stdout = "", stderr = ""),
    error = function(e) {
      message(sprintf("  ERROR launching subprocess [%s]: %s", label, conditionMessage(e)))
      1L
    }
  )
  if (exit_code == 0L) {
    "OK"
  } else {
    message(sprintf("  FAILED [%s]: subprocess exited with code %d", label, exit_code))
    "FAILED"
  }
}

log_step <- function(run_id, level, step, status, t_start, t_end) {
  fwrite(data.table(
    run_id     = run_id,
    geography  = level,
    step       = step,
    status     = status,
    started    = format(t_start, "%Y-%m-%dT%H:%M:%S"),
    finished   = format(t_end,   "%Y-%m-%dT%H:%M:%S"),
    duration_s = round(as.numeric(difftime(t_end, t_start, units = "secs")), 1L)
  ), pipeline_log_path, append = TRUE)
}

clear_matching_outputs <- function(level) {
  geo_dir <- file.path(MATCHED_DATA_DIR, level)
  if (dir.exists(geo_dir)) {
    n_removed <- length(list.files(geo_dir, recursive = TRUE))
    unlink(geo_dir, recursive = TRUE)
    dir.create(geo_dir, recursive = TRUE, showWarnings = FALSE)
    message(sprintf("  [OVERWRITE] Cleared %d file(s) from %s", n_removed, geo_dir))
  }
  manifest <- file.path(SUMMARY_TABLES_DIR, paste0("run_manifest_05_", level, ".json"))
  if (file.exists(manifest)) { file.remove(manifest); message(sprintf("  [OVERWRITE] Removed %s", basename(manifest))) }
}

clear_regression_outputs <- function(level) {
  wc_sfx <- if (RUN_WITHIN_CORPORATE) "_wc" else ""
  files <- c(
    file.path(SUMMARY_TABLES_DIR, paste0("results_table_",       level, wc_sfx, ".csv")),
    file.path(SUMMARY_TABLES_DIR, paste0("regression_errors_",   level, wc_sfx, ".csv")),
    file.path(SUMMARY_TABLES_DIR, paste0("run_manifest_06_",     level, wc_sfx, ".json")),
    file.path(SUMMARY_TABLES_DIR, paste0("results_enriched_",    level, wc_sfx, ".csv")),
    file.path(SUMMARY_TABLES_DIR, paste0("narrative_summary_",   level, wc_sfx, ".csv"))
  )
  for (f in files) {
    if (file.exists(f)) { file.remove(f); message(sprintf("  [OVERWRITE] Removed %s", basename(f))) }
  }
}


# Phase 1: Matching --------------------------------------------------------

if (RUN_MATCHING) {
  message("\n\n========== PHASE 1: MATCHING (all geographies) ==========\n")

  for (level in geo_levels) {
    message(sprintf("\n>>> Matching [%s] <<<", level))

    if (OVERWRITE_MATCHING) clear_matching_outputs(level)

    Sys.setenv(MATCHING_GEOGRAPHY_OVERRIDE  = level)
    Sys.setenv(MATCHING_N_WORKERS_OVERRIDE  = as.character(workers_per_geo[[level]]))

    t_start <- Sys.time()
    status  <- run_step(
      sprintf("Matching [%s]", level),
      here::here("scripts", "05_create_matched_pairs.R")
    )
    t_end <- Sys.time()

    log_step(run_id, level, "matching", status, t_start, t_end)
    run_log[[paste0(level, ":matching")]] <- status
    gc()
  }
} else {
  message("\n\n========== PHASE 1: MATCHING (SKIPPED) ==========\n")
}


# Phase 2: Regressions ----------------------------------------------------

if (RUN_REGRESSIONS) {
  message("\n\n========== PHASE 2: REGRESSIONS (all geographies) ==========\n")

  for (level in geo_levels) {
    message(sprintf("\n>>> Regressions [%s] <<<", level))

    if (OVERWRITE_REGRESSIONS) clear_regression_outputs(level)

    Sys.setenv(MATCHING_GEOGRAPHY_OVERRIDE = level)

    t_start <- Sys.time()
    status  <- run_step(
      sprintf("Regressions [%s]", level),
      here::here("scripts", "06_run_regressions.R")
    )
    t_end <- Sys.time()

    log_step(run_id, level, "regressions", status, t_start, t_end)
    run_log[[paste0(level, ":regressions")]] <- status
    gc()
  }
} else {
  message("\n\n========== PHASE 2: REGRESSIONS (SKIPPED) ==========\n")
}


# Phase 3: Balance Diagnostics (05b) ----------------------------------------

if (RUN_BALANCE) {
  message("\n\n========== PHASE 3: BALANCE DIAGNOSTICS (all geographies) ==========\n")

  for (level in geo_levels) {
    message(sprintf("\n>>> Balance [%s] <<<", level))
    Sys.setenv(MATCHING_GEOGRAPHY_OVERRIDE = level)

    t_start <- Sys.time()
    status  <- run_step(
      sprintf("Balance [%s]", level),
      here::here("scripts", "05b_compute_balance.R")
    )
    t_end <- Sys.time()

    log_step(run_id, level, "balance", status, t_start, t_end)
    run_log[[paste0(level, ":balance")]] <- status
    gc()
  }
} else {
  message("\n\n========== PHASE 3: BALANCE DIAGNOSTICS (SKIPPED) ==========\n")
}


# Phase 4: Enrichment --------------------------------------------------------

if (RUN_ENRICHMENT) {
  message("\n\n========== PHASE 4: ENRICHMENT (all geographies) ==========\n")

  for (level in geo_levels) {
    message(sprintf("\n>>> Enrichment [%s] <<<", level))
    Sys.setenv(MATCHING_GEOGRAPHY_OVERRIDE = level)

    t_start <- Sys.time()
    status  <- run_step(
      sprintf("Enrichment [%s]", level),
      here::here("scripts", "06b_enrich_results.R")
    )
    t_end <- Sys.time()

    log_step(run_id, level, "enrichment", status, t_start, t_end)
    run_log[[paste0(level, ":enrichment")]] <- status
    gc()
  }
} else {
  message("\n\n========== PHASE 4: ENRICHMENT (SKIPPED) ==========\n")
}


# Phase 5: Geography Recommendation + Synthesis ----------------------------

if (RUN_SYNTHESIS) {
  message("\n\n========== PHASE 5: SYNTHESIS (09a + 09) ==========\n")

  # 09a: Geography Recommendation (geography-agnostic, runs once)
  message("\n>>> Geography Recommendation (09a) <<<")
  t_start <- Sys.time()
  status <- run_step("Geography Recommendation",
                     here::here("scripts", "09a_geography_recommendation.R"))
  t_end <- Sys.time()
  log_step(run_id, "ALL", "geo_recommendation", status, t_start, t_end)
  run_log[["ALL:geo_recommendation"]] <- status
  gc()

  # 09: Synthesis Report (depends on 09a output)
  message("\n>>> Synthesis Report (09) <<<")
  t_start <- Sys.time()
  status <- run_step("Synthesis Report",
                     here::here("scripts", "09_results_synthesis.R"))
  t_end <- Sys.time()
  log_step(run_id, "ALL", "synthesis", status, t_start, t_end)
  run_log[["ALL:synthesis"]] <- status
  gc()
} else {
  message("\n\n========== PHASE 5: SYNTHESIS (SKIPPED) ==========\n")
}


# Final summary -----------------------------------------------------------
message("\n===== RUN SUMMARY =====")
if (!RUN_MATCHING)    message("  (Matching phase was SKIPPED)")
if (!RUN_REGRESSIONS) message("  (Regressions phase was SKIPPED)")
if (!RUN_BALANCE)     message("  (Balance phase was SKIPPED)")
if (!RUN_ENRICHMENT)  message("  (Enrichment phase was SKIPPED)")
if (!RUN_SYNTHESIS)   message("  (Synthesis phase was SKIPPED)")
for (key in names(run_log)) {
  message(sprintf("  %-35s %s", key, run_log[[key]]))
}
n_failed <- sum(unlist(run_log) == "FAILED")
if (length(run_log) == 0L) {
  message("No steps were executed.")
} else if (n_failed == 0L) {
  message("All steps completed successfully.")
} else {
  message(sprintf("%d step(s) FAILED.", n_failed))
}
message(sprintf("Pipeline log: %s", pipeline_log_path))
message("========================")
