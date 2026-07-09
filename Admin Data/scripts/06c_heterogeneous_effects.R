# Script: 06c_heterogeneous_effects.R
# Purpose: Heterogeneous treatment effects via archetypes (PI feedback,
#          workstream 1). Built on the matched-pair design: with 1:1
#          nearest-neighbour matching and exact strata, each subclass is a
#          treated-control pair sharing all exact covariates, so the headline
#          "PSM (Matched) + Subclass FE" model collapses to a pair-difference
#          regression:  D_y ~ 1 + D_rooms + D_floor  (same treatment coefficient,
#          half the rows, no subclass FE). One D_y per pair is formed by selecting
#          the exact-var-consistent lodgement of each member (05 matched at
#          lodgement level but saved only uprn), so this is a CONSISTENCY
#          reference to the headline, not a numerical identity when a matched
#          uprn carries duplicate EPC lodgements.
#
#          Deliverables per (treatment x outcome), Baseline spec x base core:
#            1. Cell CATEs: feols(D_y ~ 0 + i(cell) + D_rooms + D_floor),
#               cells = property_type x built_form x construction_age_band x
#               main_fuel x floor-area tercile; Wald test of equal cell betas.
#            2. Archetypes: the 10 most common cells in the ELIGIBLE CONTROL
#               POOL (treatment-invariant), with their CATEs read off (1).
#            3. Reweighted ATT: tau_rw = sum_c w_c beta_c with w_c = control-
#               pool cell shares (delta-method SE); composition effect =
#               pooled ATT - tau_rw.
#            4. Honest tree (supporting): rpart on a 50% training half,
#               1-SE pruned, node CATEs estimated on the held-out half.
#
# Scope: LA geography, 14 standard treatments, 5 outcomes, no caliper.
# Orchestrated by run_analysis.R (RUN_HTE flag, Phase 4b) or standalone.
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 7, 2026.
rm(list = setdiff(ls(), c("script", "pipeline.start.time")))
gc()

set.seed(20230703)
start.time <- Sys.time()

run_id <- local({
  rid <- Sys.getenv("PIPELINE_RUN_ID", unset = "")
  if (nzchar(rid)) rid else format(start.time, "run_%Y%m%d_%H%M%S")
})

library(data.table)
library(fixest)
library(arrow)
library(dplyr)
library(here)
library(jsonlite)
library(rpart)

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))

setFixest_notes(FALSE)
setFixest_nthreads(2)

if (WITHIN_CORPORATE) {
  stop("06c_heterogeneous_effects.R does not support WITHIN_CORPORATE mode.")
}

# Tuning parameters ---------------------------------------------------------
HTE_MIN_PAIRS   <- 50L    # cells with fewer pairs collapse to "_other"
HTE_MAX_CELLS   <- 100L   # cap: top cells by control-pool frequency
HTE_N_ARCHETYPES <- 10L   # narrative archetype count
FEW_CLUSTERS_THRESHOLD <- 20L
BROKEN_PAIR_WARN_SHARE <- 0.03

# R2 (2026-07-09): the narrative archetype set is chosen by DIVERSITY, not raw
# frequency. The raw top-10 control-pool cells are near-duplicates (period gas
# terraced houses); a diversity-aware greedy selection over the whitelist spans
# coarse buckets (property type x fuel class x construction era) so a narrative
# develops (modern flats, electric/oil fuel, ...). ARCH_SHARE_FLOOR is the
# minimum control-pool share for a cell to be an archetype candidate.
ARCH_SHARE_FLOOR <- 0.0015

# Archetype x ownership CATE matrix (interpretable "baseline + treatment effect"
# exhibit). Estimate each narrative-archetype cell's pair-difference CATE per
# treatment DIRECTLY, down to a low floor, and FLAG rather than drop: a cell is
# "ok" with >= HTE_MIN_PAIRS pairs and >= FEW_CLUSTERS_THRESHOLD LAs, "suspect"
# when thin or few-cluster (still reported), and "none" below MATRIX_MIN_PAIRS
# (no reliable estimate). This recovers an archetype for the treatments that can
# support it instead of restricting the set to universally-estimable cells.
MATRIX_OUTCOMES   <- c("current_energy_efficiency", "bad_epc_c")
MATRIX_MIN_PAIRS  <- 10L

HTE_SPEC_NAME     <- "Baseline"
HTE_MATCHING_CORE <- "base"

hte_outcomes <- c("bad_epc_c", "bad_epc_e", "current_energy_efficiency",
                  "energy_consumption_current", "energy_efficiency_c_gap")
HTE_TREE_OUTCOMES <- c("bad_epc_c", "current_energy_efficiency")

# Optional treatment filter for pilot runs, e.g. HTE_TREATMENTS="ch" (comma-
# separated short IDs). Empty = all 14 treatments.
HTE_TREATMENTS <- local({
  x <- Sys.getenv("HTE_TREATMENTS", unset = "")
  if (nzchar(x)) trimws(strsplit(x, ",")[[1L]]) else character(0)
})

archetype_vars <- c("property_type", "built_form", "construction_age_band", "main_fuel")
base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                        "property_type", "main_fuel", "construction_age_band", "built_form")

# Coarse buckets for the diversity-aware archetype selection (R2). Each maps the
# fine covariate value onto a small category so the greedy picker can span the
# (property type x fuel class x era) grid.
classify_fuel_class <- function(mf) {
  data.table::fcase(
    grepl("gas", mf, ignore.case = TRUE), "gas",
    grepl("electric", mf, ignore.case = TRUE), "electric",
    grepl("oil", mf, ignore.case = TRUE), "oil",
    default = "other")
}
classify_era <- function(ab) {
  # First 4-digit year in the band ("England and Wales: 1983-1990" -> 1983);
  # "... before 1900" has no 4-digit start and is treated as period.
  yr <- suppressWarnings(as.integer(sub(".*?([0-9]{4}).*", "\\1", ab)))
  is_before1900 <- grepl("before 1900", ab, ignore.case = TRUE)
  data.table::fcase(
    is_before1900, "period",
    !is.na(yr) & yr < 1950, "period",
    !is.na(yr) & yr <= 1982, "mid",
    !is.na(yr) & yr >= 1983, "modern",
    default = "other")
}
# property_type {House, Flat, Bungalow, ...} is already coarse.

matched_data_dir <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY)
summary_dir      <- SUMMARY_TABLES_DIR

message("===================================================================")
message("  06c_heterogeneous_effects.R - HTE / Archetypes")
message(sprintf("  Geography: %s | Spec: %s | Core: %s",
                MATCHING_GEOGRAPHY, HTE_SPEC_NAME, HTE_MATCHING_CORE))
message(sprintf("  Started: %s", format(start.time, "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

if (!dir.exists(matched_data_dir)) {
  stop("Matched data directory not found: ", matched_data_dir)
}

# Run manifest ---------------------------------------------------------------
manifest_path <- file.path(summary_dir,
                           paste0("run_manifest_06c_", MATCHING_GEOGRAPHY, ".json"))
if (!file.exists(manifest_path)) {
  jsonlite::write_json(
    list(run_id = run_id, script = "06c_heterogeneous_effects.R",
         matching_geography = MATCHING_GEOGRAPHY, ccod_version = CCOD_VERSION,
         spec = HTE_SPEC_NAME, matching_core = HTE_MATCHING_CORE,
         hte_min_pairs = HTE_MIN_PAIRS, hte_max_cells = HTE_MAX_CELLS,
         n_archetypes = HTE_N_ARCHETYPES,
         r_version = R.version$version.string,
         start_time = format(start.time, "%Y-%m-%dT%H:%M:%S"),
         hostname = Sys.info()[["nodename"]]),
    manifest_path, auto_unbox = TRUE, pretty = TRUE)
  message(sprintf("  Run manifest written: %s", basename(manifest_path)))
}

# Output paths ---------------------------------------------------------------
defs_path  <- file.path(summary_dir, paste0("hte_archetype_definitions_", MATCHING_GEOGRAPHY, ".csv"))
cells_path <- file.path(summary_dir, paste0("hte_cells_", MATCHING_GEOGRAPHY, ".csv"))
rw_path    <- file.path(summary_dir, paste0("hte_reweighted_", MATCHING_GEOGRAPHY, ".csv"))
tree_path  <- file.path(summary_dir, paste0("hte_tree_nodes_", MATCHING_GEOGRAPHY, ".csv"))
err_path   <- file.path(summary_dir, paste0("hte_errors_", MATCHING_GEOGRAPHY, ".csv"))
typical_path <- file.path(summary_dir, paste0("hte_typical_property_", MATCHING_GEOGRAPHY, ".csv"))
baselines_path <- file.path(summary_dir, paste0("hte_cell_baselines_", MATCHING_GEOGRAPHY, ".csv"))
matrix_path <- file.path(summary_dir, paste0("hte_archetype_matrix_", MATCHING_GEOGRAPHY, ".csv"))
reference_path <- file.path(summary_dir, paste0("hte_reference_cells_", MATCHING_GEOGRAPHY, ".csv"))

log_error <- function(treatment, stage, msg) {
  row <- data.table(run_id = run_id, treatment_short_id = treatment,
                    stage = stage, error_message = msg,
                    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  fwrite(row, err_path, append = file.exists(err_path))
  message(sprintf("  ERROR [%s | %s]: %s", treatment, stage, msg))
}


# 1. Load data (column-selected) ---------------------------------------------
message("\n--- Loading data ---")
needed_cols <- unique(c(
  "uprn", "local_authority", "ITL1",
  base_matching_vars,
  "current_energy_efficiency", "energy_consumption_current",
  "source", "tenure_2", "coarse_proprietorship", "country_incorporated_1",
  "country_incorporated_tax_haven", "country_incorporated_british_haven",
  "country_incorporated_european_haven", "country_incorporated_caribbean_haven",
  "country_incorporated_other_haven"
))

all_parquet_files <- list.files(EPC_LA_REFINED_DIR, pattern = "\\.parquet$", full.names = TRUE)
unknown_mask <- grepl("unknown", basename(all_parquet_files), ignore.case = TRUE)
if (any(unknown_mask)) all_parquet_files <- all_parquet_files[!unknown_mask]

ds <- arrow::open_dataset(all_parquet_files, format = "parquet")
available_cols <- intersect(needed_cols, names(ds))
missing_cols   <- setdiff(needed_cols, available_cols)
if (length(missing_cols) > 0L) {
  message(sprintf("  NOTE: %d columns absent from parquet: %s",
                  length(missing_cols), paste(missing_cols, collapse = ", ")))
}

# Stream per-LA and rbindlist. Collecting the whole national dataset at once
# holds arrow's Table AND the R copy simultaneously (~2x peak) and OOMs on a
# tight RAM budget; per-file collect caps the peak near the final table size.
arrow::set_cpu_count(1L)
dat_parts <- vector("list", length(all_parquet_files))
for (i in seq_along(all_parquet_files)) {
  dat_parts[[i]] <- setDT(
    arrow::open_dataset(all_parquet_files[i], format = "parquet") |>
      select(all_of(available_cols)) |> collect())
}
dat <- rbindlist(dat_parts, use.names = TRUE, fill = TRUE)
rm(dat_parts, ds); gc()
message(sprintf("  Loaded %s rows x %d cols",
                formatC(nrow(dat), big.mark = ","), ncol(dat)))

dat <- define_treatments(dat)

# Derive the regulatory-bound outcomes (mirrors 06_run_regressions.R)
cee <- dat[["current_energy_efficiency"]]
dat[, bad_epc_c := fifelse(is.na(cee), NA_real_, as.numeric(cee < 69))]
dat[, bad_epc_e := fifelse(is.na(cee), NA_real_, as.numeric(cee < 39))]
dat[, energy_efficiency_c_gap := cee - 68]
rm(cee)

# Drop treatment-definition inputs
drop_cols <- intersect(c("source", "tenure_2", "coarse_proprietorship",
                         "country_incorporated_1", "country_incorporated_tax_haven",
                         "country_incorporated_british_haven",
                         "country_incorporated_european_haven",
                         "country_incorporated_caribbean_haven",
                         "country_incorporated_other_haven"), names(dat))
if (length(drop_cols) > 0L) dat[, (drop_cols) := NULL]

# Keep `dat` at EPC-lodgement-row level (do NOT dedupe by UPRN). 05 matched at
# lodgement-row level, and the saved matched objects carry only `uprn` (no
# lodgement key), so a uprn-dedupe here would make the pair join retrieve an
# ARBITRARY lodgement's covariates/outcome. Instead the join fans out over all
# lodgement rows and build_pair_diffs() selects the treated x control lodgement
# combination that agrees on the spec's exact-match vars (see there). A
# uprn-deduplicated view is used only where property-level counts are wanted
# (the control pool + tercile cuts + reweighting base).
n_dup <- sum(duplicated(dat$uprn))
message(sprintf("  Retaining %s rows at lodgement level (%s duplicate-UPRN rows kept for the fan-out join).",
                formatC(nrow(dat), big.mark = ","), formatC(n_dup, big.mark = ",")))
setkey(dat, uprn)
gc()


# 2. Archetype cell definitions from the eligible control pool ----------------
# Eligible control pool: complete cases on the base matching covariates with
# the common control condition (identical across the 14 treatments), England-
# wide, base core. This is treatment-invariant, so ONE common cell set serves
# every treatment, and it is the weight base for reweighting.
message("\n--- Building archetype cells from the eligible control pool ---")

# Property-level (uprn-deduplicated): cell frequencies, tercile cuts and the
# reweighting weights are population-of-control-properties quantities, not
# lodgement counts.
ctrl_pool <- unique(dat[!is.na(treat_for_profit) & treat_for_profit == 0L], by = "uprn")
ctrl_pool <- ctrl_pool[complete.cases(ctrl_pool[, ..base_matching_vars])]
message(sprintf("  Eligible control pool: %s properties",
                formatC(nrow(ctrl_pool), big.mark = ",")))

# Floor-area terciles cut on the control pool (persisted in the definitions CSV)
tercile_cuts <- quantile(ctrl_pool$total_floor_area, probs = c(1/3, 2/3),
                         na.rm = TRUE, names = FALSE)
message(sprintf("  Floor-area tercile cuts: %.1f / %.1f sqm",
                tercile_cuts[1], tercile_cuts[2]))

assign_tercile <- function(x) {
  cut(x, breaks = c(-Inf, tercile_cuts[1], tercile_cuts[2], Inf),
      labels = c("small", "medium", "large"), right = TRUE)
}

make_cell_id <- function(d) {
  paste(d$property_type, d$built_form, d$construction_age_band, d$main_fuel,
        as.character(assign_tercile(d$total_floor_area)), sep = " | ")
}

ctrl_pool[, cell_id := make_cell_id(.SD)]
cell_freq <- ctrl_pool[, .(n_control_pool = .N), by = cell_id][order(-n_control_pool)]
cell_freq[, share_control_pool := n_control_pool / sum(n_control_pool)]
cell_freq[, cum_share := cumsum(share_control_pool)]
cell_freq[, rank := .I]
cell_freq[, in_whitelist := rank <= HTE_MAX_CELLS]

# Split covariate components + coarse buckets (needed for the greedy picker)
cell_freq[, c("property_type", "built_form", "construction_age_band",
              "main_fuel", "floor_tercile") := tstrsplit(cell_id, " | ", fixed = TRUE)]
cell_freq[, `:=`(ptype_bucket = property_type,
                 fuel_class = classify_fuel_class(main_fuel),
                 era = classify_era(construction_age_band))]
cell_freq[, arch_bucket := paste(ptype_bucket, fuel_class, era, sep = "/")]

whitelist <- cell_freq[in_whitelist == TRUE, cell_id]
w_lookup  <- setNames(cell_freq$share_control_pool, cell_freq$cell_id)

# --- Diversity-aware greedy archetype selection (R2) --------------------------
# Candidates: whitelist cells above the share floor, ordered by frequency. Seed
# with the overall modal cell, then repeatedly add the highest-frequency
# candidate that fills a (property type x fuel class x era) bucket not yet
# represented; once every reachable bucket is spanned, fill the remaining slots
# with the highest-frequency candidates until HTE_N_ARCHETYPES is reached.
cand <- cell_freq[in_whitelist == TRUE & share_control_pool >= ARCH_SHARE_FLOOR][order(-share_control_pool)]
if (nrow(cand) == 0L) stop("No archetype candidates above ARCH_SHARE_FLOOR — lower the floor.")

sel_ids     <- character(0)
sel_reason  <- character(0)
filled      <- character(0)
n_target    <- min(HTE_N_ARCHETYPES, nrow(cand))

seed <- cand[1L]
sel_ids    <- seed$cell_id
sel_reason <- "modal: overall most common control-pool cell"
filled     <- seed$arch_bucket

while (length(sel_ids) < n_target) {
  remaining <- cand[!(cell_id %in% sel_ids)]
  if (nrow(remaining) == 0L) break
  new_bkt <- remaining[!(arch_bucket %in% filled)]
  if (nrow(new_bkt) > 0L) {
    pick <- new_bkt[1L]  # highest frequency filling a fresh bucket
    reason <- sprintf("diversity: fills bucket %s", pick$arch_bucket)
    filled <- c(filled, pick$arch_bucket)
  } else {
    pick <- remaining[1L]  # buckets spanned; top up by frequency
    reason <- "frequency: high-share top-up (all reachable buckets spanned)"
  }
  sel_ids    <- c(sel_ids, pick$cell_id)
  sel_reason <- c(sel_reason, reason)
}

cell_freq[, is_archetype := cell_id %in% sel_ids]
cell_freq[, arch_rank := match(cell_id, sel_ids)]  # NA for non-archetypes
sel_reason_lookup <- setNames(sel_reason, sel_ids)
cell_freq[, selection_reason := unname(sel_reason_lookup[cell_id])]
archetype_cells <- sel_ids

n_buckets_spanned <- uniqueN(cell_freq[is_archetype == TRUE, arch_bucket])
message(sprintf("  %d distinct cells; whitelist = top %d (%.1f%% of control pool).",
                nrow(cell_freq), length(whitelist),
                cell_freq[in_whitelist == TRUE, sum(share_control_pool)] * 100))
message(sprintf("  Narrative archetypes: %d cells spanning %d buckets (%d property types, %d fuel classes, %d eras); cover %.1f%% of control pool.",
                length(archetype_cells), n_buckets_spanned,
                uniqueN(cell_freq[is_archetype == TRUE, ptype_bucket]),
                uniqueN(cell_freq[is_archetype == TRUE, fuel_class]),
                uniqueN(cell_freq[is_archetype == TRUE, era]),
                cell_freq[is_archetype == TRUE, sum(share_control_pool)] * 100))

# Persist definitions (covariate values + buckets + tercile cuts + flags)
defs <- copy(cell_freq[in_whitelist == TRUE])
defs[, `:=`(floor_tercile_cut1 = tercile_cuts[1],
            floor_tercile_cut2 = tercile_cuts[2],
            n_control_pool_total = nrow(ctrl_pool),
            geo_level = MATCHING_GEOGRAPHY, run_id = run_id)]
setorder(defs, rank)
fwrite(defs, defs_path)
message(sprintf("  Definitions written: %s (%d rows)", basename(defs_path), nrow(defs)))

# --- Reference archetypes: representative social/council stock (post-war gas
# house + flat). These anchor the public / non-profit effect on THEIR typical
# stock, which the private-rental control pool under-represents (no post-war
# flat reaches the top-100 whitelist). Chosen as the modal control-pool cell in
# each social-stock bucket; carried into the baselines and the archetype x
# ownership matrix (flagged where matched support is thin). Note the council
# house is typically also a narrative archetype (A3); the council flat is new.
pick_modal <- function(pt, er, fc) {
  x <- cell_freq[property_type == pt & era == er & fuel_class == fc][order(-share_control_pool)]
  if (nrow(x) == 0L) NA_character_ else x$cell_id[1L]
}
reference_defs <- data.table(
  ref_label = c("Council house (post-war gas semi)", "Council flat (post-war gas)"),
  ref_type  = c("council_house", "council_flat"),
  cell_id   = c(pick_modal("House", "mid", "gas"), pick_modal("Flat", "mid", "gas")))
reference_defs <- reference_defs[!is.na(cell_id)]
reference_cells <- reference_defs$cell_id
ref_label_lookup <- setNames(reference_defs$ref_label, reference_defs$cell_id)
if (nrow(reference_defs) > 0L) {
  refj <- merge(reference_defs,
                cell_freq[, .(cell_id, share_control_pool, rank, property_type,
                              built_form, construction_age_band, main_fuel, floor_tercile)],
                by = "cell_id", all.x = TRUE)
  refj[, `:=`(geo_level = MATCHING_GEOGRAPHY, run_id = run_id)]
  fwrite(refj, reference_path)
  message(sprintf("  Reference (council) archetypes: %s",
                  paste(sprintf("%s = %s", reference_defs$ref_type, reference_defs$cell_id),
                        collapse = " | ")))
}

# --- Per-cell control-pool baseline levels (level anchor for the archetype x
# ownership matrix). Treatment-invariant baseline: what a property like this
# cell scores under the ordinary private-individual control. Computed on the
# eligible control pool (whitelist + reference cells) before it is freed.
baseline_cols <- intersect(hte_outcomes, names(ctrl_pool))
cell_baselines <- ctrl_pool[cell_id %in% unique(c(whitelist, reference_cells)),
  c(list(n_control_pool = .N),
    setNames(lapply(baseline_cols, function(o) mean(get(o), na.rm = TRUE)),
             paste0("baseline_", baseline_cols))),
  by = cell_id]
cell_baselines[, `:=`(geo_level = MATCHING_GEOGRAPHY, run_id = run_id)]
fwrite(cell_baselines, baselines_path)
message(sprintf("  Cell baselines written: %s (%d whitelist cells)",
                basename(baselines_path), nrow(cell_baselines)))

rm(ctrl_pool); gc()

# --- Typical property per ownership type (R2 descriptive) ---------------------
# For each treatment, the modal covariate cell of its TREATED eligible units
# (not the matched-control modal, which is the common control stock). This is
# where the distinctive treated stock shows up (e.g. offshore -> prime flats).
# estimable_matched (cell in whitelist AND >= HTE_MIN_PAIRS matched for that
# treatment) is filled in the main loop; NA for treatments not processed here.
message("\n--- Building typical-property-per-ownership-type table ---")
typical_rows <- list()
for (cfg in treatment_metadata) {
  tv_col <- cfg$var
  if (!tv_col %in% names(dat)) next
  tp <- unique(dat[!is.na(get(tv_col)) & get(tv_col) == 1L], by = "uprn")
  tp <- tp[complete.cases(tp[, ..base_matching_vars])]
  if (nrow(tp) == 0L) next
  tp[, cell_id := make_cell_id(.SD)]
  freq <- tp[, .N, by = cell_id][order(-N)]
  modal_id <- freq$cell_id[1L]
  comp <- as.list(tstrsplit(modal_id, " | ", fixed = TRUE))
  typical_rows[[cfg$short_id]] <- data.table(
    treatment_short_id = cfg$short_id,
    treatment = cfg$title,
    modal_cell_id = modal_id,
    property_type = comp[[1L]], built_form = comp[[2L]],
    construction_age_band = comp[[3L]], main_fuel = comp[[4L]],
    floor_tercile = comp[[5L]],
    n_treated = nrow(tp),
    n_modal = freq$N[1L],
    share_treated = freq$N[1L] / nrow(tp),
    treated_mean_epc = mean(tp$current_energy_efficiency, na.rm = TRUE),
    in_whitelist = modal_id %in% whitelist,
    estimable_matched = NA,
    geo_level = MATCHING_GEOGRAPHY, run_id = run_id)
  rm(tp, freq)
}
typical_dt <- rbindlist(typical_rows, fill = TRUE)
message(sprintf("  Typical-property base built for %d treatments.", nrow(typical_dt)))
rm(typical_rows); gc()


# 3. Crash-resume --------------------------------------------------------------
# A treatment counts as complete when hte_reweighted has all 5 outcome rows.
# Incomplete treatments get their partial rows purged from every output.
treat_shorts <- vapply(treatment_metadata, `[[`, "", "short_id")
done_treats <- character(0)

if (file.exists(rw_path)) {
  rw_existing <- tryCatch(fread(rw_path, na.strings = c("NA", "")), error = function(e) data.table())
  if (nrow(rw_existing) > 0L && "treatment_short_id" %in% names(rw_existing)) {
    cnt <- rw_existing[, .(n_out = uniqueN(outcome)), by = treatment_short_id]
    done_treats <- cnt[n_out >= length(hte_outcomes), treatment_short_id]
    partial <- setdiff(unique(rw_existing$treatment_short_id), done_treats)
    if (length(partial) > 0L) {
      message(sprintf("  Crash-resume: purging partial treatments: %s",
                      paste(partial, collapse = ", ")))
      for (p in list(cells_path, rw_path, tree_path, matrix_path)) {
        if (file.exists(p)) {
          x <- tryCatch(fread(p, na.strings = c("NA", "")), error = function(e) NULL)
          if (!is.null(x) && "treatment_short_id" %in% names(x)) {
            fwrite(x[!(treatment_short_id %in% partial)], p)
          }
        }
      }
    }
    message(sprintf("  Crash-resume: %d treatments already complete.", length(done_treats)))
  }
  rm(rw_existing)
}

# Reference coefficients for the pair-collapse equivalence check
ref_coefs <- NULL
results_path <- file.path(summary_dir, paste0("results_table_", MATCHING_GEOGRAPHY, ".csv"))
if (file.exists(results_path)) {
  res <- tryCatch(fread(results_path, na.strings = c("NA", "")), error = function(e) NULL)
  if (!is.null(res) && nrow(res) > 0L) {
    ref_coefs <- res[model == "PSM (Matched) + Subclass FE" & spec == HTE_SPEC_NAME &
                       matching_core == HTE_MATCHING_CORE & regression_core == HTE_MATCHING_CORE &
                       outcome == "current_energy_efficiency" & status == "ok",
                     .(treatment_short_id, ref_coef = coef)]
    ref_coefs <- unique(ref_coefs, by = "treatment_short_id")
  }
  rm(res)
}


# 4. Helpers -------------------------------------------------------------------

# Build the pair-difference dataset for one treatment (Baseline spec, base core).
# Returns NULL (with logging) when the matched file is missing or unusable.
build_pair_diffs <- function(config) {
  matched_file <- file.path(matched_data_dir,
                            paste0("matched_pairs_", config$file_id,
                                   "_matching_core_", HTE_MATCHING_CORE,
                                   "_", CCOD_VERSION, ".RData"))
  if (!file.exists(matched_file)) {
    log_error(config$short_id, "load", paste0("matched file missing: ", basename(matched_file)))
    return(NULL)
  }
  local_env <- new.env(parent = emptyenv())
  ok <- tryCatch({ load(matched_file, envir = local_env); TRUE },
                 error = function(e) { log_error(config$short_id, "load", conditionMessage(e)); FALSE })
  if (!ok) return(NULL)
  md <- local_env$matched_results[[HTE_SPEC_NAME]]
  rm(local_env)
  if (is.null(md) || nrow(md) == 0L) {
    log_error(config$short_id, "load", "no Baseline matched pairs in RData")
    return(NULL)
  }
  md <- md[!is.na(uprn)]

  # 1:1 matching without replacement => all weights must be 1
  if ("weights" %in% names(md) && any(md$weights != 1)) {
    log_error(config$short_id, "weights",
              sprintf("non-unit weights found (%d rows) — pair collapse invalid",
                      sum(md$weights != 1)))
    return(NULL)
  }
  md <- unique(md[, .(uprn, subclass, distance)], by = c("subclass", "uprn"))

  # Fan-out join: bring ALL lodgement rows for each matched uprn (dat is
  # lodgement-level). Each subclass then holds every candidate lodgement of its
  # treated and control uprns.
  pr <- dat[md, on = "uprn", allow.cartesian = TRUE, nomatch = 0]
  pr[, tv := pr[[config$var]]]
  pr <- pr[!is.na(tv)]
  n_subclass_total <- uniqueN(pr$subclass)

  # Exact-var-consistent selection. Within each subclass pick the treated x
  # control lodgement combination that AGREES on the spec's per-lodgement exact
  # vars (the true matched rows agree by construction of 05's exact matching;
  # this recovers them despite the uprn-only key). Where a uprn contributes
  # several observationally-equivalent lodgements to the same exact stratum the
  # tie is broken by keep-first (noise, not bias, ~1% of matched uprns).
  cons_vars <- setdiff(Filter(function(s) identical(s$name, HTE_SPEC_NAME),
                              spec_configs)[[1L]]$exact_vars, "local_authority")
  pr[, .stratum := do.call(paste, c(.SD, sep = "\r")), .SDcols = cons_vars]
  both <- pr[, .(has_t = any(tv == 1L), has_c = any(tv == 0L)),
             by = .(subclass, .stratum)][has_t & has_c]
  if (nrow(both) == 0L) {
    log_error(config$short_id, "pairs", "no exact-var-consistent pairs after fan-out")
    return(NULL)
  }
  setorder(both, subclass, .stratum)
  chosen <- both[, .(.stratum = .stratum[1L]), by = subclass]
  keep <- pr[chosen, on = .(subclass, .stratum), nomatch = 0]
  p_t <- keep[tv == 1L]; p_t <- p_t[p_t[, .I[1L], by = subclass]$V1]
  p_c <- keep[tv == 0L]; p_c <- p_c[p_c[, .I[1L], by = subclass]$V1]
  good <- intersect(p_t$subclass, p_c$subclass)
  p_t <- p_t[subclass %in% good]
  p_c <- p_c[subclass %in% good]

  # Broken = subclasses with no recoverable consistent pair (a member's uprn was
  # NA in the register linkage, or its matched lodgement is not recoverable).
  # This is the irreducible EPC-CCOD linkage gap, not a matching defect.
  n_broken <- n_subclass_total - length(good)
  broken_share <- if (n_subclass_total > 0L) n_broken / n_subclass_total else NA_real_
  if (!is.na(broken_share) && broken_share > BROKEN_PAIR_WARN_SHARE) {
    message(sprintf("  NOTE [%s]: %.1f%% of subclasses unrecoverable (%d of %d) — EPC-CCOD linkage gap.",
                    config$short_id, broken_share * 100, n_broken, n_subclass_total))
  }
  if (length(good) == 0L) {
    log_error(config$short_id, "pairs", "no intact pairs after consistent selection")
    return(NULL)
  }
  t_cols <- c("subclass", hte_outcomes, "number_habitable_rooms", "total_floor_area")
  p_t <- p_t[, ..t_cols]
  setnames(p_t, setdiff(t_cols, "subclass"), paste0("t_", setdiff(t_cols, "subclass")))
  c_cols <- unique(c("subclass", "local_authority", "ITL1", hte_outcomes,
                     "number_habitable_rooms", "total_floor_area",
                     archetype_vars, "lodgement_year"))
  c_cols <- intersect(c_cols, names(p_c))
  p_c <- p_c[, ..c_cols]

  pd <- merge(p_t, p_c, by = "subclass")
  for (oc in hte_outcomes) {
    pd[, paste0("D_", oc) := get(paste0("t_", oc)) - get(oc)]
  }
  pd[, D_rooms := t_number_habitable_rooms - number_habitable_rooms]
  pd[, D_floor := t_total_floor_area - total_floor_area]

  # Cell assignment from the CONTROL member's covariates (exact-matched vars
  # are shared within pair; the floor tercile uses the control's floor area)
  pd[, cell_id := make_cell_id(.SD)]
  pd[, n_broken_pairs := n_broken]
  pd[, broken_share := broken_share]
  pd
}

# Wald test that all cell coefficients are equal (contrasts vs the first cell)
wald_equal_cells <- function(b, V) {
  K <- length(b)
  if (K < 2L) return(list(chi2 = NA_real_, df = NA_integer_, p = NA_real_))
  R <- cbind(-1, diag(K - 1L))
  Rb <- as.vector(R %*% b)
  RVR <- R %*% V %*% t(R)
  chi2 <- tryCatch(as.numeric(t(Rb) %*% solve(RVR, Rb)), error = function(e) NA_real_)
  list(chi2 = chi2, df = K - 1L,
       p = if (is.na(chi2)) NA_real_ else pchisq(chi2, df = K - 1L, lower.tail = FALSE))
}

# Terminal-node assignment for new data from an rpart fit (yval-swap trick)
rpart_nodes <- function(fit, newdata) {
  fit2 <- fit
  fit2$frame$yval <- as.numeric(rownames(fit2$frame))
  as.integer(predict(fit2, newdata = newdata, type = "vector"))
}


# 5. Main loop: per treatment ---------------------------------------------------
message("\n=== Running HTE estimation per treatment ===")

for (config in treatment_metadata) {
  tsid <- config$short_id
  if (length(HTE_TREATMENTS) > 0L && !(tsid %in% HTE_TREATMENTS)) next
  if (tsid %in% done_treats) {
    message(sprintf("\n[%s] SKIP (already complete)", tsid))
    next
  }
  message(sprintf("\n[%s] %s", tsid, config$title))
  t_treat <- Sys.time()

  pd <- tryCatch(build_pair_diffs(config),
                 error = function(e) { log_error(tsid, "build_pair_diffs", conditionMessage(e)); NULL })
  if (is.null(pd)) next
  message(sprintf("  %s intact pairs (broken share %.2f%%)",
                  formatC(nrow(pd), big.mark = ","), pd$broken_share[1L] * 100))

  # Cell collapse: whitelist + minimum pair support (per treatment)
  pair_cell_n <- pd[, .N, by = cell_id]
  keep_cells <- pair_cell_n[cell_id %in% whitelist & N >= HTE_MIN_PAIRS, cell_id]

  # Fill the typical-property estimable_matched flag for this treatment: the
  # treated-modal cell is estimable when it is in the whitelist AND has enough
  # matched pairs for this treatment.
  modal_id_t <- typical_dt[treatment_short_id == tsid, modal_cell_id]
  if (length(modal_id_t) == 1L && !is.na(modal_id_t)) {
    n_mm <- pair_cell_n[cell_id == modal_id_t, N]
    n_mm <- if (length(n_mm) == 0L) 0L else n_mm[1L]
    typical_dt[treatment_short_id == tsid,
               estimable_matched := (modal_id_t %in% whitelist) && (n_mm >= HTE_MIN_PAIRS)]
  }
  pd[, cell_est := fifelse(cell_id %in% keep_cells, cell_id, "_other")]
  message(sprintf("  %d cells estimated (+ _other pooling %s pairs)",
                  length(keep_cells),
                  formatC(pd[cell_est == "_other", .N], big.mark = ",")))

  cells_buffer <- list()
  rw_buffer    <- list()
  tree_buffer  <- list()

  for (oc in hte_outcomes) {
    dy <- paste0("D_", oc)
    est <- pd[!is.na(get(dy)) & !is.na(D_rooms) & !is.na(D_floor)]
    if (nrow(est) < 100L) {
      log_error(tsid, paste0("outcome:", oc), sprintf("only %d pairs with valid D_y", nrow(est)))
      next
    }

    # --- Pooled ATT (pair-diff form of PSM + Subclass FE) ---
    att <- tryCatch(
      feols(as.formula(paste0(dy, " ~ 1 + D_rooms + D_floor")),
            data = est, cluster = ~local_authority, lean = TRUE),
      error = function(e) { log_error(tsid, paste0("att:", oc), conditionMessage(e)); NULL })
    if (is.null(att)) next
    act <- coeftable(att)
    att_coef <- act["(Intercept)", "Estimate"]
    att_se   <- act["(Intercept)", "Std. Error"]
    att_p    <- act["(Intercept)", "Pr(>|t|)"]

    # Consistency reference vs 06's headline coefficient. NOT an identity: the
    # headline "PSM + Subclass FE" keeps every EPC lodgement row and averages
    # them in the subclass FE, whereas this pair-diff selects one exact-var-
    # consistent lodgement per member. The two agree exactly only when no matched
    # uprn has duplicate lodgements; otherwise they should be close, same sign.
    if (oc == "current_energy_efficiency" && !is.null(ref_coefs)) {
      rc <- ref_coefs[treatment_short_id == tsid, ref_coef]
      if (length(rc) == 1L && !is.na(rc)) {
        message(sprintf("  Consistency ref: pair-diff ATT = %.6f vs headline = %.6f (diff %.2e, same sign: %s)",
                        att_coef, rc, abs(att_coef - rc), sign(att_coef) == sign(rc)))
      }
    }

    # --- Cell CATEs ---
    est[, cell_f := factor(cell_est)]
    cm <- tryCatch(
      feols(as.formula(paste0(dy, " ~ 0 + i(cell_f) + D_rooms + D_floor")),
            data = est, cluster = ~local_authority, lean = TRUE),
      error = function(e) { log_error(tsid, paste0("cells:", oc), conditionMessage(e)); NULL })
    if (is.null(cm)) next

    ct <- coeftable(cm)
    cell_rows_idx <- grep("^cell_f::", rownames(ct))
    cell_names <- sub("^cell_f::", "", rownames(ct)[cell_rows_idx])
    b <- ct[cell_rows_idx, "Estimate"]
    V <- tryCatch(vcov(cm)[cell_rows_idx, cell_rows_idx, drop = FALSE],
                  error = function(e) NULL)
    if (is.null(V)) { log_error(tsid, paste0("vcov:", oc), "vcov extraction failed"); next }

    wt <- wald_equal_cells(b, V)
    n_clust_total <- tryCatch(as.integer(fitstat(cm, "G", simplify = TRUE)),
                              error = function(e) NA_integer_)

    # Per-cell support (pairs + distinct LAs), computed on the estimation sample
    cell_support <- est[, .(n_pairs = .N, n_las = uniqueN(local_authority)),
                        by = cell_est]
    setkey(cell_support, cell_est)

    cells_buffer[[oc]] <- data.table(
      treatment_short_id = tsid,
      treatment = config$title,
      outcome = oc,
      cell_id = cell_names,
      beta = b,
      se = ct[cell_rows_idx, "Std. Error"],
      p_value = ct[cell_rows_idx, "Pr(>|t|)"],
      n_pairs = cell_support[cell_names, n_pairs],
      n_las = cell_support[cell_names, n_las],
      w_control_pool = unname(w_lookup[cell_names]),
      is_archetype = cell_names %in% archetype_cells,
      is_other = cell_names == "_other",
      few_clusters_flag = as.integer(cell_support[cell_names, n_las] < FEW_CLUSTERS_THRESHOLD),
      wald_chi2 = wt$chi2, wald_df = wt$df, wald_p = wt$p,
      n_clusters_total = n_clust_total,
      spec = HTE_SPEC_NAME, matching_core = HTE_MATCHING_CORE,
      matching_geography = MATCHING_GEOGRAPHY, run_id = run_id
    )

    # --- Reweighting (delta-method SEs from the single clustered vcov) ---
    real_idx <- which(cell_names != "_other")
    rw_row <- data.table(
      treatment_short_id = tsid, treatment = config$title, outcome = oc,
      att = att_coef, att_se = att_se, att_p = att_p,
      n_pairs = nrow(est), n_clusters = n_clust_total,
      n_cells_estimated = length(real_idx),
      tau_rw_full = NA_real_, tau_rw_full_se = NA_real_,
      coverage_full = NA_real_,
      tau_rw_arch10 = NA_real_, tau_rw_arch10_se = NA_real_,
      coverage_arch10 = NA_real_, n_archetypes_estimated = NA_integer_,
      composition_effect = NA_real_,
      wald_chi2 = wt$chi2, wald_df = wt$df, wald_p = wt$p,
      spec = HTE_SPEC_NAME, matching_core = HTE_MATCHING_CORE,
      matching_geography = MATCHING_GEOGRAPHY, run_id = run_id
    )
    if (length(real_idx) > 0L) {
      w_raw <- unname(w_lookup[cell_names[real_idx]])
      cov_full <- sum(w_raw)
      w_norm <- w_raw / cov_full
      rw_row[, tau_rw_full := sum(w_norm * b[real_idx])]
      rw_row[, tau_rw_full_se := sqrt(as.numeric(
        t(w_norm) %*% V[real_idx, real_idx, drop = FALSE] %*% w_norm))]
      rw_row[, coverage_full := cov_full]
      rw_row[, composition_effect := att_coef - tau_rw_full]

      arch_idx <- which(cell_names %in% archetype_cells)
      rw_row[, n_archetypes_estimated := length(arch_idx)]
      if (length(arch_idx) > 0L) {
        w_arch_raw <- unname(w_lookup[cell_names[arch_idx]])
        cov_arch <- sum(w_arch_raw)
        w_arch <- w_arch_raw / cov_arch
        rw_row[, tau_rw_arch10 := sum(w_arch * b[arch_idx])]
        rw_row[, tau_rw_arch10_se := sqrt(as.numeric(
          t(w_arch) %*% V[arch_idx, arch_idx, drop = FALSE] %*% w_arch))]
        rw_row[, coverage_arch10 := cov_arch]
      }
    }
    rw_buffer[[oc]] <- rw_row

    rm(cm, ct, V, att, act)

    # --- Honest tree (supporting; selected outcomes only) ---
    if (oc %in% HTE_TREE_OUTCOMES) {
      tree_res <- tryCatch({
        # 50/50 split, ITL1-stratified, seeded per treatment x outcome
        set.seed(20230703 + match(tsid, treat_shorts) * 100L + match(oc, hte_outcomes))
        est[, .split_train := as.logical(rbinom(.N, 1L, 0.5)), by = ITL1]
        train <- est[.split_train == TRUE]
        held  <- est[.split_train == FALSE]

        tree_fml <- as.formula(paste0(
          dy, " ~ property_type + built_form + construction_age_band + main_fuel + ",
          "total_floor_area + number_habitable_rooms + lodgement_year"))
        fit <- rpart(tree_fml, data = train, method = "anova",
                     control = rpart.control(maxdepth = 4, minbucket = 200,
                                             cp = 1e-4, xval = 10))
        cp_tab <- fit$cptable
        if (is.null(cp_tab) || nrow(cp_tab) < 2L || max(cp_tab[, "nsplit"]) == 0L) {
          data.table(treatment_short_id = tsid, treatment = config$title, outcome = oc,
                     node_id = 1L, node_rule = "<root: no splits survive pruning>",
                     n_train = nrow(train), n_est = nrow(held),
                     beta = NA_real_, se = NA_real_, p_value = NA_real_,
                     n_clusters = NA_integer_, run_id = run_id)
        } else {
          ii <- which.min(cp_tab[, "xerror"])
          thr <- cp_tab[ii, "xerror"] + cp_tab[ii, "xstd"]
          best_row <- min(which(cp_tab[, "xerror"] <= thr))
          fit_p <- prune(fit, cp = cp_tab[best_row, "CP"])

          if (nrow(fit_p$frame) <= 1L) {
            data.table(treatment_short_id = tsid, treatment = config$title, outcome = oc,
                       node_id = 1L, node_rule = "<root: 1-SE prune removed all splits>",
                       n_train = nrow(train), n_est = nrow(held),
                       beta = NA_real_, se = NA_real_, p_value = NA_real_,
                       n_clusters = NA_integer_, run_id = run_id)
          } else {
            held[, node := rpart_nodes(fit_p, held)]
            held[, node_f := factor(node)]
            nm <- feols(as.formula(paste0(dy, " ~ 0 + i(node_f)")),
                        data = held, cluster = ~local_authority, lean = TRUE)
            nt <- coeftable(nm)
            nidx <- grep("^node_f::", rownames(nt))
            node_ids <- as.integer(sub("^node_f::", "", rownames(nt)[nidx]))

            # Human-readable split rules per terminal node
            rules <- tryCatch({
              pr <- path.rpart(fit_p, nodes = node_ids, print.it = FALSE)
              vapply(as.character(node_ids), function(nd)
                paste(pr[[nd]][-1L], collapse = " & "), character(1L))
            }, error = function(e) rep(NA_character_, length(node_ids)))

            node_support <- held[, .(n_node = .N, n_las = uniqueN(local_authority)), by = node]
            setkey(node_support, node)
            out <- data.table(
              treatment_short_id = tsid, treatment = config$title, outcome = oc,
              node_id = node_ids, node_rule = rules,
              n_train = nrow(train), n_est = node_support[.(node_ids), n_node],
              beta = nt[nidx, "Estimate"], se = nt[nidx, "Std. Error"],
              p_value = nt[nidx, "Pr(>|t|)"],
              n_clusters = node_support[.(node_ids), n_las], run_id = run_id)
            rm(nm, nt)
            out
          }
        }
      }, error = function(e) {
        log_error(tsid, paste0("tree:", oc), conditionMessage(e))
        NULL
      })
      if (!is.null(tree_res)) tree_buffer[[oc]] <- tree_res
      if (".split_train" %in% names(est)) est[, .split_train := NULL]
    }

    rm(est); gc()
  }  # end outcome loop

  # --- Archetype x ownership CATE matrix (recover + flag, do NOT drop) ---
  # Direct pair-difference CATE for each narrative-archetype cell, this
  # treatment. Thin / few-cluster cells are flagged "suspect" and still
  # reported; cells below MATRIX_MIN_PAIRS are "none" (no reliable estimate).
  # This is the level-anchored interpretable-baseline exhibit's numerator.
  matrix_buffer <- list()
  matrix_cells <- unique(c(archetype_cells, reference_cells))
  for (ac in matrix_cells) {
    for (oc in intersect(MATRIX_OUTCOMES, hte_outcomes)) {
      dyy <- paste0("D_", oc)
      sub <- pd[cell_id == ac & !is.na(get(dyy)) & !is.na(D_rooms) & !is.na(D_floor)]
      npr <- nrow(sub)
      nla <- if (npr > 0L) uniqueN(sub$local_authority) else 0L
      beta <- NA_real_; se <- NA_real_; pval <- NA_real_; support <- "none"
      if (npr >= MATRIX_MIN_PAIRS) {
        mm <- tryCatch(
          feols(as.formula(paste0(dyy, " ~ 1 + D_rooms + D_floor")),
                data = sub, cluster = ~local_authority, lean = TRUE),
          error = function(e) NULL)
        if (!is.null(mm)) {
          mct  <- coeftable(mm)
          beta <- mct["(Intercept)", "Estimate"]
          se   <- mct["(Intercept)", "Std. Error"]
          pval <- mct["(Intercept)", "Pr(>|t|)"]
          support <- if (npr >= HTE_MIN_PAIRS && nla >= FEW_CLUSTERS_THRESHOLD) "ok" else "suspect"
          rm(mm)
        }
      }
      matrix_buffer[[paste(ac, oc)]] <- data.table(
        treatment_short_id = tsid, treatment = config$title,
        cell_id = ac, arch_rank = match(ac, archetype_cells),
        is_reference = ac %in% reference_cells,
        ref_label = unname(ref_label_lookup[ac]), outcome = oc,
        beta = beta, se = se, p_value = pval,
        ci_lower = beta - 1.96 * se, ci_upper = beta + 1.96 * se,
        n_pairs = npr, n_las = nla, support = support,
        spec = HTE_SPEC_NAME, matching_core = HTE_MATCHING_CORE,
        matching_geography = MATCHING_GEOGRAPHY, run_id = run_id)
    }
  }

  # Per-treatment write (crash-resume unit)
  if (length(rw_buffer) > 0L) {
    cells_dt <- rbindlist(cells_buffer, fill = TRUE)
    rw_dt    <- rbindlist(rw_buffer, fill = TRUE)
    fwrite(cells_dt, cells_path, append = file.exists(cells_path))
    fwrite(rw_dt, rw_path, append = file.exists(rw_path))
    if (length(tree_buffer) > 0L) {
      tree_dt <- rbindlist(tree_buffer, fill = TRUE)
      fwrite(tree_dt, tree_path, append = file.exists(tree_path))
    }
    if (length(matrix_buffer) > 0L) {
      matrix_dt <- rbindlist(matrix_buffer, fill = TRUE)
      fwrite(matrix_dt, matrix_path, append = file.exists(matrix_path))
      rm(matrix_dt)
    }
    message(sprintf("  [%s] wrote %d cell rows, %d reweighted rows, %d tree rows, %d matrix rows (%.1f min)",
                    tsid, nrow(cells_dt), nrow(rw_dt),
                    if (length(tree_buffer) > 0L) nrow(rbindlist(tree_buffer, fill = TRUE)) else 0L,
                    length(matrix_buffer),
                    as.numeric(difftime(Sys.time(), t_treat, units = "mins"))))
    rm(cells_dt, rw_dt)
  } else {
    message(sprintf("  [%s] no results produced (see %s)", tsid, basename(err_path)))
  }

  rm(pd, cells_buffer, rw_buffer, tree_buffer, matrix_buffer)
  gc()
}

# Write the typical-property descriptive table (R2)
if (exists("typical_dt") && nrow(typical_dt) > 0L) {
  fwrite(typical_dt, typical_path)
  message(sprintf("\n  Typical-property table written: %s (%d treatments; %d with estimable matched modal cell)",
                  basename(typical_path), nrow(typical_dt),
                  sum(typical_dt$estimable_matched %in% TRUE)))
}

end.time <- Sys.time()
message(sprintf("\n06c_heterogeneous_effects.R complete [%s]. Runtime: %.1f min.",
                MATCHING_GEOGRAPHY,
                as.numeric(difftime(end.time, start.time, units = "mins"))))
