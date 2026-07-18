# Script: 06c_heterogeneous_effects_pilot.R
# Purpose: PILOT of the archetype reweighting method with the estimability gate
#          fixed. The production 06c gates which cells receive their own CATE on a
#          top-100 CONTROL-POOL frequency whitelist (HTE_MAX_CELLS = 100). That
#          whitelist is the correct base for the reweighting WEIGHTS (control-pool
#          shares are treatment-invariant), but it is the WRONG gate for
#          estimability: a cell that is rare in the overall control pool can still
#          have abundant MATCHED support for a treatment that concentrates in it.
#          The Foreign For-Profit modal cell -- a modern (2003-2006) electric flat
#          -- is exactly such a cell: ~1,300 matched pairs across many LAs, yet
#          06c reports it "estimable = no" only because it is outside the top 100.
#
#          This pilot replaces the whitelist estimability gate with a per-treatment
#          MATCHED-SUPPORT floor (n_pairs >= HTE_MIN_PAIRS AND n_las >=
#          FEW_CLUSTERS_THRESHOLD), keeps the control-pool shares as the reweighting
#          weights, and reports BOTH the whitelist-restricted and the support-based
#          tau_rw / coverage so the loss from the top-100 cap is explicit.
#
#          Lean descendant of 06c: all 14 standard treatments, two outcomes
#          (current_energy_efficiency primary, bad_epc_c secondary), no honest tree,
#          no diversity archetype selection. frfp + np are the detailed worked
#          example within the sweep.
#
#          Outputs (LA, Baseline spec, base core):
#            - hte_pilot_cells_LA.csv        per (treatment x outcome x cell)
#            - hte_pilot_estimability_LA.csv  one row per treatment = modal cell
#            - hte_pilot_summary_LA.csv       per (treatment x outcome) reweighting
#
# Reuses helpers ported from 06c: make_cell_id, assign_tercile, classify_fuel_class,
# classify_era, build_pair_diffs (trimmed to 2 outcomes), wald_equal_cells.
#
# Standalone: run ALONE (full-parquet load, ~15.7 GB peak). Existing 06c untouched.
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 10, 2026.
rm(list = setdiff(ls(), c("script", "pipeline.start.time")))
gc()

set.seed(20230703)
start.time <- Sys.time()

run_id <- local({
  rid <- Sys.getenv("PIPELINE_RUN_ID", unset = "")
  if (nzchar(rid)) rid else format(start.time, "pilot_%Y%m%d_%H%M%S")
})

library(data.table)
library(fixest)
library(arrow)
library(dplyr)
library(here)
library(jsonlite)

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))

setFixest_notes(FALSE)
setFixest_nthreads(2)

if (WITHIN_CORPORATE) {
  stop("06c_heterogeneous_effects_pilot.R does not support WITHIN_CORPORATE mode.")
}

# Tuning parameters ---------------------------------------------------------
HTE_MIN_PAIRS          <- 50L    # matched-support floor: cells with fewer pairs -> "_other"
HTE_MAX_CELLS          <- 100L   # 06c whitelist size (kept ONLY for the comparison arm)
FEW_CLUSTERS_THRESHOLD <- 20L    # matched-support floor: distinct LAs
BROKEN_PAIR_WARN_SHARE <- 0.03

HTE_SPEC_NAME     <- "Baseline"
HTE_MATCHING_CORE <- "base"

# Two outcomes only (primary first)
hte_outcomes <- c("current_energy_efficiency", "bad_epc_c")

archetype_vars     <- c("property_type", "built_form", "construction_age_band", "main_fuel")
base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                        "property_type", "main_fuel", "construction_age_band", "built_form")

# Coarse buckets (ported from 06c; used only for readable cell labels here)
classify_fuel_class <- function(mf) {
  data.table::fcase(
    grepl("gas", mf, ignore.case = TRUE), "gas",
    grepl("electric", mf, ignore.case = TRUE), "electric",
    grepl("oil", mf, ignore.case = TRUE), "oil",
    default = "other")
}
classify_era <- function(ab) {
  yr <- suppressWarnings(as.integer(sub(".*?([0-9]{4}).*", "\\1", ab)))
  is_before1900 <- grepl("before 1900", ab, ignore.case = TRUE)
  data.table::fcase(
    is_before1900, "period",
    !is.na(yr) & yr < 1950, "period",
    !is.na(yr) & yr <= 1982, "mid",
    !is.na(yr) & yr >= 1983, "modern",
    default = "other")
}

matched_data_dir <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY)
summary_dir      <- SUMMARY_TABLES_DIR

message("===================================================================")
message("  06c_heterogeneous_effects_pilot.R - estimability-gate pilot")
message(sprintf("  Geography: %s | Spec: %s | Core: %s",
                MATCHING_GEOGRAPHY, HTE_SPEC_NAME, HTE_MATCHING_CORE))
message(sprintf("  Support floor: n_pairs >= %d AND n_las >= %d (whitelist NOT required)",
                HTE_MIN_PAIRS, FEW_CLUSTERS_THRESHOLD))
message(sprintf("  Started: %s", format(start.time, "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

if (!dir.exists(matched_data_dir)) stop("Matched data directory not found: ", matched_data_dir)

# Output paths ---------------------------------------------------------------
cells_path <- file.path(summary_dir, paste0("hte_pilot_cells_", MATCHING_GEOGRAPHY, ".csv"))
est_path   <- file.path(summary_dir, paste0("hte_pilot_estimability_", MATCHING_GEOGRAPHY, ".csv"))
summ_path  <- file.path(summary_dir, paste0("hte_pilot_summary_", MATCHING_GEOGRAPHY, ".csv"))
manifest_path <- file.path(summary_dir, paste0("run_manifest_06c_pilot_", MATCHING_GEOGRAPHY, ".json"))

# Fresh pilot run: clear prior pilot outputs so appends do not accumulate.
for (p in c(cells_path, est_path, summ_path)) if (file.exists(p)) file.remove(p)

# 1. Load data (column-selected, streamed per-file) --------------------------
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
rm(ds)

# Per-file streamed collect (caps peak RAM near the final table size).
arrow::set_cpu_count(1L)
dat_parts <- vector("list", length(all_parquet_files))
for (i in seq_along(all_parquet_files)) {
  dat_parts[[i]] <- setDT(
    arrow::open_dataset(all_parquet_files[i], format = "parquet") |>
      select(all_of(available_cols)) |> collect())
}
dat <- rbindlist(dat_parts, use.names = TRUE, fill = TRUE)
rm(dat_parts); gc()
message(sprintf("  Loaded %s rows x %d cols",
                formatC(nrow(dat), big.mark = ","), ncol(dat)))

dat <- define_treatments(dat)

# Regulatory-bound secondary outcome (mirrors 06_run_regressions.R)
cee <- dat[["current_energy_efficiency"]]
dat[, bad_epc_c := fifelse(is.na(cee), NA_real_, as.numeric(cee < 69))]
rm(cee)

drop_cols <- intersect(c("source", "tenure_2", "coarse_proprietorship",
                         "country_incorporated_1", "country_incorporated_tax_haven",
                         "country_incorporated_british_haven",
                         "country_incorporated_european_haven",
                         "country_incorporated_caribbean_haven",
                         "country_incorporated_other_haven"), names(dat))
if (length(drop_cols) > 0L) dat[, (drop_cols) := NULL]

# Keep dat at lodgement-row level (05 matched at lodgement level, saved only uprn;
# the pair join fans out over lodgements and build_pair_diffs() picks the exact-var-
# consistent combination -- see there).
setkey(dat, uprn)
gc()

# 2. Control pool -> cell frequencies, weights, terciles, A1 anchor ----------
message("\n--- Building control-pool cell frequencies and reweighting weights ---")
ctrl_pool <- unique(dat[!is.na(treat_for_profit) & treat_for_profit == 0L], by = "uprn")
ctrl_pool <- ctrl_pool[complete.cases(ctrl_pool[, ..base_matching_vars])]
message(sprintf("  Eligible control pool: %s properties", formatC(nrow(ctrl_pool), big.mark = ",")))

tercile_cuts <- quantile(ctrl_pool$total_floor_area, probs = c(1/3, 2/3),
                         na.rm = TRUE, names = FALSE)
message(sprintf("  Floor-area tercile cuts: %.1f / %.1f sqm", tercile_cuts[1], tercile_cuts[2]))

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
cell_freq[, rank := .I]
cell_freq[, in_top100 := rank <= HTE_MAX_CELLS]

# Reweighting weights w_c = control-pool cell shares (treatment-invariant base).
w_lookup    <- setNames(cell_freq$share_control_pool, cell_freq$cell_id)
rank_lookup <- setNames(cell_freq$rank, cell_freq$cell_id)
top100_lookup <- setNames(cell_freq$in_top100, cell_freq$cell_id)
whitelist   <- cell_freq[in_top100 == TRUE, cell_id]

# A1 = rank-1 modal control-pool cell (the common anchor archetype).
A1_cell <- cell_freq$cell_id[1L]
message(sprintf("  A1 (modal control-pool cell): %s  [share %.3f]",
                A1_cell, w_lookup[[A1_cell]]))
message(sprintf("  Top-100 whitelist covers %.1f%% of the control pool.",
                cell_freq[in_top100 == TRUE, sum(share_control_pool)] * 100))

# Per-cell control-pool baseline EPC (the level a treatment effect shifts from).
cell_baseline <- ctrl_pool[, .(baseline_epc = mean(current_energy_efficiency, na.rm = TRUE)),
                           by = cell_id]
baseline_lookup <- setNames(cell_baseline$baseline_epc, cell_baseline$cell_id)

rm(ctrl_pool); gc()

# 3. Modal treated cell per treatment (for the estimability sweep) -----------
modal_lookup <- list()
for (cfg in treatment_metadata) {
  tv_col <- cfg$var
  if (!tv_col %in% names(dat)) next
  tp <- unique(dat[!is.na(get(tv_col)) & get(tv_col) == 1L], by = "uprn")
  tp <- tp[complete.cases(tp[, ..base_matching_vars])]
  if (nrow(tp) == 0L) next
  tp[, cell_id := make_cell_id(.SD)]
  freq <- tp[, .N, by = cell_id][order(-N)]
  modal_lookup[[cfg$short_id]] <- list(
    modal_cell = freq$cell_id[1L],
    n_treated  = nrow(tp),
    modal_share = freq$N[1L] / nrow(tp))
  rm(tp, freq)
}
gc()

# 4. Helpers -----------------------------------------------------------------
build_pair_diffs <- function(config) {
  matched_file <- file.path(matched_data_dir,
                            paste0("matched_pairs_", config$file_id,
                                   "_matching_core_", HTE_MATCHING_CORE,
                                   "_", CCOD_VERSION, ".RData"))
  if (!file.exists(matched_file)) {
    stop("matched file missing: ", basename(matched_file))
  }
  local_env <- new.env(parent = emptyenv())
  load(matched_file, envir = local_env)
  md <- local_env$matched_results[[HTE_SPEC_NAME]]
  rm(local_env)
  if (is.null(md) || nrow(md) == 0L) stop("no Baseline matched pairs in RData")
  md <- md[!is.na(uprn)]
  if ("weights" %in% names(md) && any(md$weights != 1)) {
    stop(sprintf("non-unit weights found (%d rows) -- pair collapse invalid",
                 sum(md$weights != 1)))
  }
  md <- unique(md[, .(uprn, subclass, distance)], by = c("subclass", "uprn"))

  pr <- dat[md, on = "uprn", allow.cartesian = TRUE, nomatch = 0]
  pr[, tv := pr[[config$var]]]
  pr <- pr[!is.na(tv)]
  n_subclass_total <- uniqueN(pr$subclass)

  cons_vars <- setdiff(Filter(function(s) identical(s$name, HTE_SPEC_NAME),
                              spec_configs)[[1L]]$exact_vars, "local_authority")
  pr[, .stratum := do.call(paste, c(.SD, sep = "\r")), .SDcols = cons_vars]
  both <- pr[, .(has_t = any(tv == 1L), has_c = any(tv == 0L)),
             by = .(subclass, .stratum)][has_t & has_c]
  if (nrow(both) == 0L) stop("no exact-var-consistent pairs after fan-out")
  setorder(both, subclass, .stratum)
  chosen <- both[, .(.stratum = .stratum[1L]), by = subclass]
  keep <- pr[chosen, on = .(subclass, .stratum), nomatch = 0]
  p_t <- keep[tv == 1L]; p_t <- p_t[p_t[, .I[1L], by = subclass]$V1]
  p_c <- keep[tv == 0L]; p_c <- p_c[p_c[, .I[1L], by = subclass]$V1]
  good <- intersect(p_t$subclass, p_c$subclass)
  p_t <- p_t[subclass %in% good]
  p_c <- p_c[subclass %in% good]

  n_broken <- n_subclass_total - length(good)
  broken_share <- if (n_subclass_total > 0L) n_broken / n_subclass_total else NA_real_
  if (!is.na(broken_share) && broken_share > BROKEN_PAIR_WARN_SHARE) {
    message(sprintf("  NOTE [%s]: %.1f%% of subclasses unrecoverable (%d of %d) -- EPC-CCOD linkage gap.",
                    config$short_id, broken_share * 100, n_broken, n_subclass_total))
  }
  if (length(good) == 0L) stop("no intact pairs after consistent selection")

  t_cols <- c("subclass", hte_outcomes, "number_habitable_rooms", "total_floor_area")
  p_t <- p_t[, ..t_cols]
  setnames(p_t, setdiff(t_cols, "subclass"), paste0("t_", setdiff(t_cols, "subclass")))
  c_cols <- unique(c("subclass", "local_authority", "ITL1", hte_outcomes,
                     "number_habitable_rooms", "total_floor_area",
                     archetype_vars, "lodgement_year"))
  c_cols <- intersect(c_cols, names(p_c))
  p_c <- p_c[, ..c_cols]

  pd <- merge(p_t, p_c, by = "subclass")
  for (oc in hte_outcomes) pd[, paste0("D_", oc) := get(paste0("t_", oc)) - get(oc)]
  pd[, D_rooms := t_number_habitable_rooms - number_habitable_rooms]
  pd[, D_floor := t_total_floor_area - total_floor_area]
  pd[, cell_id := make_cell_id(.SD)]
  pd[, broken_share := broken_share]
  pd
}

# Cochran's Q heterogeneity test over independently-estimated cell CATEs
# (H0: all beta_c equal). var = per-cell sampling variances.
cochran_q <- function(b, var) {
  ok <- is.finite(b) & is.finite(var) & var > 0
  b <- b[ok]; var <- var[ok]
  K <- length(b)
  if (K < 2L) return(list(chi2 = NA_real_, df = NA_integer_, p = NA_real_))
  w <- 1 / var
  bbar <- sum(w * b) / sum(w)
  Q <- sum(w * (b - bbar)^2)
  list(chi2 = Q, df = K - 1L, p = pchisq(Q, df = K - 1L, lower.tail = FALSE))
}

# Reweighted ATT over a subset of estimated cells. Cells are estimated on
# DISJOINT matched-pair samples, so their CATEs are independent and the
# reweighting variance is Var(tau_rw) = sum_c w_norm_c^2 Var(beta_c).
# w_raw = control-pool shares; renormalised over covered cells (matches 06c).
# b_by / var_by are named vectors keyed by cell_id.
reweight <- function(b_by, var_by, keep_ids) {
  ids <- intersect(names(b_by), keep_ids)
  ids <- ids[is.finite(b_by[ids])]
  out <- list(tau = NA_real_, se = NA_real_, coverage = NA_real_,
              n_cells = 0L, contrib = setNames(numeric(0), character(0)))
  if (length(ids) == 0L) return(out)
  w_raw  <- unname(w_lookup[ids])
  cov    <- sum(w_raw)
  w_norm <- w_raw / cov
  out$tau      <- sum(w_norm * b_by[ids])
  out$se       <- sqrt(sum(w_norm^2 * var_by[ids]))
  out$coverage <- cov
  out$n_cells  <- length(ids)
  out$contrib  <- setNames(w_norm * b_by[ids], ids)
  out
}

# 5. Main sweep: per treatment -----------------------------------------------
message("\n=== Estimability-gate sweep over all treatments ===")
cells_all <- list()
summ_all  <- list()
est_all   <- list()

for (config in treatment_metadata) {
  tsid <- config$short_id
  message(sprintf("\n[%s] %s", tsid, config$title))
  t_treat <- Sys.time()

  ml <- modal_lookup[[tsid]]
  modal_cell  <- if (!is.null(ml)) ml$modal_cell  else NA_character_
  n_treated_t <- if (!is.null(ml)) ml$n_treated   else NA_integer_
  modal_share <- if (!is.null(ml)) ml$modal_share else NA_real_

  pd <- tryCatch(build_pair_diffs(config), error = function(e) {
    message(sprintf("  FAIL build_pair_diffs: %s", conditionMessage(e)))
    conditionMessage(e)
  })

  # Failure to build pairs -> emit a fail row in the sweep and continue.
  if (is.character(pd)) {
    est_all[[tsid]] <- data.table(
      treatment_short_id = tsid, treatment = config$title,
      modal_cell_id = modal_cell, n_treated = n_treated_t,
      total_pairs = 0L, modal_cell_pairs = 0L, modal_cell_las = 0L,
      modal_in_top100 = if (!is.na(modal_cell)) isTRUE(unname(top100_lookup[modal_cell])) else NA,
      modal_rank = if (!is.na(modal_cell)) unname(rank_lookup[modal_cell]) else NA_integer_,
      modal_w = if (!is.na(modal_cell)) unname(w_lookup[modal_cell]) else NA_real_,
      estimable_06c = FALSE, estimable_support = FALSE,
      status = "fail", reason = paste0("no matched pairs: ", pd),
      geo_level = MATCHING_GEOGRAPHY, run_id = run_id)
    next
  }

  total_pairs <- nrow(pd)
  message(sprintf("  %s intact pairs (broken share %.2f%%)",
                  formatC(total_pairs, big.mark = ","), pd$broken_share[1L] * 100))

  # Per-cell matched support (treatment-level, outcome-invariant).
  cell_supp <- pd[, .(n_pairs = .N, n_las = uniqueN(local_authority)), by = cell_id]
  cell_supp[, status := fcase(
    n_pairs >= HTE_MIN_PAIRS & n_las >= FEW_CLUSTERS_THRESHOLD, "ok",
    n_pairs >= HTE_MIN_PAIRS & n_las <  FEW_CLUSTERS_THRESHOLD, "suspect",
    default = "fail")]
  setkey(cell_supp, cell_id)

  # --- Estimability gates ---
  # Support-based (pilot): matched support only, whitelist NOT required.
  keep_support <- cell_supp[status == "ok", cell_id]
  # 06c whitelist arm (for the comparison): in top-100 AND >= min pairs.
  keep_top100  <- cell_supp[n_pairs >= HTE_MIN_PAIRS & cell_id %in% whitelist, cell_id]
  # Estimate a CATE for the union of both arms' cells; reweight over each arm's
  # subset. Each cell's CATE is fit on a DISJOINT sample (its own matched pairs),
  # so K is unbounded at trivial memory cost -- no whitelist/size cap needed.
  keep_union <- union(keep_support, keep_top100)
  message(sprintf("  cells estimable: support=%d, top100=%d (union %d cells fit per-cell)",
                  length(keep_support), length(keep_top100), length(keep_union)))

  # --- Estimability sweep row (primary outcome frame; modal cell) ---
  ms <- if (!is.na(modal_cell) && modal_cell %in% cell_supp$cell_id) cell_supp[modal_cell] else NULL
  modal_pairs <- if (!is.null(ms)) ms$n_pairs else 0L
  modal_las   <- if (!is.null(ms)) ms$n_las   else 0L
  modal_status <- if (!is.null(ms)) ms$status else "fail"
  modal_in_top100 <- if (!is.na(modal_cell)) isTRUE(unname(top100_lookup[modal_cell])) else NA
  reason <- switch(modal_status,
    ok      = sprintf("matched support ok: n_pairs=%d>=%d, n_las=%d>=%d",
                      modal_pairs, HTE_MIN_PAIRS, modal_las, FEW_CLUSTERS_THRESHOLD),
    suspect = sprintf("few clusters: n_las=%d<%d (n_pairs=%d>=%d)",
                      modal_las, FEW_CLUSTERS_THRESHOLD, modal_pairs, HTE_MIN_PAIRS),
    fail    = sprintf("insufficient matched support: n_pairs=%d<%d (n_las=%d)",
                      modal_pairs, HTE_MIN_PAIRS, modal_las))
  est_all[[tsid]] <- data.table(
    treatment_short_id = tsid, treatment = config$title,
    modal_cell_id = modal_cell, n_treated = n_treated_t, modal_share = modal_share,
    total_pairs = total_pairs, modal_cell_pairs = modal_pairs, modal_cell_las = modal_las,
    modal_in_top100 = modal_in_top100,
    modal_rank = if (!is.na(modal_cell)) unname(rank_lookup[modal_cell]) else NA_integer_,
    modal_w = if (!is.na(modal_cell)) unname(w_lookup[modal_cell]) else NA_real_,
    estimable_06c = isTRUE(modal_in_top100) && modal_pairs >= HTE_MIN_PAIRS,
    estimable_support = modal_status == "ok",
    status = modal_status, reason = reason,
    geo_level = MATCHING_GEOGRAPHY, run_id = run_id)
  message(sprintf("  MODAL %s | pairs=%d las=%d top100=%s | 06c=%s support=%s [%s]",
                  modal_cell, modal_pairs, modal_las, modal_in_top100,
                  est_all[[tsid]]$estimable_06c, est_all[[tsid]]$estimable_support, modal_status))

  # --- Per-outcome estimation ---
  for (oc in hte_outcomes) {
    dy <- paste0("D_", oc)
    est <- pd[!is.na(get(dy)) & !is.na(D_rooms) & !is.na(D_floor)]
    if (nrow(est) < 100L) {
      message(sprintf("  [%s] skip: only %d pairs with valid D_y", oc, nrow(est)))
      next
    }

    att <- tryCatch(
      feols(as.formula(paste0(dy, " ~ 1 + D_rooms + D_floor")),
            data = est, cluster = ~local_authority, lean = TRUE),
      error = function(e) { message(sprintf("  [%s|%s] att feols FAIL: %s", tsid, oc, conditionMessage(e))); NULL })
    if (is.null(att)) next
    act <- coeftable(att)
    att_coef <- act["(Intercept)", "Estimate"]
    att_se   <- act["(Intercept)", "Std. Error"]
    att_p    <- act["(Intercept)", "Pr(>|t|)"]

    # --- Per-cell CATEs (each on its own disjoint matched-pair sample) ---
    # feols(D_y ~ 1 + D_rooms + D_floor) restricted to the cell, LA-clustered.
    # Mirrors 06c's archetype-matrix recover+flag; memory-trivial for any K.
    cn <- keep_union
    b_by <- setNames(rep(NA_real_, length(cn)), cn)
    se_by <- setNames(rep(NA_real_, length(cn)), cn)
    p_by  <- setNames(rep(NA_real_, length(cn)), cn)
    fml_cell <- as.formula(paste0(dy, " ~ 1 + D_rooms + D_floor"))
    setkey(est, cell_id)   # fast binary-search subset per cell
    for (cc in cn) {
      sub <- est[.(cc), nomatch = NULL]
      if (nrow(sub) < HTE_MIN_PAIRS) next
      mm <- tryCatch(feols(fml_cell, data = sub, cluster = ~local_authority, lean = TRUE),
                     error = function(e) NULL)
      if (is.null(mm)) next
      mct <- coeftable(mm)
      if (!"(Intercept)" %in% rownames(mct)) next
      b_by[cc]  <- mct["(Intercept)", "Estimate"]
      se_by[cc] <- mct["(Intercept)", "Std. Error"]
      p_by[cc]  <- mct["(Intercept)", "Pr(>|t|)"]
    }
    var_by <- se_by^2
    ok_cell <- names(b_by)[is.finite(b_by[names(b_by)])]
    n_clust_total <- uniqueN(est$local_authority)

    # Defaults for the no-estimable-cell case (e.g. oh): keep the pooled ATT row
    # so the treatment still appears in the summary and Fig P1, with NA reweighting.
    wt <- list(chi2 = NA_real_, df = NA_integer_, p = NA_real_)
    rw_s <- list(tau = NA_real_, se = NA_real_, coverage = NA_real_, n_cells = 0L,
                 contrib = setNames(numeric(0), character(0)))
    rw_t <- rw_s
    contrib_A1 <- NA_real_; contrib_modal <- NA_real_
    beta_A1 <- NA_real_;    beta_modal <- NA_real_

    if (length(ok_cell) > 0L) {
      wt <- cochran_q(b_by[ok_cell], var_by[ok_cell])
      supp_by_cell <- cell_supp[ok_cell, on = "cell_id"]

      # --- Per-cell rows ---
      cells_all[[paste(tsid, oc)]] <- data.table(
        treatment_short_id = tsid, treatment = config$title, outcome = oc,
        cell_id = ok_cell, beta = unname(b_by[ok_cell]), se = unname(se_by[ok_cell]),
        p_value = unname(p_by[ok_cell]),
        w_c = unname(w_lookup[ok_cell]),
        n_pairs = supp_by_cell$n_pairs, n_las = supp_by_cell$n_las,
        rank = unname(rank_lookup[ok_cell]),
        in_top100 = as.logical(unname(top100_lookup[ok_cell])),
        is_modal = ok_cell == modal_cell,
        is_A1 = ok_cell == A1_cell,
        baseline_epc = unname(baseline_lookup[ok_cell]),
        support = supp_by_cell$status,
        spec = HTE_SPEC_NAME, matching_core = HTE_MATCHING_CORE,
        matching_geography = MATCHING_GEOGRAPHY, run_id = run_id)

      # --- Reweighting: support arm vs top-100 arm ---
      rw_s <- reweight(b_by, var_by, keep_support)
      rw_t <- reweight(b_by, var_by, keep_top100)

      contrib_A1    <- if (A1_cell %in% names(rw_s$contrib)) rw_s$contrib[[A1_cell]] else NA_real_
      contrib_modal <- if (!is.na(modal_cell) && modal_cell %in% names(rw_s$contrib))
                         rw_s$contrib[[modal_cell]] else NA_real_
      beta_A1    <- if (A1_cell %in% ok_cell) unname(b_by[A1_cell]) else NA_real_
      beta_modal <- if (!is.na(modal_cell) && modal_cell %in% ok_cell)
                      unname(b_by[modal_cell]) else NA_real_
    } else {
      message(sprintf("  [%s|%s] no cell CATE estimable (pooled ATT only)", tsid, oc))
    }

    summ_all[[paste(tsid, oc)]] <- data.table(
      treatment_short_id = tsid, treatment = config$title, outcome = oc,
      att = att_coef, att_se = att_se, att_p = att_p,
      n_pairs = nrow(est), n_clusters = n_clust_total,
      tau_rw_support = rw_s$tau, tau_rw_support_se = rw_s$se,
      coverage_support = rw_s$coverage, n_cells_support = rw_s$n_cells,
      tau_rw_top100 = rw_t$tau, tau_rw_top100_se = rw_t$se,
      coverage_top100 = rw_t$coverage, n_cells_top100 = rw_t$n_cells,
      composition = att_coef - rw_s$tau,
      composition_top100 = att_coef - rw_t$tau,
      A1_cell = A1_cell, w_A1 = unname(w_lookup[A1_cell]),
      beta_A1 = beta_A1, contrib_A1 = contrib_A1,
      modal_cell = modal_cell, w_modal = unname(w_lookup[modal_cell]),
      beta_modal = beta_modal, contrib_modal = contrib_modal,
      het_q = wt$chi2, het_df = wt$df, het_p = wt$p,
      spec = HTE_SPEC_NAME, matching_core = HTE_MATCHING_CORE,
      matching_geography = MATCHING_GEOGRAPHY, run_id = run_id)

    if (oc == "current_energy_efficiency") {
      message(sprintf("  ATT=%.3f | tau_rw(support)=%.3f cov=%.2f | tau_rw(top100)=%.3f cov=%.2f | comp=%.3f",
                      att_coef, rw_s$tau, rw_s$coverage, rw_t$tau, rw_t$coverage,
                      att_coef - rw_s$tau))
    }
    rm(att, act, est); gc()
  }

  message(sprintf("  [%s] done (%.1f min)", tsid,
                  as.numeric(difftime(Sys.time(), t_treat, units = "mins"))))
  rm(pd); gc()
}

# 6. Write outputs -----------------------------------------------------------
message("\n--- Writing pilot outputs ---")
if (length(cells_all) > 0L) fwrite(rbindlist(cells_all, fill = TRUE), cells_path)
if (length(est_all)  > 0L)  fwrite(rbindlist(est_all,  fill = TRUE), est_path)
if (length(summ_all) > 0L)  fwrite(rbindlist(summ_all, fill = TRUE), summ_path)
message(sprintf("  Wrote:\n    %s\n    %s\n    %s",
                basename(cells_path), basename(est_path), basename(summ_path)))

jsonlite::write_json(
  list(run_id = run_id, script = "06c_heterogeneous_effects_pilot.R",
       matching_geography = MATCHING_GEOGRAPHY, ccod_version = CCOD_VERSION,
       spec = HTE_SPEC_NAME, matching_core = HTE_MATCHING_CORE,
       hte_min_pairs = HTE_MIN_PAIRS, few_clusters_threshold = FEW_CLUSTERS_THRESHOLD,
       hte_max_cells = HTE_MAX_CELLS, outcomes = hte_outcomes, A1_cell = A1_cell,
       r_version = R.version$version.string,
       start_time = format(start.time, "%Y-%m-%dT%H:%M:%S"),
       end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
       hostname = Sys.info()[["nodename"]]),
  manifest_path, auto_unbox = TRUE, pretty = TRUE)

end.time <- Sys.time()
message(sprintf("\n06c_heterogeneous_effects_pilot.R complete [%s]. Runtime: %.1f min.",
                MATCHING_GEOGRAPHY,
                as.numeric(difftime(end.time, start.time, units = "mins"))))
