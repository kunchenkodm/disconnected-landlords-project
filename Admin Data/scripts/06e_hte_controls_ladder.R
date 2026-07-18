# Script: 06e_hte_controls_ladder.R
# Purpose: Controls-ladder version of the HTE reweighting exercise (forest-chart
#          v2). Script 06c estimates archetype CATEs and the reweighted ATT for
#          the Baseline spec x base core only; this script generalises the same
#          pair-difference machinery across ALL valid spec x core combinations
#          ("control types") so the three headline coefficients can be compared
#          across control sets on one chart:
#            1. Pooled pair-diff ATT (consistency reference to the pipeline's
#               "PSM (Matched) + Subclass FE" coefficient for that spec x core),
#            2. The modal-property-archetype CATE (A1 from 06c's diversity
#               selection: the overall most common control-pool cell),
#            3. The reweighted ATT tau_rw = sum_c w_c beta_c.
#
#          Cell definitions, floor-tercile cuts, the whitelist and the control-
#          pool weights are all taken FROM THE EXISTING 06c DEFINITIONS FILE
#          (hte_archetype_definitions_<GEO>.csv), so the reference composition
#          is IDENTICAL across control types: only the estimation sample and
#          the control set vary along the x-axis. Run 06c first.
#
# Scope: LA geography; headline forest treatments (fp, ukfp, frfp, np, th, ps);
#        outcomes bad_epc_c + current_energy_efficiency; all 9 spec x core
#        combos from spec_core_pairs. Output: hte_controls_ladder_<GEO>.csv.
#        Heavy (full-parquet load + 54 fan-out joins): never run concurrently
#        with another full-parquet script.
#
# Authors: Dmytro Kunchenko
# Date: July 12, 2026.
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

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))

setFixest_notes(FALSE)
setFixest_nthreads(2)

if (WITHIN_CORPORATE) {
  stop("06e_hte_controls_ladder.R does not support WITHIN_CORPORATE mode.")
}

# Tuning parameters (mirror 06c) ---------------------------------------------
HTE_MIN_PAIRS <- 50L            # cells with fewer pairs collapse to "_other"
FEW_CLUSTERS_THRESHOLD <- 20L
BROKEN_PAIR_WARN_SHARE <- 0.03

ladder_outcomes <- c("bad_epc_c", "current_energy_efficiency")

# Headline forest treatments; override with LADDER_TREATMENTS="fp,np" etc.
ladder_treats <- local({
  x <- Sys.getenv("LADDER_TREATMENTS", unset = "")
  if (nzchar(x)) trimws(strsplit(x, ",")[[1L]]) else c("fp", "ukfp", "frfp", "np", "th", "ps")
})

archetype_vars <- c("property_type", "built_form", "construction_age_band", "main_fuel")

matched_data_dir <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY)
summary_dir      <- SUMMARY_TABLES_DIR

ladder_path <- file.path(summary_dir, paste0("hte_controls_ladder_", MATCHING_GEOGRAPHY, ".csv"))
err_path    <- file.path(summary_dir, paste0("hte_controls_ladder_errors_", MATCHING_GEOGRAPHY, ".csv"))

message("===================================================================")
message("  06e_hte_controls_ladder.R - HTE across control types")
message(sprintf("  Geography: %s | Treatments: %s",
                MATCHING_GEOGRAPHY, paste(ladder_treats, collapse = ", ")))
message(sprintf("  Started: %s", format(start.time, "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

log_error <- function(treatment, stage, msg) {
  row <- data.table(run_id = run_id, treatment_short_id = treatment,
                    stage = stage, error_message = msg,
                    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  fwrite(row, err_path, append = file.exists(err_path))
  message(sprintf("  ERROR [%s | %s]: %s", treatment, stage, msg))
}


# 1. Cell definitions from the 06c run ----------------------------------------
# The whole point of the ladder is a FIXED reference composition, so the cell
# system (tercile cuts, whitelist, weights, modal archetype) is read from the
# 06c definitions file rather than rebuilt per core.
defs_path <- file.path(summary_dir, paste0("hte_archetype_definitions_", MATCHING_GEOGRAPHY, ".csv"))
if (!file.exists(defs_path)) {
  stop("06c definitions file not found (run 06c first): ", defs_path)
}
defs <- fread(defs_path, na.strings = c("NA", ""))
if (!all(c("cell_id", "share_control_pool", "arch_rank",
           "floor_tercile_cut1", "floor_tercile_cut2") %in% names(defs))) {
  stop("Definitions file missing required columns: ", defs_path)
}

whitelist     <- defs$cell_id
w_lookup      <- setNames(defs$share_control_pool, defs$cell_id)
tercile_cuts  <- c(defs$floor_tercile_cut1[1L], defs$floor_tercile_cut2[1L])
modal_cell_id <- defs[arch_rank == 1L, cell_id]
if (length(modal_cell_id) != 1L) stop("Expected exactly one arch_rank == 1 (modal) cell.")

message(sprintf("  Cell system from 06c (run %s): %d whitelist cells, tercile cuts %.1f / %.1f",
                defs$run_id[1L], length(whitelist), tercile_cuts[1], tercile_cuts[2]))
message(sprintf("  Modal archetype (A1): %s", modal_cell_id))

assign_tercile <- function(x) {
  cut(x, breaks = c(-Inf, tercile_cuts[1], tercile_cuts[2], Inf),
      labels = c("small", "medium", "large"), right = TRUE)
}
make_cell_id <- function(d) {
  paste(d$property_type, d$built_form, d$construction_age_band, d$main_fuel,
        as.character(assign_tercile(d$total_floor_area)), sep = " | ")
}

# Spec x core grid: invert spec_core_pairs so each core lists its valid specs
# (one matched RData load serves every spec of that core).
all_cores <- unique(unlist(spec_core_pairs))
specs_by_core <- setNames(lapply(all_cores, function(cr)
  names(spec_core_pairs)[vapply(spec_core_pairs, function(x) cr %in% x, logical(1L))]),
  all_cores)
spec_by_name <- setNames(spec_configs, vapply(spec_configs, `[[`, "", "name"))
n_combos <- sum(lengths(specs_by_core))
message(sprintf("  %d spec x core combos across %d cores.", n_combos, length(all_cores)))

# Difference-column name for each continuous covariate of a spec
d_col_map <- c(number_habitable_rooms = "D_rooms", total_floor_area = "D_floor",
               ppd_price_sqm = "D_ppd")


# 2. Load data (column-selected, lodgement level) ------------------------------
message("\n--- Loading data ---")
needed_cols <- unique(c(
  "uprn", "local_authority",
  "number_habitable_rooms", "total_floor_area", "lodgement_year",
  archetype_vars,
  "tax_band", "ppd_price_sqm", "ppd_year_transfer",   # non-Baseline exact/continuous vars
  "current_energy_efficiency",
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

# Stream per-LA and rbindlist (peak-RAM cap; see 06c for rationale).
# uprn is converted character -> numeric per part: UPRNs are digit-only,
# no leading zeros, <= 12 digits (verified 2026-07-12), exact in doubles.
# This avoids data.table's range_str character sort in setkey(dat, uprn),
# which exhausted memory at 22M rows (range_str realloc failure), and frees
# the interned-string cache as parts are converted.
arrow::set_cpu_count(1L)
dat_parts <- vector("list", length(all_parquet_files))
for (i in seq_along(all_parquet_files)) {
  part <- setDT(
    arrow::open_dataset(all_parquet_files[i], format = "parquet") |>
      select(all_of(available_cols)) |> collect())
  if (is.character(part$uprn)) {
    n_na_before <- sum(is.na(part$uprn))
    part[, uprn := as.numeric(uprn)]
    if (sum(is.na(part$uprn)) != n_na_before) {
      stop("uprn character->numeric conversion produced NAs in: ", all_parquet_files[i])
    }
  }
  dat_parts[[i]] <- part
  if (i %% 50L == 0L) gc()
}
dat <- rbindlist(dat_parts, use.names = TRUE, fill = TRUE)
rm(dat_parts, ds, part); gc()
message(sprintf("  Loaded %s rows x %d cols",
                formatC(nrow(dat), big.mark = ","), ncol(dat)))

dat <- define_treatments(dat)

# This run only uses the ladder treatments: drop the other treat_* columns
# (each is 8 bytes x 22M rows; define_treatments creates the full family).
treat_configs <- Filter(function(cfg) cfg$short_id %in% ladder_treats, treatment_metadata)
if (length(treat_configs) == 0L) stop("No treatments matched LADDER_TREATMENTS.")
keep_tvars <- vapply(treat_configs, `[[`, "", "var")
drop_tvars <- setdiff(grep("^treat_", names(dat), value = TRUE), keep_tvars)
if (length(drop_tvars) > 0L) dat[, (drop_tvars) := NULL]

# Regulatory-bound outcome (mirrors 06_run_regressions.R)
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

# Row prefilter: keep only lodgement rows whose uprn appears in at least one
# matched file this run will use. The fan-out join can never touch other rows,
# and dropping them cuts dat by roughly two thirds — decisive on a 16GB box
# where the joint i(cell_f) clustered vcov needs nobs x K scores matrices.
message("  Collecting matched uprns for the row prefilter...")
ups <- list()
for (config in treat_configs) {
  for (core in all_cores) {
    mf <- file.path(matched_data_dir,
                    paste0("matched_pairs_", config$file_id,
                           "_matching_core_", core, "_", CCOD_VERSION, ".RData"))
    if (!file.exists(mf)) next
    le <- new.env(parent = emptyenv())
    ok <- tryCatch({ load(mf, envir = le); TRUE }, error = function(e) FALSE)
    if (!ok) { rm(le); next }
    for (spn in specs_by_core[[core]]) {
      m <- le$matched_results[[spn]]
      if (is.null(m) || !("uprn" %in% names(m))) next
      u <- m$uprn
      if (is.character(u)) u <- as.numeric(u)
      ups[[length(ups) + 1L]] <- unique(u[!is.na(u)])
    }
    rm(le); gc()
  }
}
ups <- unique(unlist(ups, use.names = FALSE))
n_before <- nrow(dat)
dat <- dat[uprn %in% ups]
message(sprintf("  Row prefilter: %s -> %s lodgement rows (%s distinct matched uprns).",
                formatC(n_before, big.mark = ","), formatC(nrow(dat), big.mark = ","),
                formatC(length(ups), big.mark = ",")))
rm(ups); gc()

# Lodgement-row level retained for the fan-out join (see 06c)
setkey(dat, uprn)
gc()


# 3. Crash-resume --------------------------------------------------------------
# Completeness unit: (spec, matching_core, treatment) with all ladder outcomes.
done_keys <- character(0)
if (file.exists(ladder_path)) {
  existing <- tryCatch(fread(ladder_path, na.strings = c("NA", "")), error = function(e) data.table())
  if (nrow(existing) > 0L &&
      all(c("spec", "matching_core", "treatment_short_id", "outcome") %in% names(existing))) {
    cnt <- existing[, .(n_out = uniqueN(outcome)), by = .(spec, matching_core, treatment_short_id)]
    done <- cnt[n_out >= length(ladder_outcomes)]
    done_keys <- done[, paste(spec, matching_core, treatment_short_id, sep = "|")]
    partial <- cnt[n_out < length(ladder_outcomes)]
    if (nrow(partial) > 0L) {
      message(sprintf("  Crash-resume: purging %d partial spec x core x treatment groups.", nrow(partial)))
      existing[, .key := paste(spec, matching_core, treatment_short_id, sep = "|")]
      keep <- existing[.key %in% done_keys][, .key := NULL]
      fwrite(keep, ladder_path)
    }
    message(sprintf("  Crash-resume: %d spec x core x treatment groups already complete.", length(done_keys)))
  }
  rm(existing)
}


# 4. Pair-difference builder (spec x core generalisation of 06c's) -------------
# md: matched pairs (uprn + subclass) for one treatment under one spec x core.
# Selection of the exact-var-consistent lodgement combination follows 06c; the
# consistency vars and the continuous difference columns come from the spec.
build_pair_diffs <- function(config, spec, md) {
  md <- md[!is.na(uprn)]
  # Match dat's numeric uprn key (same verified-safe conversion as the load)
  if (is.character(md$uprn)) {
    n_na_before <- sum(is.na(md$uprn))
    md[, uprn := as.numeric(uprn)]
    if (sum(is.na(md$uprn)) != n_na_before) {
      log_error(config$short_id, "uprn",
                sprintf("[%s] uprn character->numeric produced NAs", spec$name))
      return(NULL)
    }
  }
  if ("weights" %in% names(md) && any(md$weights != 1)) {
    log_error(config$short_id, "weights",
              sprintf("[%s] non-unit weights (%d rows) — pair collapse invalid",
                      spec$name, sum(md$weights != 1)))
    return(NULL)
  }
  md <- unique(md[, .(uprn, subclass)], by = c("subclass", "uprn"))

  pr <- dat[md, on = "uprn", allow.cartesian = TRUE, nomatch = 0]
  pr[, tv := pr[[config$var]]]
  pr <- pr[!is.na(tv)]
  n_subclass_total <- uniqueN(pr$subclass)

  cons_vars <- setdiff(spec$exact_vars, "local_authority")
  pr[, .stratum := do.call(paste, c(.SD, sep = "\r")), .SDcols = cons_vars]
  both <- pr[, .(has_t = any(tv == 1L), has_c = any(tv == 0L)),
             by = .(subclass, .stratum)][has_t & has_c]
  if (nrow(both) == 0L) {
    log_error(config$short_id, "pairs",
              sprintf("[%s] no exact-var-consistent pairs after fan-out", spec$name))
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

  n_broken <- n_subclass_total - length(good)
  broken_share <- if (n_subclass_total > 0L) n_broken / n_subclass_total else NA_real_
  if (!is.na(broken_share) && broken_share > BROKEN_PAIR_WARN_SHARE) {
    message(sprintf("  NOTE [%s | %s]: %.1f%% of subclasses unrecoverable (%d of %d) — EPC-CCOD linkage gap.",
                    config$short_id, spec$name, broken_share * 100, n_broken, n_subclass_total))
  }
  if (length(good) == 0L) {
    log_error(config$short_id, "pairs",
              sprintf("[%s] no intact pairs after consistent selection", spec$name))
    return(NULL)
  }

  t_cols <- unique(c("subclass", ladder_outcomes, spec$continuous_vars))
  p_t <- p_t[, ..t_cols]
  setnames(p_t, setdiff(t_cols, "subclass"), paste0("t_", setdiff(t_cols, "subclass")))
  c_cols <- unique(c("subclass", "local_authority", ladder_outcomes,
                     spec$continuous_vars, archetype_vars, "total_floor_area"))
  c_cols <- intersect(c_cols, names(p_c))
  p_c <- p_c[, ..c_cols]

  pd <- merge(p_t, p_c, by = "subclass")
  for (oc in ladder_outcomes) {
    pd[, paste0("D_", oc) := get(paste0("t_", oc)) - get(oc)]
  }
  for (cv in spec$continuous_vars) {
    pd[, (d_col_map[[cv]]) := get(paste0("t_", cv)) - get(cv)]
  }

  # Cell assignment from the CONTROL member (exact vars shared within pair)
  pd[, cell_id := make_cell_id(.SD)]
  pd[, broken_share := broken_share]

  # Slim to estimation columns only: the t_* and raw covariate columns are
  # dead weight after the D_* differences are formed, and the joint i(cell_f)
  # clustered vcov needs a nobs x K scores matrix — every spare 100MB counts.
  keep_cols <- c("local_authority", paste0("D_", ladder_outcomes),
                 unname(d_col_map[spec$continuous_vars]), "cell_id", "broken_share")
  pd[, setdiff(names(pd), keep_cols) := NULL]
  pd
}


# 5. Main loop: treatment -> core (one RData load) -> spec -> outcome ----------
message("\n=== Running controls-ladder estimation ===")

for (config in treat_configs) {
  tsid <- config$short_id
  message(sprintf("\n[%s] %s", tsid, config$title))

  for (core in all_cores) {
    core_specs <- specs_by_core[[core]]
    todo_specs <- core_specs[!(paste(core_specs, core, tsid, sep = "|") %in% done_keys)]
    if (length(todo_specs) == 0L) {
      message(sprintf("  [core %s] SKIP (all specs complete)", core))
      next
    }

    matched_file <- file.path(matched_data_dir,
                              paste0("matched_pairs_", config$file_id,
                                     "_matching_core_", core,
                                     "_", CCOD_VERSION, ".RData"))
    if (!file.exists(matched_file)) {
      log_error(tsid, "load", paste0("matched file missing: ", basename(matched_file)))
      next
    }
    local_env <- new.env(parent = emptyenv())
    ok <- tryCatch({ load(matched_file, envir = local_env); TRUE },
                   error = function(e) { log_error(tsid, "load", conditionMessage(e)); FALSE })
    if (!ok) next

    for (spec_name in todo_specs) {
      spec <- spec_by_name[[spec_name]]
      t_combo <- Sys.time()
      md <- local_env$matched_results[[spec_name]]
      if (is.null(md) || nrow(md) == 0L) {
        log_error(tsid, "load", sprintf("[%s | core %s] no matched pairs in RData", spec_name, core))
        next
      }

      pd <- tryCatch(build_pair_diffs(config, spec, md),
                     error = function(e) {
                       log_error(tsid, sprintf("build_pair_diffs [%s|%s]", spec_name, core),
                                 conditionMessage(e)); NULL })
      rm(md)
      if (is.null(pd)) { gc(); next }

      d_ctrl <- unname(d_col_map[spec$continuous_vars])
      pair_cell_n <- pd[, .N, by = cell_id]
      keep_cells <- pair_cell_n[cell_id %in% whitelist & N >= HTE_MIN_PAIRS, cell_id]
      pd[, cell_est := fifelse(cell_id %in% keep_cells, cell_id, "_other")]

      rows_buffer <- list()
      for (oc in ladder_outcomes) {
        dy <- paste0("D_", oc)
        cc_expr <- paste(c(sprintf("!is.na(%s)", dy), sprintf("!is.na(%s)", d_ctrl)), collapse = " & ")
        est <- pd[eval(parse(text = cc_expr))]
        if (nrow(est) < 100L) {
          log_error(tsid, sprintf("outcome:%s [%s|%s]", oc, spec_name, core),
                    sprintf("only %d pairs with valid D_y", nrow(est)))
          next
        }

        # --- Pooled ATT (pair-diff form of PSM + Subclass FE for this spec) ---
        rhs <- paste(d_ctrl, collapse = " + ")
        att <- tryCatch(
          feols(as.formula(paste0(dy, " ~ 1 + ", rhs)),
                data = est, cluster = ~local_authority, lean = TRUE),
          error = function(e) {
            log_error(tsid, sprintf("att:%s [%s|%s]", oc, spec_name, core),
                      conditionMessage(e)); NULL })
        if (is.null(att)) next
        act <- coeftable(att)
        att_coef <- act["(Intercept)", "Estimate"]
        att_se   <- act["(Intercept)", "Std. Error"]
        att_p    <- act["(Intercept)", "Pr(>|t|)"]
        rm(att)

        # --- Cell CATEs ---
        est[, cell_f := factor(cell_est)]
        cm <- tryCatch(
          feols(as.formula(paste0(dy, " ~ 0 + i(cell_f) + ", rhs)),
                data = est, cluster = ~local_authority, lean = TRUE),
          error = function(e) {
            log_error(tsid, sprintf("cells:%s [%s|%s]", oc, spec_name, core),
                      conditionMessage(e)); NULL })
        if (is.null(cm)) next
        ct <- coeftable(cm)
        cell_rows_idx <- grep("^cell_f::", rownames(ct))
        cell_names <- sub("^cell_f::", "", rownames(ct)[cell_rows_idx])
        b <- ct[cell_rows_idx, "Estimate"]
        V <- tryCatch(vcov(cm)[cell_rows_idx, cell_rows_idx, drop = FALSE],
                      error = function(e) NULL)
        n_clust_total <- tryCatch(as.integer(fitstat(cm, "G", simplify = TRUE)),
                                  error = function(e) NA_integer_)
        if (is.null(V)) {
          rm(cm, ct)
          log_error(tsid, sprintf("vcov:%s [%s|%s]", oc, spec_name, core), "vcov extraction failed")
          next
        }

        cell_support <- est[, .(n_pairs = .N, n_las = uniqueN(local_authority)), by = cell_est]
        setkey(cell_support, cell_est)

        # --- Modal-archetype CATE (A1) ---
        m_idx <- match(modal_cell_id, cell_names)
        modal_beta <- modal_se <- modal_p <- NA_real_
        modal_n_pairs <- modal_n_las <- NA_integer_
        if (!is.na(m_idx)) {
          modal_beta <- b[m_idx]
          modal_se   <- ct[cell_rows_idx[m_idx], "Std. Error"]
          modal_p    <- ct[cell_rows_idx[m_idx], "Pr(>|t|)"]
          modal_n_pairs <- cell_support[modal_cell_id, n_pairs]
          modal_n_las   <- cell_support[modal_cell_id, n_las]
        }
        rm(cm, ct)

        # --- Reweighted ATT over estimated whitelist cells (fixed 06c weights) ---
        real_idx <- which(cell_names != "_other")
        tau_rw <- tau_rw_se <- cov_full <- NA_real_
        if (length(real_idx) > 0L) {
          w_raw <- unname(w_lookup[cell_names[real_idx]])
          cov_full <- sum(w_raw)
          w_norm <- w_raw / cov_full
          tau_rw <- sum(w_norm * b[real_idx])
          tau_rw_se <- sqrt(as.numeric(
            t(w_norm) %*% V[real_idx, real_idx, drop = FALSE] %*% w_norm))
        }

        rows_buffer[[oc]] <- data.table(
          treatment_short_id = tsid, treatment = config$title, outcome = oc,
          spec = spec_name, matching_core = core,
          att = att_coef, att_se = att_se, att_p = att_p,
          n_pairs = nrow(est), n_clusters = n_clust_total,
          n_cells_estimated = length(real_idx),
          modal_cell_id = modal_cell_id,
          modal_beta = modal_beta, modal_se = modal_se, modal_p = modal_p,
          modal_n_pairs = modal_n_pairs, modal_n_las = modal_n_las,
          modal_few_clusters_flag = as.integer(!is.na(modal_n_las) &&
                                                 modal_n_las < FEW_CLUSTERS_THRESHOLD),
          tau_rw_full = tau_rw, tau_rw_full_se = tau_rw_se,
          coverage_full = cov_full,
          composition_effect = att_coef - tau_rw,
          broken_share = pd$broken_share[1L],
          matching_geography = MATCHING_GEOGRAPHY, run_id = run_id
        )
        rm(est, V, b); gc()
      }  # end outcome loop

      # Write only complete (all-outcome) groups so crash-resume stays simple
      if (length(rows_buffer) == length(ladder_outcomes)) {
        out <- rbindlist(rows_buffer, fill = TRUE)
        fwrite(out, ladder_path, append = file.exists(ladder_path))
        done_keys <- c(done_keys, paste(spec_name, core, tsid, sep = "|"))
        message(sprintf("  [%s | %s | core %s] ATT %.4f | modal %.4f | rw %.4f (bad_epc_c; %s pairs; %.1f min)",
                        tsid, spec_name, core,
                        out[outcome == "bad_epc_c", att],
                        out[outcome == "bad_epc_c", modal_beta],
                        out[outcome == "bad_epc_c", tau_rw_full],
                        formatC(out[outcome == "bad_epc_c", n_pairs], big.mark = ","),
                        as.numeric(difftime(Sys.time(), t_combo, units = "mins"))))
        rm(out)
      } else {
        message(sprintf("  [%s | %s | core %s] incomplete (%d/%d outcomes) — not written",
                        tsid, spec_name, core, length(rows_buffer), length(ladder_outcomes)))
      }
      rm(pd, rows_buffer); gc()
    }  # end spec loop

    rm(local_env); gc()
  }  # end core loop
}

message(sprintf("\nDone in %.1f min. Output: %s",
                as.numeric(difftime(Sys.time(), start.time, units = "mins")),
                ladder_path))
