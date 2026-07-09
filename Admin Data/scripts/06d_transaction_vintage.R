# Script: 06d_transaction_vintage.R
# Purpose: Transaction-vintage gradient (PI feedback, workstream 3): does the
#          ownership efficiency gradient fade with time since the property
#          last transacted? The "Price Paid" spec exact-matches on BOTH
#          lodgement_year and ppd_year_transfer, so time-since-transaction
#          ts = lodgement_year - ppd_year_transfer is constant within each
#          matched pair and the gradient collapses to a clean pair-difference
#          regression: D_y ~ i(ts_bin).
#
#          Part A: descriptive treated/control means by ts bin on the eligible
#                  ppd-core sample. Part B: pair-difference gradient on the
#                  Price Paid matched pairs (levels + differences vs the 0-1
#                  bin; optional PS<=0.2 pair caliper). Part C: exhibit plot +
#                  a small LaTeX table.
#
# CAVEAT (printed on every exhibit): ppd_year_transfer is the property's most
# recent observed sale, and selection into transaction timing is not random;
# the gradient is a descriptive shape, not identification of retrofit
# dynamics. Sales dated after the EPC lodgement (negative ts) are binned as
# sale_after_epc, excluded from the gradient, and counted.
#
# Scope: LA geography; treatments fp, frfp, th, thfp, np; outcomes
# current_energy_efficiency, bad_epc_c, energy_consumption_current.
# Orchestrated by run_analysis.R (RUN_TXN_VINTAGE flag, Phase 4c) or standalone.
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
library(ggplot2)

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "model_specifications.R"))
source(here::here("scripts", "treatment_definitions.R"))

setFixest_notes(FALSE)
setFixest_nthreads(2)

if (WITHIN_CORPORATE) {
  stop("06d_transaction_vintage.R does not support WITHIN_CORPORATE mode.")
}

# Configuration ----------------------------------------------------------------
TV_SPEC_NAME     <- "Price Paid"
TV_MATCHING_CORE <- "ppd"
tv_treatments <- c("fp", "frfp", "th", "thfp", "np")
tv_outcomes   <- c("current_energy_efficiency", "bad_epc_c", "energy_consumption_current")
TS_REF_BIN    <- "0-1"

base_matching_vars <- c("number_habitable_rooms", "total_floor_area", "lodgement_year",
                        "property_type", "main_fuel", "construction_age_band", "built_form")

ts_bin_levels <- c("0-1", "2-4", "5-9", "10-14", "15+")
assign_ts_bin <- function(ts) {
  nna <- !is.na(ts)
  fcase(
    !nna, NA_character_,
    nna & ts < 0, "sale_after_epc",
    nna & ts <= 1, "0-1",
    nna & ts <= 4, "2-4",
    nna & ts <= 9, "5-9",
    nna & ts <= 14, "10-14",
    default = "15+"
  )
}

matched_data_dir <- file.path(MATCHED_DATA_DIR, MATCHING_GEOGRAPHY)
summary_dir      <- SUMMARY_TABLES_DIR

descriptive_path <- file.path(summary_dir, paste0("txn_vintage_descriptive_", MATCHING_GEOGRAPHY, ".csv"))
gradient_path    <- file.path(summary_dir, paste0("txn_vintage_gradient_", MATCHING_GEOGRAPHY, ".csv"))
figure_path      <- file.path(FIGURES_DIR, paste0("txn_vintage_penalty_", MATCHING_GEOGRAPHY, ".pdf"))

message("===================================================================")
message("  06d_transaction_vintage.R - Transaction-Vintage Gradient")
message(sprintf("  Geography: %s | Spec: %s | Core: %s",
                MATCHING_GEOGRAPHY, TV_SPEC_NAME, TV_MATCHING_CORE))
message(sprintf("  Started: %s", format(start.time, "%Y-%m-%d %H:%M:%S")))
message("===================================================================")

treat_meta_by_short <- setNames(treatment_metadata,
                                vapply(treatment_metadata, `[[`, "", "short_id"))


# 1. Load data -------------------------------------------------------------------
message("\n--- Loading data ---")
needed_cols <- unique(c(
  "uprn", "local_authority",
  base_matching_vars, "ppd_price_sqm", "ppd_year_transfer",
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
# Stream per-LA and rbindlist to cap the load peak near the final table size
# (a single national collect() holds arrow's Table + the R copy at once and OOMs).
arrow::set_cpu_count(1L)
dat_parts <- vector("list", length(all_parquet_files))
for (i in seq_along(all_parquet_files)) {
  dat_parts[[i]] <- setDT(
    arrow::open_dataset(all_parquet_files[i], format = "parquet") |>
      select(all_of(available_cols)) |> collect())
}
dat <- rbindlist(dat_parts, use.names = TRUE, fill = TRUE)
rm(dat_parts, ds); gc()
message(sprintf("  Loaded %s rows x %d cols", formatC(nrow(dat), big.mark = ","), ncol(dat)))

dat <- define_treatments(dat)
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

# Keep `dat` at EPC-lodgement-row level (do NOT dedupe by UPRN): the Part B pair
# join fans out over lodgement rows and selects the exact-var-consistent match
# (05 matched at lodgement level; the saved objects carry only `uprn`). A
# uprn-deduplicated view feeds the Part A property-level descriptive.
n_dup <- sum(duplicated(dat$uprn))
message(sprintf("  Retaining %s lodgement-level rows (%s duplicate-UPRN rows kept for the fan-out join).",
                formatC(nrow(dat), big.mark = ","), formatC(n_dup, big.mark = ",")))
setkey(dat, uprn)

# Eligible ppd-core sample (Price Paid spec complete cases; mirrors 05),
# property-level for the descriptive.
elig <- unique(dat[!is.na(ppd_price_sqm) & !is.infinite(ppd_price_sqm)], by = "uprn")
elig <- elig[complete.cases(elig[, ..base_matching_vars])]
elig <- elig[!is.na(ppd_year_transfer)]
elig[, ts := as.integer(as.character(lodgement_year)) - as.integer(as.character(ppd_year_transfer))]
elig[, ts_bin := assign_ts_bin(ts)]

share_neg <- elig[, mean(ts_bin == "sale_after_epc", na.rm = TRUE)]
message(sprintf("  Eligible ppd-core sample: %s rows; sale-after-EPC (negative ts): %.1f%%",
                formatC(nrow(elig), big.mark = ","), share_neg * 100))
gc()


# 2. Part A: descriptive means by ts bin ------------------------------------------
message("\n--- Part A: descriptive gradient (eligible ppd-core sample) ---")
desc_rows <- list()
for (tsid in tv_treatments) {
  tvar <- treat_meta_by_short[[tsid]]$var
  sub <- elig[!is.na(get(tvar)) & !is.na(ts_bin)]
  for (oc in tv_outcomes) {
    d <- sub[!is.na(get(oc)),
             .(n = .N, mean_y = mean(get(oc))),
             by = .(treated = get(tvar), ts_bin)]
    w <- dcast(d, ts_bin ~ treated, value.var = c("n", "mean_y"))
    setnames(w, c("n_0", "n_1", "mean_y_0", "mean_y_1"),
             c("n_control", "n_treated", "mean_control", "mean_treated"),
             skip_absent = TRUE)
    for (cn in c("n_control", "n_treated", "mean_control", "mean_treated")) {
      if (!cn %in% names(w)) w[, (cn) := NA]
    }
    w[, raw_gap := mean_treated - mean_control]
    w[, `:=`(treatment_short_id = tsid, outcome = oc)]
    desc_rows[[paste(tsid, oc)]] <- w
  }
}
desc_dt <- rbindlist(desc_rows, fill = TRUE)
desc_dt[, bin_idx := match(ts_bin, c(ts_bin_levels, "sale_after_epc"))]
setorder(desc_dt, treatment_short_id, outcome, bin_idx)
desc_dt[, bin_idx := NULL]
desc_dt[, `:=`(matching_geography = MATCHING_GEOGRAPHY, run_id = run_id)]
fwrite(desc_dt, descriptive_path)
message(sprintf("  Descriptive written: %s (%d rows)", basename(descriptive_path), nrow(desc_dt)))
rm(desc_rows); gc()


# 2b. Part D: simple (unmatched) ownership penalty by ts bin (R2) -----------------
# A regression form of the Part A means gap, with SEs, on the eligible ppd-core
# sample (NO matching). Two estimators:
#   raw      = i(ts_bin, treat) + ts_bin main effects          (per-bin penalty)
#   adjusted = raw + property covariates                        (composition-adjusted)
# The four categorical covariates enter as fixed effects (sparse demeaning;
# numerically identical to including their main-effect dummies on the RHS but
# memory-safe while `dat` is still loaded), the two continuous covariates as
# regressors. elig is already complete on all six, so raw and adjusted share the
# same sample. This is the "estimator ladder" companion to the matched
# pair-difference gradient (Part B): raw > adjusted > matched, typically.
message("\n--- Part D: simple (unmatched) vintage penalty ---")
simple_path <- file.path(summary_dir, paste0("txn_vintage_simple_", MATCHING_GEOGRAPHY, ".csv"))
adj_fe_covars   <- c("property_type", "built_form", "construction_age_band", "main_fuel")
adj_cont_covars <- c("total_floor_area", "number_habitable_rooms")
simple_rows <- list()
for (tsid in tv_treatments) {
  tvar <- treat_meta_by_short[[tsid]]$var
  sub <- elig[!is.na(get(tvar)) & ts_bin %in% ts_bin_levels]
  if (nrow(sub) < 100L) next
  sub[, ts_bin_f := factor(ts_bin, levels = ts_bin_levels)]
  sub[, treat_var := as.integer(get(tvar))]
  for (oc in tv_outcomes) {
    est <- sub[!is.na(get(oc))]
    if (nrow(est) < 100L || uniqueN(est$ts_bin_f) < 2L) next
    for (estimator in c("raw", "adjusted")) {
      fml <- if (estimator == "raw") {
        as.formula(paste0(oc, " ~ i(ts_bin_f, treat_var) + ts_bin_f"))
      } else {
        as.formula(paste0(oc, " ~ i(ts_bin_f, treat_var) + ts_bin_f + ",
                          paste(adj_cont_covars, collapse = " + "), " | ",
                          paste(adj_fe_covars, collapse = " + ")))
      }
      m <- tryCatch(feols(fml, data = est, cluster = ~local_authority, lean = TRUE),
                    error = function(e) { message(sprintf("  [%s|%s|%s] feols error: %s",
                                                          tsid, oc, estimator, conditionMessage(e))); NULL })
      if (is.null(m)) next
      ct <- coeftable(m)
      idx <- grep("^ts_bin_f::.*:treat_var$", rownames(ct))
      if (length(idx) == 0L) next
      bins <- sub("^ts_bin_f::(.*):treat_var$", "\\1", rownames(ct)[idx])
      bin_n <- est[, .N, by = ts_bin]; setkey(bin_n, ts_bin)
      simple_rows[[paste(tsid, oc, estimator)]] <- data.table(
        treatment_short_id = tsid, treatment = treat_meta_by_short[[tsid]]$title,
        outcome = oc, estimator = estimator, ts_bin = bins,
        penalty = ct[idx, "Estimate"], se = ct[idx, "Std. Error"],
        p_value = ct[idx, "Pr(>|t|)"],
        ci_lower = ct[idx, "Estimate"] - 1.96 * ct[idx, "Std. Error"],
        ci_upper = ct[idx, "Estimate"] + 1.96 * ct[idx, "Std. Error"],
        n = bin_n[bins, N], n_total = nrow(est),
        matching_geography = MATCHING_GEOGRAPHY, run_id = run_id)
      rm(m)
    }
  }
  rm(sub); gc()
}
simple_dt <- rbindlist(Filter(Negate(is.null), simple_rows), fill = TRUE)
if (nrow(simple_dt) > 0L) {
  simple_dt[, .ord := match(ts_bin, ts_bin_levels)]
  setorder(simple_dt, treatment_short_id, outcome, estimator, .ord)
  simple_dt[, .ord := NULL]
  fwrite(simple_dt, simple_path)
  message(sprintf("  Simple penalty written: %s (%d rows)", basename(simple_path), nrow(simple_dt)))
} else {
  message("  No simple-penalty rows produced.")
}
rm(elig, simple_rows); gc()


# 3. Part B: pair-difference gradient ----------------------------------------------
message("\n--- Part B: pair-difference gradient (Price Paid matched pairs) ---")

# Build pair differences for one treatment on the Price Paid spec / ppd core.
# Same construction as 06c: intact 1 treated + 1 control subclasses, unit weights.
build_pair_diffs_ppd <- function(config) {
  matched_file <- file.path(matched_data_dir,
                            paste0("matched_pairs_", config$file_id,
                                   "_matching_core_", TV_MATCHING_CORE,
                                   "_", CCOD_VERSION, ".RData"))
  if (!file.exists(matched_file)) {
    message(sprintf("  [%s] SKIP: %s not found", config$short_id, basename(matched_file)))
    return(NULL)
  }
  local_env <- new.env(parent = emptyenv())
  ok <- tryCatch({ load(matched_file, envir = local_env); TRUE }, error = function(e) FALSE)
  if (!ok) return(NULL)
  md <- local_env$matched_results[[TV_SPEC_NAME]]
  rm(local_env)
  if (is.null(md) || nrow(md) == 0L) {
    message(sprintf("  [%s] SKIP: no '%s' matched pairs", config$short_id, TV_SPEC_NAME))
    return(NULL)
  }
  md <- md[!is.na(uprn)]
  stopifnot("1:1 matching violated: non-unit weights" = all(md$weights == 1))
  md <- unique(md[, .(uprn, subclass, distance)], by = c("subclass", "uprn"))

  # Fan-out join over all lodgement rows, then exact-var-consistent selection
  # (see 06c). The Price Paid exact vars include lodgement_year AND
  # ppd_year_transfer, so ts is identical within the selected pair by construction.
  pr <- dat[md, on = "uprn", allow.cartesian = TRUE, nomatch = 0]
  pr[, tv := pr[[config$var]]]
  pr <- pr[!is.na(tv)]
  n_subclass_total <- uniqueN(pr$subclass)

  cons_vars <- setdiff(Filter(function(s) identical(s$name, TV_SPEC_NAME),
                              spec_configs)[[1L]]$exact_vars, "local_authority")
  pr[, .stratum := do.call(paste, c(.SD, sep = "\r")), .SDcols = cons_vars]
  both <- pr[, .(has_t = any(tv == 1L), has_c = any(tv == 0L)),
             by = .(subclass, .stratum)][has_t & has_c]
  if (nrow(both) == 0L) return(NULL)
  setorder(both, subclass, .stratum)
  chosen <- both[, .(.stratum = .stratum[1L]), by = subclass]
  keep <- pr[chosen, on = .(subclass, .stratum), nomatch = 0]
  s_t <- keep[tv == 1L]; s_t <- s_t[s_t[, .I[1L], by = subclass]$V1]
  s_c <- keep[tv == 0L]; s_c <- s_c[s_c[, .I[1L], by = subclass]$V1]
  good <- intersect(s_t$subclass, s_c$subclass)
  s_t <- s_t[subclass %in% good]; s_c <- s_c[subclass %in% good]
  n_broken <- n_subclass_total - length(good)
  if (length(good) == 0L) return(NULL)

  s_t[, ts := as.integer(as.character(lodgement_year)) - as.integer(as.character(ppd_year_transfer))]
  s_c[, ts := as.integer(as.character(lodgement_year)) - as.integer(as.character(ppd_year_transfer))]

  p_t <- s_t[, c(list(subclass = subclass, ts_t = ts, dist_t = distance),
                 mget(tv_outcomes))]
  setnames(p_t, tv_outcomes, paste0("t_", tv_outcomes))
  p_c <- s_c[, c(list(subclass = subclass, local_authority = local_authority,
                      ts_c = ts, dist_c = distance),
                 mget(tv_outcomes))]
  pd <- merge(p_t, p_c, by = "subclass")

  # Integrity: ts must be identical within pair (exact match on both years)
  ts_mismatch <- pd[!is.na(ts_t) & !is.na(ts_c) & ts_t != ts_c, .N]
  if (ts_mismatch > 0L) {
    stop(sprintf("[%s] %d pairs with within-pair ts mismatch — exact matching broken",
                 config$short_id, ts_mismatch))
  }
  pd[, ts := ts_c]
  pd[, ts_bin := assign_ts_bin(ts)]
  pd[, pair_max_dist := pmax(dist_t, dist_c)]
  for (oc in tv_outcomes) pd[, paste0("D_", oc) := get(paste0("t_", oc)) - get(oc)]
  pd[, n_broken_pairs := n_broken]
  pd
}

grad_rows <- list()
plot_rows <- list()

for (tsid in tv_treatments) {
  config <- treat_meta_by_short[[tsid]]
  message(sprintf("\n[%s] %s", tsid, config$title))
  pd <- tryCatch(build_pair_diffs_ppd(config),
                 error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
  if (is.null(pd)) next

  n_pairs_all <- nrow(pd)
  n_neg <- pd[ts_bin == "sale_after_epc", .N]
  message(sprintf("  %s intact pairs; %d sale-after-EPC pairs excluded from gradient (%.2f%%); %d broken subclasses dropped",
                  formatC(n_pairs_all, big.mark = ","), n_neg,
                  100 * n_neg / max(n_pairs_all, 1L), pd$n_broken_pairs[1L]))

  pd_grad <- pd[ts_bin %in% ts_bin_levels]
  pd_grad[, ts_bin_f := factor(ts_bin, levels = ts_bin_levels)]

  for (caliper in c("none", "ps<=0.2")) {
    est_all <- if (caliper == "none") pd_grad else pd_grad[pair_max_dist <= 0.2]
    for (oc in tv_outcomes) {
      dy <- paste0("D_", oc)
      est <- est_all[!is.na(get(dy))]
      if (nrow(est) < 100L || uniqueN(est$ts_bin_f) < 2L) next

      bin_support <- est[, .(n_pairs = .N, n_las = uniqueN(local_authority)), by = ts_bin]
      setkey(bin_support, ts_bin)

      # Levels: mean pair difference (penalty) per ts bin
      m_lvl <- tryCatch(
        feols(as.formula(paste0(dy, " ~ 0 + i(ts_bin_f)")),
              data = est, cluster = ~local_authority, lean = TRUE),
        error = function(e) NULL)
      # Differences vs the 0-1 reference bin
      m_dif <- tryCatch(
        feols(as.formula(paste0(dy, " ~ i(ts_bin_f, ref = '", TS_REF_BIN, "')")),
              data = est, cluster = ~local_authority, lean = TRUE),
        error = function(e) NULL)

      extract_bins <- function(m, model_lab) {
        if (is.null(m)) return(NULL)
        ct <- coeftable(m)
        idx <- grep("^ts_bin_f::", rownames(ct))
        if (length(idx) == 0L) return(NULL)
        bins <- sub("^ts_bin_f::", "", rownames(ct)[idx])
        data.table(
          treatment_short_id = tsid, treatment = config$title, outcome = oc,
          model = model_lab, caliper = caliper, ts_bin = bins,
          estimate = ct[idx, "Estimate"], se = ct[idx, "Std. Error"],
          p_value = ct[idx, "Pr(>|t|)"],
          ci_lower = ct[idx, "Estimate"] - 1.96 * ct[idx, "Std. Error"],
          ci_upper = ct[idx, "Estimate"] + 1.96 * ct[idx, "Std. Error"],
          n_pairs = bin_support[bins, n_pairs],
          n_clusters_bin = bin_support[bins, n_las],
          n_clusters_total = tryCatch(as.integer(fitstat(m, "G", simplify = TRUE)),
                                      error = function(e) NA_integer_),
          n_pairs_total = nrow(est),
          n_pairs_sale_after_epc = n_neg,
          share_sale_after_epc = n_neg / max(n_pairs_all, 1L),
          matching_geography = MATCHING_GEOGRAPHY, run_id = run_id
        )
      }

      lvl_rows <- extract_bins(m_lvl, "level")
      dif_rows <- extract_bins(m_dif, "diff_vs_ref")
      grad_rows[[paste(tsid, oc, caliper)]] <- rbindlist(
        list(lvl_rows, dif_rows), fill = TRUE)
      if (caliper == "none" && !is.null(lvl_rows)) {
        plot_rows[[paste(tsid, oc)]] <- lvl_rows
      }
      rm(m_lvl, m_dif)
    }
  }
  rm(pd, pd_grad); gc()
}

grad_dt <- rbindlist(Filter(Negate(is.null), grad_rows), fill = TRUE)
if (nrow(grad_dt) == 0L) stop("No gradient estimates produced — check matched-pair inputs.")
fwrite(grad_dt, gradient_path)
message(sprintf("\n  Gradient written: %s (%d rows)", basename(gradient_path), nrow(grad_dt)))

# Validation: bin 0-1 level should carry the mainline ppd-core PSM sign
results_path <- file.path(summary_dir, paste0("results_table_", MATCHING_GEOGRAPHY, ".csv"))
if (file.exists(results_path)) {
  res <- tryCatch(fread(results_path, na.strings = c("NA", "")), error = function(e) NULL)
  if (!is.null(res)) {
    ref <- res[model == "PSM (Matched) + Subclass FE" & spec == TV_SPEC_NAME &
                 matching_core == TV_MATCHING_CORE & regression_core == TV_MATCHING_CORE &
                 outcome == "current_energy_efficiency" & status == "ok",
               .(treatment_short_id, ref_coef = coef)]
    ref <- unique(ref, by = "treatment_short_id")
    chk <- merge(grad_dt[model == "level" & caliper == "none" & ts_bin == TS_REF_BIN &
                           outcome == "current_energy_efficiency",
                         .(treatment_short_id, estimate)],
                 ref, by = "treatment_short_id")
    for (i in seq_len(nrow(chk))) {
      r <- chk[i]
      agree <- sign(r$estimate) == sign(r$ref_coef)
      message(sprintf("  Validation [%s]: bin %s level = %.4f | mainline ppd PSM = %.4f | sign %s",
                      r$treatment_short_id, TS_REF_BIN, r$estimate, r$ref_coef,
                      if (agree) "AGREES" else "DIFFERS (note: mainline pools all bins)"))
    }
  }
}


# 4. Part C: exhibit plot + LaTeX table ---------------------------------------------
message("\n--- Part C: exhibits ---")
plot_dt <- rbindlist(Filter(Negate(is.null), plot_rows), fill = TRUE)
if (nrow(plot_dt) > 0L) {
  outcome_labs <- c(current_energy_efficiency = "EPC score (SAP points)",
                    bad_epc_c = "Below EPC C (share)",
                    energy_consumption_current = "Energy consumption (kWh/sqm)")
  treat_labs <- c(fp = "For-profit", frfp = "Foreign for-profit",
                  th = "Tax haven", thfp = "Tax-haven for-profit", np = "Non-profit")
  plot_dt[, ts_bin_f := factor(ts_bin, levels = ts_bin_levels)]
  plot_dt[, outcome_lab := outcome_labs[outcome]]
  plot_dt[, treat_lab := treat_labs[treatment_short_id]]

  p <- ggplot(plot_dt, aes(x = ts_bin_f, y = estimate, colour = treat_lab, group = treat_lab)) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treat_lab),
                alpha = 0.12, colour = NA) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.6) +
    facet_wrap(~ outcome_lab, scales = "free_y", ncol = 1) +
    labs(
      title = "Ownership penalty by time since last transaction",
      subtitle = "Matched-pair differences (Price Paid spec, ppd core); 95% CI, LA-clustered",
      x = "Years between last sale and EPC lodgement",
      y = "Treated - control pair difference",
      colour = NULL, fill = NULL,
      caption = paste("Descriptive gradient: selection into transaction timing is not random;",
                      "not identification of retrofit dynamics. Sales postdating the EPC excluded.")
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")

  ggsave(figure_path, p, width = 8, height = 9)
  message(sprintf("  Figure written: %s", figure_path))
} else {
  message("  No plot rows — figure skipped.")
}

# Small LaTeX table: EPC score levels per bin x treatment
tex_tab <- grad_dt[model == "level" & caliper == "none" &
                     outcome == "current_energy_efficiency"]
if (nrow(tex_tab) > 0L) {
  fmt_c <- function(x) ifelse(is.na(x), "--",
                              ifelse(x > 0, sprintf("+%.2f", x), sprintf("%.2f", x)))
  stars <- function(p) ifelse(is.na(p), "",
                              ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                                     ifelse(p < 0.05, "*", ""))))
  tt <- c("\\begin{table}[htbp]", "\\centering",
          "\\caption{EPC-Score Penalty by Time Since Last Transaction}",
          "\\label{tab:txn_vintage}", "\\small",
          paste0("\\begin{tabular}{l", paste(rep("r", length(tv_treatments)), collapse = ""), "}"),
          "\\toprule",
          paste0("Years since sale & ", paste(c("For-profit", "Foreign FP", "Tax haven",
                                                "TH for-profit", "Non-profit"),
                                              collapse = " & "), " \\\\"),
          "\\midrule")
  for (b in ts_bin_levels) {
    cells <- vapply(tv_treatments, function(t) {
      r <- tex_tab[treatment_short_id == t & ts_bin == b]
      if (nrow(r) == 0L) "--" else paste0(fmt_c(r$estimate[1L]), stars(r$p_value[1L]),
                                          " (", sprintf("%.2f", r$se[1L]), ")")
    }, character(1L))
    tt <- c(tt, paste0(b, " & ", paste(cells, collapse = " & "), " \\\\"))
  }
  tt <- c(tt, "\\bottomrule", "\\end{tabular}",
          "\\begin{minipage}{0.95\\textwidth}", "\\vspace{4pt}", "\\footnotesize",
          "\\textit{Notes:} Mean matched-pair difference in EPC score (treated $-$ control)",
          "by years between the property's most recent sale and its EPC lodgement.",
          "Price Paid specification, ppd matching core: pairs exact-match on both",
          "\\texttt{lodgement\\_year} and \\texttt{ppd\\_year\\_transfer}, so the vintage is",
          "constant within pair. LA-clustered SEs in parentheses.",
          "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
          "\\textbf{Descriptive:} selection into transaction timing is not random; this is a",
          "gradient shape, not identification of retrofit dynamics. Sales postdating the EPC",
          "are excluded (share reported in \\texttt{txn\\_vintage\\_gradient\\_LA.csv}).",
          "Source: script 06d.",
          "\\end{minipage}", "\\end{table}")
  writeLines(tt, here::here("tables", "txn_vintage_gradient.tex"))
  message("  Written: tables/txn_vintage_gradient.tex")
}


# 5. Estimator ladder: simple (raw/adjusted) vs matched overlay (R2) ---------------
message("\n--- Estimator-ladder overlay (simple vs matched) ---")
if (exists("simple_dt") && nrow(simple_dt) > 0L && exists("plot_dt") && nrow(plot_dt) > 0L) {
  outcome_labs_o <- c(current_energy_efficiency = "EPC score (SAP points)",
                      bad_epc_c = "Below EPC C (share)",
                      energy_consumption_current = "Energy consumption (kWh/sqm)")
  treat_labs_o <- c(fp = "For-profit", frfp = "Foreign for-profit",
                    th = "Tax haven", thfp = "Tax-haven for-profit", np = "Non-profit")

  matched_lvl <- plot_dt[, .(treatment_short_id, outcome, ts_bin,
                             penalty = estimate, ci_lower, ci_upper)]
  matched_lvl[, estimator := "matched"]
  simple_lvl <- simple_dt[, .(treatment_short_id, outcome, ts_bin,
                              penalty, ci_lower, ci_upper, estimator)]
  overlay <- rbind(simple_lvl, matched_lvl)
  overlay <- overlay[outcome %in% tv_outcomes & ts_bin %in% ts_bin_levels]
  overlay[, ts_bin_f := factor(ts_bin, levels = ts_bin_levels)]
  overlay[, estimator_lab := factor(estimator, levels = c("raw", "adjusted", "matched"),
                                    labels = c("Simple (raw)", "Simple (adjusted)",
                                               "Matched (pair-diff)"))]
  overlay[, outcome_lab := factor(outcome_labs_o[outcome],
                                  levels = outcome_labs_o[tv_outcomes])]
  overlay[, treat_lab := factor(treat_labs_o[treatment_short_id],
                                levels = treat_labs_o[tv_treatments])]

  # facet_wrap (not facet_grid) so EACH panel gets its own free y-scale: the
  # three outcomes have incompatible units (SAP points vs a 0-1 share vs
  # kWh/sqm) and must not share an axis. ncol = n_outcomes reproduces the
  # treatment x outcome grid layout (treatment-major panel order).
  p2 <- ggplot(overlay, aes(x = ts_bin_f, y = penalty,
                            colour = estimator_lab, group = estimator_lab)) +
    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = estimator_lab),
                alpha = 0.10, colour = NA) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.4) +
    facet_wrap(vars(treat_lab, outcome_lab), scales = "free_y",
               ncol = length(tv_outcomes), labeller = label_wrap_gen(width = 24)) +
    scale_colour_manual(values = c("Simple (raw)" = "#d95f02",
                                   "Simple (adjusted)" = "#7570b3",
                                   "Matched (pair-diff)" = "#1b9e77")) +
    scale_fill_manual(values = c("Simple (raw)" = "#d95f02",
                                 "Simple (adjusted)" = "#7570b3",
                                 "Matched (pair-diff)" = "#1b9e77")) +
    labs(
      title = "Ownership penalty by time since last transaction: estimator ladder",
      subtitle = "Unmatched raw and covariate-adjusted vs matched pair-difference (ppd core); 95% CI, LA-clustered; free y-scale per panel",
      x = "Years between last sale and EPC lodgement",
      y = "Ownership penalty (treated - control)",
      colour = NULL, fill = NULL,
      caption = paste("Raw = unmatched per-bin gap; adjusted = + property covariates;",
                      "matched = pair difference. Descriptive: not identification of retrofit dynamics.")
    ) +
    theme_minimal(base_size = 8.5) +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 7, lineheight = 0.9))

  figure_simple_path <- file.path(FIGURES_DIR, paste0("txn_vintage_simple_", MATCHING_GEOGRAPHY, ".pdf"))
  ggsave(figure_simple_path, p2, width = 10, height = 12, limitsize = FALSE)
  message(sprintf("  Figure written: %s", figure_simple_path))

  # LaTeX table: estimator ladder for EPC score, grouped by treatment
  lad <- overlay[outcome == "current_energy_efficiency"]
  if (nrow(lad) > 0L) {
    fmt_c <- function(x) ifelse(is.na(x), "--",
                                ifelse(x > 0, sprintf("+%.2f", x), sprintf("%.2f", x)))
    tt2 <- c("\\begin{table}[htbp]", "\\centering",
             "\\caption{Estimator Ladder: EPC-Score Ownership Penalty by Transaction Vintage}",
             "\\label{tab:txn_vintage_simple}", "\\small",
             "\\begin{tabular}{llrrr}",
             "\\toprule",
             "Ownership & Years since sale & Simple (raw) & Simple (adj.) & Matched \\\\",
             "\\midrule")
    for (tsid in tv_treatments) {
      sub <- lad[treatment_short_id == tsid]
      if (nrow(sub) == 0L) next
      lab <- treat_labs_o[tsid]
      first <- TRUE
      for (b in ts_bin_levels) {
        rr <- sub[ts_bin == b]
        raw_v <- rr[estimator == "raw", penalty]
        adj_v <- rr[estimator == "adjusted", penalty]
        mat_v <- rr[estimator == "matched", penalty]
        gv <- function(v) if (length(v) == 0L) NA_real_ else v[1L]
        tt2 <- c(tt2, sprintf("%s & %s & %s & %s & %s \\\\",
                              if (first) lab else "", b,
                              fmt_c(gv(raw_v)), fmt_c(gv(adj_v)), fmt_c(gv(mat_v))))
        first <- FALSE
      }
      tt2 <- c(tt2, "\\midrule")
    }
    tt2 <- c(tt2[-length(tt2)], "\\bottomrule", "\\end{tabular}",
             "\\begin{minipage}{0.95\\textwidth}", "\\vspace{4pt}", "\\footnotesize",
             "\\textit{Notes:} Ownership penalty in EPC score (treated $-$ control) by years",
             "between the property's most recent sale and its EPC lodgement. \\emph{Simple (raw)}:",
             "unmatched per-bin gap from $y \\sim \\mathrm{i}(\\text{bin},\\text{owner}) + \\text{bin}$.",
             "\\emph{Simple (adjusted)}: same, plus property-type, built-form, age-band, main-fuel",
             "fixed effects and floor area / habitable rooms (unmatched but composition-adjusted).",
             "\\emph{Matched}: mean pair difference (Price Paid spec, ppd core; exact-matched on",
             "vintage). Raw includes composition by vintage; matched strips it; adjusted sits",
             "between. LA-clustered SEs. \\textbf{Descriptive:} selection into transaction timing is",
             "not random. Source: script 06d.",
             "\\end{minipage}", "\\end{table}")
    writeLines(tt2, here::here("tables", "txn_vintage_simple.tex"))
    message("  Written: tables/txn_vintage_simple.tex")
  }
} else {
  message("  Estimator-ladder overlay skipped (need both simple_dt and plot_dt).")
}

end.time <- Sys.time()
message(sprintf("\n06d_transaction_vintage.R complete [%s]. Runtime: %.1f min.",
                MATCHING_GEOGRAPHY,
                as.numeric(difftime(end.time, start.time, units = "mins"))))
