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
dat <- ds |> select(all_of(available_cols)) |> collect() |> as.data.table()
rm(ds); gc()
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

n_dup <- sum(duplicated(dat$uprn))
if (n_dup > 0L) {
  message(sprintf("  NOTE: dropping %s duplicate-UPRN rows.", formatC(n_dup, big.mark = ",")))
  dat <- dat[!duplicated(uprn)]
}
setkey(dat, uprn)

# Eligible ppd-core sample (Price Paid spec complete cases; mirrors 05)
elig <- dat[!is.na(ppd_price_sqm) & !is.infinite(ppd_price_sqm)]
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
rm(elig, desc_rows); gc()


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

  pr <- dat[md, on = "uprn", nomatch = 0]
  pr[, tv := get(config$var)]
  pr <- pr[!is.na(tv)]
  cnt <- pr[, .(n = .N, n_t = sum(tv == 1L)), by = subclass]
  good <- cnt[n == 2L & n_t == 1L, subclass]
  n_broken <- nrow(cnt) - length(good)
  pr <- pr[subclass %in% good]
  if (nrow(pr) == 0L) return(NULL)

  pr[, ts := as.integer(as.character(lodgement_year)) - as.integer(as.character(ppd_year_transfer))]

  p_t <- pr[tv == 1L, c(list(subclass = subclass, ts_t = ts, dist_t = distance),
                        mget(tv_outcomes))]
  setnames(p_t, tv_outcomes, paste0("t_", tv_outcomes))
  p_c <- pr[tv == 0L, c(list(subclass = subclass, local_authority = local_authority,
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

end.time <- Sys.time()
message(sprintf("\n06d_transaction_vintage.R complete [%s]. Runtime: %.1f min.",
                MATCHING_GEOGRAPHY,
                as.numeric(difftime(end.time, start.time, units = "mins"))))
