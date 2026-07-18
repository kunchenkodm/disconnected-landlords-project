# Script: generate_results_tables_pilot.R
# Purpose: LaTeX exhibits for the estimability-gate pilot (see
#          06c_heterogeneous_effects_pilot.R). Writes two fragments to tables/,
#          \input by progress_pack_pilot.tex:
#   Table P1 (hte_pilot_contribution.tex) - two-archetype contribution:
#            rows A1 (modal common stock) and the now-estimated 2003-2006 electric
#            flat (frfp modal); cols w_c, baseline EPC, frfp beta_c, np beta_c,
#            frfp contribution w*beta; footer tau_rw / raw ATT / composition.
#   Table P2 (hte_pilot_estimability.tex) - the full 14-treatment sweep: modal
#            cell, total/modal pairs, LAs, in_top100, estimable 06c vs support,
#            status/reason, tau_rw & coverage under the whitelist vs support rule.
#
# Sources: hte_pilot_cells_LA.csv, hte_pilot_summary_LA.csv,
#          hte_pilot_estimability_LA.csv (script 06c_..._pilot).
# Authors: Dmytro Kunchenko
# Date: July 10, 2026.

library(data.table)
library(here)
source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "archetype_names.R"))

outdir <- here("tables")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

tex_esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", x)
fmt_b   <- function(x) ifelse(is.na(x), "--", sprintf("%+.2f", x))
fmt_c   <- function(x) ifelse(is.na(x), "--", sprintf("%+.3f", x))
fmt_w   <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
fmt_cov <- function(x) ifelse(is.na(x), "--", sprintf("%.2f", x))
clean_band <- function(x) sub("England and Wales: ", "", x)
short_fuel <- function(mf) fifelse(grepl("electric", mf, ignore.case = TRUE), "Electricity",
                          fifelse(grepl("oil", mf, ignore.case = TRUE), "Oil",
                          fifelse(grepl("gas", mf, ignore.case = TRUE), "Mains gas", "Other")))

cells_path <- file.path(SUMMARY_TABLES_DIR, "hte_pilot_cells_LA.csv")
summ_path  <- file.path(SUMMARY_TABLES_DIR, "hte_pilot_estimability_LA.csv")
est_path   <- file.path(SUMMARY_TABLES_DIR, "hte_pilot_estimability_LA.csv")
summary_path <- file.path(SUMMARY_TABLES_DIR, "hte_pilot_summary_LA.csv")
stopifnot(file.exists(cells_path), file.exists(est_path), file.exists(summary_path))

cells <- fread(cells_path, na.strings = c("NA", ""))
est   <- fread(est_path,   na.strings = c("NA", ""))
summ  <- fread(summary_path, na.strings = c("NA", ""))
cells_cee <- cells[outcome == "current_energy_efficiency"]
summ_cee  <- summ[outcome == "current_energy_efficiency"]

# short name for a full "pt | bf | band | fuel | tercile" cell id
cell_short <- function(cid) {
  if (is.na(cid)) return(NA_character_)
  parts <- tstrsplit(cid, " | ", fixed = TRUE)
  d <- data.table(property_type = parts[[1]], built_form = parts[[2]],
                  construction_age_band = parts[[3]], main_fuel = parts[[4]],
                  floor_tercile = parts[[5]])
  make_archetype_names(d)
}

# ===========================================================================
# Table P1 - two-archetype contribution (frfp focal, np contrast)
# ===========================================================================
A1_cell    <- summ_cee[treatment_short_id == "frfp", A1_cell][1L]
modal_cell <- summ_cee[treatment_short_id == "frfp", modal_cell][1L]
focus_cells <- c(A1 = A1_cell, modal = modal_cell)

get_cell <- function(tsid, cid) cells_cee[treatment_short_id == tsid & cell_id == cid]

row_for <- function(tag, cid) {
  frfp <- get_cell("frfp", cid)
  np   <- get_cell("np",   cid)
  # covariate short name + baseline from the frfp row (baseline is treatment-invariant)
  base_epc <- if (nrow(frfp)) frfp$baseline_epc[1L] else NA_real_
  w_c      <- if (nrow(frfp)) frfp$w_c[1L] else NA_real_
  contrib  <- if (tag == "A1") summ_cee[treatment_short_id == "frfp", contrib_A1][1L]
              else summ_cee[treatment_short_id == "frfp", contrib_modal][1L]
  list(short = cell_short(cid), w = w_c, base = base_epc,
       b_frfp = if (nrow(frfp)) frfp$beta[1L] else NA_real_,
       b_np   = if (nrow(np))   np$beta[1L]   else NA_real_,
       contrib = contrib,
       label = if (tag == "A1") "A1: modal common stock" else "Offshore modal: 2003--2006 electric flat")
}
rA1 <- row_for("A1", A1_cell)
rMd <- row_for("modal", modal_cell)

frfp_att  <- summ_cee[treatment_short_id == "frfp", att][1L]
frfp_tau  <- summ_cee[treatment_short_id == "frfp", tau_rw_support][1L]
frfp_cov  <- summ_cee[treatment_short_id == "frfp", coverage_support][1L]
frfp_comp <- summ_cee[treatment_short_id == "frfp", composition][1L]
np_att    <- summ_cee[treatment_short_id == "np", att][1L]
np_tau    <- summ_cee[treatment_short_id == "np", tau_rw_support][1L]
np_comp   <- summ_cee[treatment_short_id == "np", composition][1L]

p1 <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Two-archetype contribution to the reweighted effect (EPC score, foreign for-profit focal)}",
  "\\label{tab:hte_pilot_contribution}", "\\small",
  "\\begin{tabular}{llrrrrr}",
  "\\toprule",
  " & Archetype & $w_c$ & Baseline & frfp $\\beta_c$ & np $\\beta_c$ & frfp $w_c\\beta_c$ \\\\",
  " & (covariate cell) & (share) & EPC & (SAP pts) & (SAP pts) & contribution \\\\",
  "\\midrule",
  sprintf("A1 & %s & %s & %.1f & %s & %s & %s \\\\",
          tex_esc(rA1$short), fmt_w(rA1$w), rA1$base,
          fmt_b(rA1$b_frfp), fmt_b(rA1$b_np), fmt_c(rA1$contrib)),
  sprintf("Offshore flat & %s & %s & %.1f & %s & %s & %s \\\\",
          tex_esc(rMd$short), fmt_w(rMd$w), rMd$base,
          fmt_b(rMd$b_frfp), fmt_b(rMd$b_np), fmt_c(rMd$contrib)),
  "\\midrule",
  sprintf("\\multicolumn{7}{l}{\\textit{Foreign for-profit (frfp):} raw ATT $= %+.2f$; $\\tau_{rw} = %+.2f$ (coverage %.2f); composition $= %+.2f$} \\\\",
          frfp_att, frfp_tau, frfp_cov, frfp_comp),
  sprintf("\\multicolumn{7}{l}{\\textit{Non-profit (np):} raw ATT $= %+.2f$; $\\tau_{rw} = %+.2f$; composition $= %+.2f$} \\\\",
          np_att, np_tau, np_comp),
  "\\bottomrule", "\\end{tabular}",
  "\\begin{minipage}{0.95\\textwidth}", "\\vspace{4pt}", "\\footnotesize",
  "\\textit{Notes:} A1 is the rank-1 (most common) cell in the eligible private-rental",
  "control pool; the offshore flat is the foreign for-profit modal cell, which the",
  "top-100 whitelist gate in the production 06c excludes but the matched-support gate",
  "makes directly estimable (over 800 matched pairs across 123 LAs). $w_c$ = control-pool share",
  "(the reweighting weight); baseline EPC = control-pool mean score of the cell; $\\beta_c$",
  "= matched-pair CATE (treated $-$ control, LA-clustered). The contribution column uses",
  "coverage-renormalised weights and sums (with the other estimated cells) to $\\tau_{rw}$.",
  "The small weight on the high-baseline offshore flat is the source of the composition",
  "effect: reweighting to the common stock down-weights it and the raw premium collapses.",
  "Source: \\texttt{hte\\_pilot\\_cells\\_LA.csv}, \\texttt{hte\\_pilot\\_summary\\_LA.csv} (pilot 06c).",
  "\\end{minipage}", "\\end{table}")
writeLines(p1, file.path(outdir, "hte_pilot_contribution.tex"))
cat("Written:", file.path(outdir, "hte_pilot_contribution.tex"), "\n")

# ===========================================================================
# Table P2 - estimability sweep (all 14 treatments)
# ===========================================================================
tp_order <- c("fp", "ukfp", "frfp", "thfp", "np", "uknp", "frnp", "thnp",
              "ps", "th", "bh", "eh", "ch", "oh")
est[, .ord := match(treatment_short_id, tp_order)]
est[is.na(.ord), .ord := 999L]
setorder(est, .ord)

# join tau_rw / coverage under each rule (primary outcome)
est2 <- merge(est,
              summ_cee[, .(treatment_short_id, tau_rw_support, coverage_support,
                           tau_rw_top100, coverage_top100)],
              by = "treatment_short_id", all.x = TRUE)
est2[, .ord := match(treatment_short_id, tp_order)]
est2[is.na(.ord), .ord := 999L]
setorder(est2, .ord)

yn   <- function(x) ifelse(is.na(x), "--", ifelse(as.logical(x), "yes", "no"))
stat_mark <- function(s) fcase(s == "ok", "\\textbf{ok}",
                               s == "suspect", "suspect$^\\dagger$",
                               s == "fail", "\\textit{fail}",
                               default = tex_esc(s))

p2 <- c(
  "\\begin{landscape}",
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Estimability sweep: modal cell of every treatment under the top-100 whitelist vs the matched-support rule (EPC score)}",
  "\\label{tab:hte_pilot_estimability}", "\\scriptsize",
  "\\begin{tabular}{llrrrccccrr}",
  "\\toprule",
  "Treat. & Modal cell (short) & Total & Modal & LAs & In & Est. & Est. & Status & $\\tau_{rw}$/cov & $\\tau_{rw}$/cov \\\\",
  " & & pairs & pairs & & top100 & 06c & support & & (top100) & (support) \\\\",
  "\\midrule")
for (i in seq_len(nrow(est2))) {
  r <- est2[i]
  short <- cell_short(r$modal_cell_id)
  taus_t <- if (is.na(r$tau_rw_top100)) "--/--"
            else sprintf("%+.2f/%.2f", r$tau_rw_top100, r$coverage_top100)
  taus_s <- if (is.na(r$tau_rw_support)) "--/--"
            else sprintf("%+.2f/%.2f", r$tau_rw_support, r$coverage_support)
  p2 <- c(p2, sprintf("%s & %s & %s & %s & %d & %s & %s & %s & %s & %s & %s \\\\",
                      tex_esc(r$treatment_short_id), tex_esc(short),
                      formatC(r$total_pairs, big.mark = ",", format = "d"),
                      formatC(r$modal_cell_pairs, big.mark = ",", format = "d"),
                      r$modal_cell_las, yn(r$modal_in_top100),
                      yn(r$estimable_06c), yn(r$estimable_support),
                      stat_mark(r$status), taus_t, taus_s))
}
p2 <- c(p2,
  "\\bottomrule", "\\end{tabular}",
  "\\begin{minipage}{0.95\\textwidth}", "\\vspace{4pt}", "\\footnotesize",
  "\\textit{Notes:} One row per treatment = its modal (most common treated) covariate",
  "cell. `Est.\\ 06c' = estimable under the production rule (cell in the top-100",
  "control-pool whitelist \\emph{and} $\\geq 50$ matched pairs); `Est.\\ support' =",
  "estimable under the pilot rule ($\\geq 50$ matched pairs \\emph{and} $\\geq 20$",
  "distinct LAs, whitelist not required). Status: \\textbf{ok} = clears the support",
  "floor; suspect$^\\dagger$ = enough pairs but few clusters ($<20$ LAs); \\textit{fail}",
  "= below the pair floor. $\\tau_{rw}$/cov = reweighted ATT and control-pool coverage",
  "under each rule. The offshore-haven 2003--2006 electric flats (frfp, thfp, th, bh, eh)",
  "flip from `no' to `ok' and coverage rises markedly; \\texttt{oh} genuinely fails",
  "(too few matched pairs) -- the honest ``not every cell needs its own estimate''.",
  "Source: \\texttt{hte\\_pilot\\_estimability\\_LA.csv}, \\texttt{hte\\_pilot\\_summary\\_LA.csv} (pilot 06c).",
  "\\end{minipage}", "\\end{table}",
  "\\end{landscape}")
writeLines(p2, file.path(outdir, "hte_pilot_estimability.tex"))
cat("Written:", file.path(outdir, "hte_pilot_estimability.tex"), "\n")

cat("\ngenerate_results_tables_pilot.R complete.\n")
