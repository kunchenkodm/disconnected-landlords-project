# generate_results_tables.R
# Reads results_enriched_LA.csv and produces LaTeX tables for the writeup.
# Output: tables/*.tex files, each containing one table.

library(data.table)
library(here)

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
outdir <- here("tables")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

dt <- fread(here("output/summary_tables/results_enriched_LA.csv"))

# Primary model filter
PSM_SUB <- "PSM (Matched) + Subclass FE"

# Treatment labels (display order)
treat_labels <- c(
  ps   = "Public sector",
  np   = "Non-profit",
  uknp = "UK non-profit",
  th   = "Tax haven (all)",
  frfp = "Foreign for-profit",
  thfp = "Tax haven for-profit",
  fp   = "For-profit (all)",
  ukfp = "UK for-profit"
)

# Formatting helpers
fmt_coef <- function(x, digits = 2) {
  ifelse(is.na(x), "--",
         ifelse(x > 0, paste0("+", formatC(x, format = "f", digits = digits)),
                formatC(x, format = "f", digits = digits)))
}

fmt_p <- function(p) {
  ifelse(is.na(p), "--",
         ifelse(p < 0.001, "$<$0.001",
                ifelse(p < 0.01, sprintf("%.3f", p),
                       ifelse(p < 0.05, sprintf("%.3f", p),
                              sprintf("%.3f", p)))))
}

fmt_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*", ""))))
}

fmt_n <- function(n) {
  ifelse(is.na(n), "--", formatC(n, format = "d", big.mark = ","))
}

fmt_std <- function(x) {
  ifelse(is.na(x), "--", formatC(x, format = "f", digits = 3))
}

write_tex <- function(lines, filename) {
  writeLines(lines, file.path(outdir, filename))
  cat("Written:", file.path(outdir, filename), "\n")
}

# ---------------------------------------------------------------------------
# Table 1: Baseline current_energy_efficiency by treatment group
# ---------------------------------------------------------------------------
tab1_treats <- c("ps", "np", "th", "frfp", "fp", "ukfp")

tab1 <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
             outcome == "current_energy_efficiency" &
             treatment_short_id %in% tab1_treats & status == "ok",
           .(treatment_short_id, coef, standardised_coef, p_value, nobs)],
           by = "treatment_short_id")

tab1[, label := treat_labels[treatment_short_id]]
tab1 <- tab1[match(tab1_treats, treatment_short_id)]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Baseline EPC Score Differentials by Ownership Type}",
  "\\label{tab:h1_baseline}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Treatment & Coef (SAP pts) & Std.\\ coef & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab1))) {
  r <- tab1[i]
  lines <- c(lines, sprintf("%s & %s%s & %s & %s & %s \\\\",
                            r$label,
                            fmt_coef(r$coef),
                            fmt_stars(r$p_value),
                            fmt_std(r$standardised_coef),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass (matched-pair) fixed effects, LA geography, Baseline specification, base matching core.",
           "Outcome: \\texttt{current\\_energy\\_efficiency} (SAP score, 0--100+; higher = better).",
           "Control group: privately rented properties with unknown ownership.",
           "Standard errors clustered at local authority level.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}, filtered to model = ``PSM (Matched) + Subclass FE'',",
           "spec = ``Baseline'', matching\\_core = ``base''.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h1_baseline.tex")

# ---------------------------------------------------------------------------
# Table 2: For-profit attenuation across spec x core diagonal
# The "most demanding" version of each core uses the matching spec:
#   base -> Baseline, council_tax -> Council Tax,
#   ppd -> Price Paid, ppd_counciltax -> Council Tax + Price Paid
# ---------------------------------------------------------------------------
attn_rows <- data.table(
  spec          = c("Baseline", "Council Tax", "Price Paid", "Council Tax + Price Paid"),
  matching_core = c("base",     "council_tax", "ppd",        "ppd_counciltax"),
  core_label    = c("Base",     "Council tax", "Price paid", "Council tax + price paid")
)

tab2_all <- list()
for (tid in c("fp", "th", "np", "ps")) {
  rows <- dt[model == PSM_SUB & treatment_short_id == tid &
               outcome == "current_energy_efficiency" & status == "ok",
             .(spec, matching_core, coef, standardised_coef, p_value, nobs)]
  # Keep first match per spec x core (deduplicate)
  rows <- unique(rows, by = c("spec", "matching_core"))
  merged <- merge(attn_rows, rows, by = c("spec", "matching_core"), all.x = TRUE)
  merged[, treat := tid]
  tab2_all[[tid]] <- merged
}
tab2 <- rbindlist(tab2_all)

# Table 2a: For-profit attenuation detail
tab2_fp <- tab2[treat == "fp"][match(attn_rows$matching_core, matching_core)]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{For-Profit EPC Score: Attenuation Across Specifications}",
  "\\label{tab:h1_attenuation_fp}",
  "\\begin{tabular}{llrrrr}",
  "\\toprule",
  "Specification & Matching core & Coef & Std.\\ coef & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab2_fp))) {
  r <- tab2_fp[i]
  lines <- c(lines, sprintf("%s & %s & %s%s & %s & %s & %s \\\\",
                            r$spec,
                            r$core_label,
                            fmt_coef(r$coef),
                            fmt_stars(r$p_value),
                            fmt_std(r$standardised_coef),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography.",
           "Each row uses the specification matched to its core: the Baseline spec on the base core,",
           "the Council Tax spec on the council\\_tax core, etc.",
           "Outcome: \\texttt{current\\_energy\\_efficiency}.",
           "Treatment: for-profit (all) vs.\\ private rental control.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h1_attenuation_fp.tex")

# Table 2b: Attenuation endpoints for all key treatments
tab2_endpoints <- unique(tab2[matching_core %in% c("base", "ppd_counciltax")],
                        by = c("treat", "matching_core"))
tab2_endpoints <- dcast(tab2_endpoints, treat ~ matching_core,
                        value.var = c("coef", "p_value", "standardised_coef", "nobs"))

tab2_endpoints[, label := treat_labels[treat]]
tab2_endpoints <- tab2_endpoints[match(c("fp", "th", "np", "ps"), treat)]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{EPC Score: Baseline vs.\\ Most Demanding Specification}",
  "\\label{tab:h1_attenuation_all}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Baseline / base} & \\multicolumn{2}{c}{CT+PP / ppd\\_counciltax} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Treatment & Coef & $N$ & Coef & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab2_endpoints))) {
  r <- tab2_endpoints[i]
  lines <- c(lines, sprintf("%s & %s%s & %s & %s%s & %s \\\\",
                            r$label,
                            fmt_coef(r$coef_base), fmt_stars(r$p_value_base),
                            fmt_n(r$nobs_base),
                            fmt_coef(r$coef_ppd_counciltax), fmt_stars(r$p_value_ppd_counciltax),
                            fmt_n(r$nobs_ppd_counciltax)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography.",
           "Left panel: Baseline spec, base core. Right panel: Council Tax + Price Paid spec, ppd\\_counciltax core.",
           "Outcome: \\texttt{current\\_energy\\_efficiency}.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h1_attenuation_all.tex")

# ---------------------------------------------------------------------------
# Table 3: Gap analysis
# ---------------------------------------------------------------------------
tab3_treats <- c("fp", "th", "frfp", "np", "ps")

tab3_cur <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
                 outcome == "current_energy_efficiency" &
                 treatment_short_id %in% tab3_treats & status == "ok",
               .(treatment_short_id, coef_cur = coef, p_cur = p_value)],
               by = "treatment_short_id")

tab3_gap <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
                 outcome == "energy_efficiency_potential_gap" &
                 treatment_short_id %in% tab3_treats & status == "ok",
               .(treatment_short_id, coef_gap = coef, p_gap = p_value)],
               by = "treatment_short_id")

tab3 <- merge(tab3_cur, tab3_gap, by = "treatment_short_id")
tab3[, ratio := abs(coef_gap / coef_cur)]
tab3[, label := treat_labels[treatment_short_id]]
tab3 <- tab3[match(tab3_treats, treatment_short_id)]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Gap Analysis: Current EPC vs.\\ Potential--Current Gap}",
  "\\label{tab:gap_analysis}",
  "\\begin{tabular}{lrrrl}",
  "\\toprule",
  "Treatment & Current EPC coef & Gap coef & $|$Ratio$|$ & Interpretation \\\\",
  "\\midrule"
)

interp_map <- c(fp = "Stock selection",
                th = "Hybrid",
                frfp = "Hybrid",
                np = "Predominantly operational",
                ps = "Predominantly operational")

for (i in seq_len(nrow(tab3))) {
  r <- tab3[i]
  lines <- c(lines, sprintf("%s & %s%s & %s%s & %.2f & %s \\\\",
                            r$label,
                            fmt_coef(r$coef_cur), fmt_stars(r$p_cur),
                            fmt_coef(r$coef_gap, 2), fmt_stars(r$p_gap),
                            r$ratio,
                            interp_map[r$treatment_short_id]))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography, Baseline spec, base core.",
           "\\textbf{Speculative.} The cross-sectional gap is not a clean test of operational",
           "investment; panel data would be needed to identify retrofit activity directly.",
           "Current EPC coef: coefficient on \\texttt{current\\_energy\\_efficiency}.",
           "Gap coef: coefficient on \\texttt{energy\\_efficiency\\_potential\\_gap} (= potential EPC $-$ current EPC).",
           "Ratio = $|$gap coef $/$ current EPC coef$|$.",
           "A ratio near 0 means both current and potential rise together (selection);",
           "a ratio near 1 means the gap shrinks (operational investment).",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_gap_analysis.tex")

# ---------------------------------------------------------------------------
# Table 4: bad_epc coefficients
# ---------------------------------------------------------------------------
tab4_treats <- c("ps", "np", "th", "frfp", "fp")

tab4 <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
             outcome == "bad_epc_c" &
             treatment_short_id %in% tab4_treats & status == "ok",
           .(treatment_short_id, coef, standardised_coef, p_value, nobs)],
           by = "treatment_short_id")

tab4[, label := treat_labels[treatment_short_id]]
tab4 <- tab4[match(tab4_treats, treatment_short_id)]
# Convert to percentage points
tab4[, coef_pp := coef * 100]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Bad EPC Prevalence by Ownership Type}",
  "\\label{tab:h2_bad_epc}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Treatment & Coef (pp) & Std.\\ coef & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab4))) {
  r <- tab4[i]
  lines <- c(lines, sprintf("%s & %s%s & %s & %s & %s \\\\",
                            r$label,
                            fmt_coef(r$coef_pp, 1),
                            fmt_stars(r$p_value),
                            fmt_std(r$standardised_coef),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography, Baseline spec, base core.",
           "Outcome: \\texttt{bad\\_epc\\_c} (= 1 if below EPC C, i.e.\\ SAP score $<$ 69) --- the incoming MEES bound.",
           "Coefficients in percentage points (coef $\\times$ 100).",
           "Negative = treated properties are \\textit{less} likely to have a bad EPC.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h2_bad_epc.tex")

# ---------------------------------------------------------------------------
# Table 5: borderline_good_epc
# ---------------------------------------------------------------------------
tab5_treats <- c("ps", "np", "fp", "th", "frfp")

tab5 <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
             outcome == "borderline_good_epc" &
             treatment_short_id %in% tab5_treats & status == "ok",
           .(treatment_short_id, coef, standardised_coef, p_value, nobs)],
           by = "treatment_short_id")

tab5[, label := treat_labels[treatment_short_id]]
tab5 <- tab5[match(tab5_treats, treatment_short_id)]
tab5[, coef_pp := coef * 100]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Bunching Just Above the C Threshold (MEES Cutoff)}",
  "\\label{tab:h3_borderline_good}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Treatment & Coef (pp) & Std.\\ coef & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab5))) {
  r <- tab5[i]
  lines <- c(lines, sprintf("%s & %s%s & %s & %s & %s \\\\",
                            r$label,
                            fmt_coef(r$coef_pp, 1),
                            fmt_stars(r$p_value),
                            fmt_std(r$standardised_coef),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography, Baseline spec, base core.",
           "Outcome: \\texttt{borderline\\_good\\_epc} (= 1 if SAP score in $(69,\\, 69 + \\frac{1}{2}\\sigma]$).",
           "Coefficients in percentage points. Positive = excess mass just above the C threshold.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h3_borderline_good.tex")

# ---------------------------------------------------------------------------
# Table 6: borderline_better_epc
# ---------------------------------------------------------------------------
tab6_treats <- c("fp", "np", "ps", "th", "frfp")

tab6 <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
             outcome == "borderline_better_epc" &
             treatment_short_id %in% tab6_treats & status == "ok",
           .(treatment_short_id, coef, standardised_coef, p_value, nobs)],
           by = "treatment_short_id")

tab6[, label := treat_labels[treatment_short_id]]
tab6 <- tab6[match(tab6_treats, treatment_short_id)]
tab6[, coef_pp := coef * 100]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Bunching Just Above Any Band Boundary}",
  "\\label{tab:h3_borderline_better}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Treatment & Coef (pp) & Std.\\ coef & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab6))) {
  r <- tab6[i]
  lines <- c(lines, sprintf("%s & %s%s & %s & %s & %s \\\\",
                            r$label,
                            fmt_coef(r$coef_pp, 2),
                            fmt_stars(r$p_value),
                            fmt_std(r$standardised_coef),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography, Baseline spec, base core.",
           "Outcome: \\texttt{borderline\\_better\\_epc} (= 1 if SAP score just above any band's lower boundary).",
           "Coefficients in percentage points.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h3_borderline_better.tex")

# ---------------------------------------------------------------------------
# Table 7: Treatment hierarchy
# ---------------------------------------------------------------------------
tab7_treats <- c("ps", "np", "uknp", "th", "frfp", "thfp",
                 "fp", "ukfp")

tab7 <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
             outcome == "current_energy_efficiency" &
             treatment_short_id %in% tab7_treats & status == "ok",
           .(treatment_short_id, standardised_coef, p_value, nobs)],
           by = "treatment_short_id")

tab7[, label := treat_labels[treatment_short_id]]
tab7 <- tab7[match(tab7_treats, treatment_short_id)]

# Assign tiers
tab7[, tier := fcase(
  treatment_short_id %in% c("ps", "np", "uknp"), "Public/Non-profit",
  treatment_short_id %in% c("th", "frfp", "thfp"), "Foreign/Tax haven",
  treatment_short_id %in% c("fp", "ukfp"), "Domestic for-profit"
)]

# Check survival in most demanding spec
tab7_surv <- unique(dt[model == PSM_SUB & spec == "Council Tax + Price Paid" &
                  matching_core == "ppd_counciltax" &
                  outcome == "current_energy_efficiency" &
                  treatment_short_id %in% tab7_treats & status == "ok",
                .(treatment_short_id, p_surv = p_value)],
                by = "treatment_short_id")

tab7 <- merge(tab7, tab7_surv, by = "treatment_short_id", all.x = TRUE)
tab7[, survives := ifelse(is.na(p_surv), "--",
                          ifelse(p_surv < 0.05, "Yes", "No"))]
tab7 <- tab7[match(tab7_treats, treatment_short_id)]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Treatment Group Hierarchy: Standardised EPC Differentials}",
  "\\label{tab:h5_hierarchy}",
  "\\begin{tabular}{llrrr}",
  "\\toprule",
  "Tier & Treatment & Std.\\ coef & $N$ & Survives Spec D? \\\\",
  "\\midrule"
)

prev_tier <- ""
for (i in seq_len(nrow(tab7))) {
  r <- tab7[i]
  tier_str <- if (r$tier != prev_tier) r$tier else ""
  prev_tier <- r$tier
  if (tier_str != "" && i > 1) {
    lines <- c(lines, "\\addlinespace")
  }
  lines <- c(lines, sprintf("%s & %s & %s & %s & %s \\\\",
                            tier_str,
                            r$label,
                            fmt_std(r$standardised_coef),
                            fmt_n(r$nobs),
                            r$survives))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography, Baseline spec, base core.",
           "Outcome: \\texttt{current\\_energy\\_efficiency}.",
           "``Survives Spec D'' = coefficient significant at $p < 0.05$ in the",
           "Council Tax + Price Paid specification on the ppd\\_counciltax core.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h5_hierarchy.tex")

# ---------------------------------------------------------------------------
# Table 8: bad_epc robustness across all spec x core
# ---------------------------------------------------------------------------
tab8 <- unique(dt[model == PSM_SUB & outcome == "bad_epc_c" &
             treatment_short_id == "fp" & status == "ok",
           .(spec, matching_core, coef, p_value, nobs)],
           by = c("spec", "matching_core"))

tab8[, coef_pp := coef * 100]
tab8 <- tab8[order(match(spec, c("Baseline", "Council Tax", "Price Paid", "Council Tax + Price Paid")),
                   match(matching_core, c("base", "council_tax", "ppd", "ppd_counciltax")))]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{For-Profit Bad EPC: Robustness Across Specifications}",
  "\\label{tab:h2_bad_epc_robust}",
  "\\begin{tabular}{llrrr}",
  "\\toprule",
  "Specification & Core & Coef (pp) & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab8))) {
  r <- tab8[i]
  lines <- c(lines, sprintf("%s & %s & %s%s & %s & %s \\\\",
                            r$spec,
                            gsub("_", "\\\\_", r$matching_core),
                            fmt_coef(r$coef_pp, 2),
                            fmt_stars(r$p_value),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography.",
           "Treatment: for-profit (all). Outcome: \\texttt{bad\\_epc\\_c} (below EPC C).",
           "All spec $\\times$ core combinations shown.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h2_bad_epc_robust.tex")

# ---------------------------------------------------------------------------
# Table 4e: bad_epc_e coefficients (present regulatory bound, below EPC E)
# ---------------------------------------------------------------------------
tab4e_treats <- c("ps", "np", "th", "frfp", "fp")

tab4e <- unique(dt[model == PSM_SUB & spec == "Baseline" & matching_core == "base" &
             outcome == "bad_epc_e" &
             treatment_short_id %in% tab4e_treats & status == "ok",
           .(treatment_short_id, coef, standardised_coef, p_value, nobs)],
           by = "treatment_short_id")

tab4e[, label := treat_labels[treatment_short_id]]
tab4e <- tab4e[match(tab4e_treats, treatment_short_id)]
tab4e[, coef_pp := coef * 100]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Below-E EPC Prevalence by Ownership Type}",
  "\\label{tab:h2_bad_epc_e}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Treatment & Coef (pp) & Std.\\ coef & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab4e))) {
  r <- tab4e[i]
  lines <- c(lines, sprintf("%s & %s%s & %s & %s & %s \\\\",
                            r$label,
                            fmt_coef(r$coef_pp, 1),
                            fmt_stars(r$p_value),
                            fmt_std(r$standardised_coef),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography, Baseline spec, base core.",
           "Outcome: \\texttt{bad\\_epc\\_e} (= 1 if below EPC E, i.e.\\ SAP score $<$ 39) --- the present regulatory minimum.",
           "Coefficients in percentage points (coef $\\times$ 100).",
           "Negative = treated properties are \\textit{less} likely to fall below E.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h2_bad_epc_e.tex")

# ---------------------------------------------------------------------------
# Table 8e: bad_epc_e robustness across all spec x core
# ---------------------------------------------------------------------------
tab8e <- unique(dt[model == PSM_SUB & outcome == "bad_epc_e" &
             treatment_short_id == "fp" & status == "ok",
           .(spec, matching_core, coef, p_value, nobs)],
           by = c("spec", "matching_core"))

tab8e[, coef_pp := coef * 100]
tab8e <- tab8e[order(match(spec, c("Baseline", "Council Tax", "Price Paid", "Council Tax + Price Paid")),
                   match(matching_core, c("base", "council_tax", "ppd", "ppd_counciltax")))]

lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{For-Profit Below-E EPC: Robustness Across Specifications}",
  "\\label{tab:h2_bad_epc_e_robust}",
  "\\begin{tabular}{llrrr}",
  "\\toprule",
  "Specification & Core & Coef (pp) & $p$-value & $N$ \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(tab8e))) {
  r <- tab8e[i]
  lines <- c(lines, sprintf("%s & %s & %s%s & %s & %s \\\\",
                            r$spec,
                            gsub("_", "\\\\_", r$matching_core),
                            fmt_coef(r$coef_pp, 2),
                            fmt_stars(r$p_value),
                            fmt_p(r$p_value),
                            fmt_n(r$nobs)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} PSM with subclass FE, LA geography.",
           "Treatment: for-profit (all). Outcome: \\texttt{bad\\_epc\\_e} (below EPC E).",
           "All spec $\\times$ core combinations shown.",
           "$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$.",
           "Source: \\texttt{results\\_enriched\\_LA.csv}.",
           "\\end{minipage}",
           "\\end{table}")

write_tex(lines, "tab_h2_bad_epc_e_robust.tex")

# ---------------------------------------------------------------------------
# Table A1: Matching attrition funnel (PI feedback, attrition workstream)
# Rows = funnel stages (treated units + LA survival); columns = headline
# treatments. Panel A: Baseline spec / base core. Panel B: Price Paid spec /
# ppd core. Source: attrition_funnel_LA.csv from 05b_compute_balance.R.
# ---------------------------------------------------------------------------
funnel_path <- here("output/summary_tables/attrition_funnel_LA.csv")
if (file.exists(funnel_path)) {
  fun <- fread(funnel_path, na.strings = c("NA", ""))
  funnel_treats <- c("fp", "frfp", "th", "np", "ps")

  stage_rows <- list(
    c("Eligible (raw, core sample)",  "n_treated_raw"),
    c("Complete covariate cases",     "n_treated_complete_case"),
    c("Passed size gate",             "n_treated_gate_passed"),
    c("Matched (1:1 PSM)",            "n_matched_treated"),
    c("Caliper PS $\\leq$ 0.2",       "n_matched_treated_cal02"),
    c("Caliper PS $\\leq$ 0.1",       "n_matched_treated_cal01")
  )
  la_rows <- list(
    c("LAs at start",                 "n_las_input"),
    c("LAs with eligible sample",     "n_las_eligible"),
    c("LAs passing gate",             "n_las_gate_passed"),
    c("LAs with matches",             "n_las_matched"),
    c("LAs in headline regression",   "n_las_regression")
  )

  make_funnel_panel <- function(fun_sub, panel_label) {
    out <- c(sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", length(funnel_treats) + 1L, panel_label))
    grab <- function(col) {
      vapply(funnel_treats, function(t) {
        v <- fun_sub[treatment_short_id == t][[col]]
        if (length(v) == 0L || is.na(v[1L])) "--" else formatC(v[1L], format = "d", big.mark = ",")
      }, character(1L))
    }
    for (sr in stage_rows) {
      out <- c(out, paste0(sr[1L], " & ", paste(grab(sr[2L]), collapse = " & "), " \\\\"))
    }
    out <- c(out, "\\addlinespace")
    for (lr in la_rows) {
      out <- c(out, paste0(lr[1L], " & ", paste(grab(lr[2L]), collapse = " & "), " \\\\"))
    }
    out
  }

  panel_a <- fun[matching_core == "base" & spec == "Baseline"]
  panel_b <- fun[matching_core == "ppd" & spec == "Price Paid"]

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Matching Attrition Funnel: Treated Units and LA Survival by Stage}",
    "\\label{tab:attrition_funnel}",
    "\\small",
    paste0("\\begin{tabular}{l", paste(rep("r", length(funnel_treats)), collapse = ""), "}"),
    "\\toprule",
    paste0("Stage & ", paste(c("For-profit", "Foreign FP", "Tax haven", "Non-profit", "Public"), collapse = " & "), " \\\\"),
    "\\midrule",
    make_funnel_panel(panel_a, "Panel A: Baseline spec, base core"),
    "\\midrule",
    make_funnel_panel(panel_b, "Panel B: Price Paid spec, ppd core"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{0.95\\textwidth}",
    "\\vspace{4pt}",
    "\\footnotesize",
    "\\textit{Notes:} Counts of treated properties surviving each stage of the matching",
    "funnel, pooled across local authorities, plus the number of LAs surviving each stage.",
    "Eligible = non-missing treatment indicator within the core sample. Complete cases =",
    "non-missing matching covariates. Size gate = per-LA minimum treated/control counts",
    "(sparse cores: $>5$ treated, $>25$ control; other cores: $>10$ treated, $>50$ control).",
    "Matched = 1:1 nearest-neighbour propensity-score matches with exact strata.",
    "Calipers are post-hoc propensity-score filters as in the PS$\\leq$0.2 / PS$\\leq$0.1 models.",
    "``LAs in headline regression'' = distinct-LA cluster count of the PSM + Subclass FE model",
    "(outcome \\texttt{current\\_energy\\_efficiency}).",
    "Source: \\texttt{attrition\\_funnel\\_LA.csv} (script 05b).",
    "\\end{minipage}",
    "\\end{table}")

  write_tex(lines, "attrition_funnel.tex")
} else {
  cat("Skipped attrition_funnel.tex (no attrition_funnel_LA.csv; run 05b first)\n")
}

# ---------------------------------------------------------------------------
# Tables H1-H3: Heterogeneous effects / archetypes (PI feedback, workstream 1)
# Sources: hte_archetype_definitions_LA.csv, hte_reweighted_LA.csv,
#          hte_tree_nodes_LA.csv (script 06c). Skipped gracefully if absent.
# ---------------------------------------------------------------------------
hte_defs_path <- here("output/summary_tables/hte_archetype_definitions_LA.csv")
hte_rw_path   <- here("output/summary_tables/hte_reweighted_LA.csv")
hte_tree_path <- here("output/summary_tables/hte_tree_nodes_LA.csv")
hte_baselines_path <- here("output/summary_tables/hte_cell_baselines_LA.csv")
hte_reference_path <- here("output/summary_tables/hte_reference_cells_LA.csv")
source(here("scripts", "archetype_names.R"))  # make_archetype_names()

tex_esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", x)

# Short-name lookup keyed on cell_id (archetypes + council references), so every
# HTE exhibit labels a cell the same way. Built once from the definitions.
archetype_short_lookup <- function() {
  if (!file.exists(hte_defs_path)) return(setNames(character(0), character(0)))
  d <- fread(hte_defs_path, na.strings = c("NA", ""))[is_archetype == TRUE]
  nm <- make_archetype_names(d)
  lk <- setNames(nm, d$cell_id)
  if (file.exists(hte_reference_path)) {
    rf <- fread(hte_reference_path, na.strings = c("NA", ""))
    rlk <- setNames(fifelse(rf$ref_type == "council_flat", "Council flat", "Council semi"),
                    rf$cell_id)
    lk <- c(lk, rlk[!(names(rlk) %in% names(lk))])
  }
  lk
}
SHORT_NAME <- archetype_short_lookup()

# --- Table H1: archetype covariate legend (short names + defining covariates) ---
# The single reference table for the HTE exhibits: each archetype's short name,
# the covariates that define it, and its control-pool baseline EPC. Archetypes
# (A) are the diversity-aware selection; the council-stock reference (R) rows add
# representative social housing. Every other HTE exhibit refers to these names.
clean_band <- function(x) sub("England and Wales: ", "", x)
short_fuel <- function(mf) fifelse(grepl("electric", mf, ignore.case = TRUE), "Electricity",
                          fifelse(grepl("oil", mf, ignore.case = TRUE), "Oil",
                          fifelse(grepl("gas", mf, ignore.case = TRUE), "Mains gas", "Other")))
if (file.exists(hte_defs_path) && file.exists(hte_baselines_path)) {
  hd <- fread(hte_defs_path, na.strings = c("NA", ""))[is_archetype == TRUE]
  hd <- if ("arch_rank" %in% names(hd)) hd[order(arch_rank)] else hd[order(rank)]
  blz <- fread(hte_baselines_path, na.strings = c("NA", ""))
  hd[, short := make_archetype_names(hd)]
  hd <- merge(hd, blz[, .(cell_id, base_epc = baseline_current_energy_efficiency)],
              by = "cell_id", all.x = TRUE)
  if ("arch_rank" %in% names(hd)) setorder(hd, arch_rank)
  refrows <- NULL
  if (file.exists(hte_reference_path)) {
    rf <- fread(hte_reference_path, na.strings = c("NA", ""))
    rf <- merge(rf, blz[, .(cell_id, base_epc = baseline_current_energy_efficiency)],
                by = "cell_id", all.x = TRUE)
    rf[, short := fifelse(ref_type == "council_flat", "Council flat", "Council semi")]
    setorder(rf, ref_type)
    refrows <- rf
  }
  lines <- c(
    "\\begin{table}[htbp]", "\\centering",
    "\\caption{Property Archetypes: Short Names and Defining Covariates}",
    "\\label{tab:hte_archetypes}", "\\scriptsize",
    "\\begin{tabular}{rlllllcr}",
    "\\toprule",
    " & Short name & Property type & Built form & Age band & Main fuel & Floor tercile & Baseline EPC \\\\",
    "\\midrule")
  for (i in seq_len(nrow(hd))) {
    r <- hd[i]
    lines <- c(lines, sprintf("A%d & %s & %s & %s & %s & %s & %s & %.1f \\\\",
                              r$arch_rank, tex_esc(r$short), tex_esc(r$property_type),
                              tex_esc(r$built_form), tex_esc(clean_band(r$construction_age_band)),
                              tex_esc(short_fuel(r$main_fuel)), r$floor_tercile, r$base_epc))
  }
  if (!is.null(refrows) && nrow(refrows) > 0L) {
    lines <- c(lines, "\\midrule",
               "\\multicolumn{8}{l}{\\textit{Council-stock reference (representative social housing)}} \\\\")
    for (i in seq_len(nrow(refrows))) {
      r <- refrows[i]
      lines <- c(lines, sprintf("R & %s & %s & %s & %s & %s & %s & %.1f \\\\",
                                tex_esc(r$short), tex_esc(r$property_type), tex_esc(r$built_form),
                                tex_esc(clean_band(r$construction_age_band)),
                                tex_esc(short_fuel(r$main_fuel)), r$floor_tercile, r$base_epc))
    }
  }
  lines <- c(lines,
             "\\bottomrule", "\\end{tabular}",
             "\\begin{minipage}{0.95\\textwidth}", "\\vspace{4pt}", "\\footnotesize",
             "\\textit{Notes:} Each archetype is a covariate cell = property type $\\times$ built form",
             "$\\times$ construction age band $\\times$ main fuel $\\times$ floor-area tercile, in the",
             "eligible private-rental control pool, England-wide. Archetypes (A) are a diversity-aware",
             "selection spanning property type $\\times$ fuel class (gas/electric/oil) $\\times$ era",
             "(Victorian/Edwardian/Interwar/post-war/modern); the council-stock reference (R) rows add",
             "representative social housing. Short names are used in all other HTE exhibits. For flats,",
             "`built form' is the RdSAP heat-loss exposure category (number of party walls), not a house",
             "typology. Baseline EPC = control-pool mean EPC score of the cell (the level a treatment",
             "effect shifts from).",
             sprintf("Floor-area terciles cut at %.0f and %.0f sqm on the control pool.",
                     hd$floor_tercile_cut1[1], hd$floor_tercile_cut2[1]),
             "Source: \\texttt{hte\\_archetype\\_definitions\\_LA.csv}, \\texttt{hte\\_reference\\_cells\\_LA.csv},",
             "\\texttt{hte\\_cell\\_baselines\\_LA.csv} (script 06c).",
             "\\end{minipage}", "\\end{table}")
  write_tex(lines, "hte_archetypes.tex")
} else {
  cat("Skipped hte_archetypes.tex (no defs/baselines; run 06c)\n")
}

# --- Table H1b: typical (modal) property per ownership type (R2) ---
# The distinctive treated stock, in contrast to the common control cells. Shows
# that offshore/haven owners hold prime flats where for-profit hold period
# terraces and public/non-profit hold post-war stock; the estimable flag marks
# whether that modal cell has matched support for a CATE.
hte_typical_path <- here("output/summary_tables/hte_typical_property_LA.csv")
if (file.exists(hte_typical_path)) {
  tp <- fread(hte_typical_path, na.strings = c("NA", ""))
  tp_order <- c("fp", "ukfp", "frfp", "np", "ps", "th", "thfp", "thnp",
                "ch", "bh", "eh", "oh", "frnp", "nonprofit")
  tp[, .ord := match(treatment_short_id, tp_order)]
  tp[is.na(.ord), .ord := 999L]
  setorder(tp, .ord)
  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Typical (Modal) Property by Ownership Type}",
    "\\label{tab:hte_typical_property}",
    "\\scriptsize",
    "\\begin{tabular}{lllllrrc}",
    "\\toprule",
    "Ownership & Property type & Built form & Age band & Main fuel & Share & Mean EPC & Estimable \\\\",
    "\\midrule"
  )
  for (i in seq_len(nrow(tp))) {
    r <- tp[i]
    est_flag <- if (isTRUE(r$estimable_matched)) "yes" else if (isFALSE(r$estimable_matched)) "no" else "--"
    own <- sub("^Effect of ", "", r$treatment)          # drop the "Effect of " prefix
    own <- sub(" Ownership$", "", own)                    # and the trailing "Ownership"
    lines <- c(lines, sprintf("%s & %s & %s & %s & %s & %.1f\\%% & %.1f & %s \\\\",
                              tex_esc(substr(own, 1, 22)),
                              tex_esc(r$property_type), tex_esc(r$built_form),
                              tex_esc(clean_band(r$construction_age_band)),
                              tex_esc(short_fuel(r$main_fuel)),
                              r$share_treated * 100, r$treated_mean_epc, est_flag))
  }
  lines <- c(lines,
             "\\bottomrule",
             "\\end{tabular}",
             "\\begin{minipage}{0.95\\textwidth}",
             "\\vspace{4pt}",
             "\\footnotesize",
             "\\textit{Notes:} For each ownership type, the single most common covariate cell among",
             "its \\emph{treated} eligible properties (property type $\\times$ built form $\\times$ age",
             "band $\\times$ main fuel $\\times$ floor tercile; tercile omitted here for space).",
             "Share = share of the treated group falling in that modal cell. Mean EPC = mean current",
             "energy-efficiency score of the treated group. Estimable = the modal cell is in the",
             "archetype whitelist \\emph{and} has $\\geq 50$ matched pairs for that ownership type, so a",
             "matched CATE exists for it; distinctive treated cells (e.g.\\ prime flats) often lack",
             "matched support and so are not estimable, which is why the matched archetypes reflect",
             "the common control stock instead. Source: \\texttt{hte\\_typical\\_property\\_LA.csv} (06c).",
             "\\end{minipage}",
             "\\end{table}")
  write_tex(lines, "hte_typical_property.tex")
} else {
  cat("Skipped hte_typical_property.tex (no hte_typical_property_LA.csv; run 06c)\n")
}

# --- Table H1c: archetype x ownership CATE matrix with baseline anchor (R2) ---
# Interpretable "baseline + treatment effect": rows = representative archetypes
# with their control-pool baseline EPC; columns = ownership CATEs, flagged
# (dagger = suspect, -- = no matched support) rather than dropped.
hte_matrix_path    <- here("output/summary_tables/hte_archetype_matrix_LA.csv")
hte_baselines_path <- here("output/summary_tables/hte_cell_baselines_LA.csv")
if (file.exists(hte_matrix_path) && file.exists(hte_defs_path) && file.exists(hte_baselines_path)) {
  mxx <- fread(hte_matrix_path, na.strings = c("NA", ""))
  bll <- fread(hte_baselines_path, na.strings = c("NA", ""))
  dfx <- fread(hte_defs_path, na.strings = c("NA", ""))[is_archetype == TRUE]
  m_treats <- c("fp", "np", "ps", "th", "frfp")
  m_labs   <- c(fp = "For-profit", np = "Non-profit", ps = "Public",
                th = "Tax haven", frfp = "Foreign FP")
  cee <- mxx[outcome == "current_energy_efficiency" & treatment_short_id %in% m_treats]
  if (nrow(cee) > 0L) {
    arch_rows <- dfx[, .(cell_id, arch_rank, property_type, built_form,
                         era = if ("era" %in% names(dfx)) era else NA_character_,
                         fuel_class = if ("fuel_class" %in% names(dfx)) fuel_class else NA_character_)]
    arch_rows <- merge(arch_rows,
                       bll[, .(cell_id, base_epc = baseline_current_energy_efficiency)],
                       by = "cell_id")[order(arch_rank)]
    fmt_m <- function(beta, support) {
      if (length(support) == 0L || is.na(support) || support == "none" || is.na(beta)) return("--")
      s <- if (beta >= 0) sprintf("+%.2f", beta) else sprintf("%.2f", beta)
      if (support == "suspect") paste0(s, "$^{\\dagger}$") else s
    }
    lines <- c(
      "\\begin{table}[htbp]", "\\centering",
      "\\caption{Archetype $\\times$ Ownership Treatment Effects with Baseline Anchor (EPC Score)}",
      "\\label{tab:hte_archetype_matrix}", "\\scriptsize",
      "\\begin{tabular}{llrrrrrr}",
      "\\toprule",
      paste0("A & Archetype & Baseline & ",
             paste(m_labs[m_treats], collapse = " & "), " \\\\"),
      "\\midrule")
    for (i in seq_len(nrow(arch_rows))) {
      r <- arch_rows[i]
      desc <- unname(SHORT_NAME[r$cell_id])
      if (is.na(desc)) desc <- paste0(r$property_type, "/", substr(r$built_form, 1, 4),
                                      "/", r$era, "/", r$fuel_class)
      cells <- vapply(m_treats, function(t) {
        rr <- cee[cell_id == r$cell_id & treatment_short_id == t]
        if (nrow(rr) == 0L) "--" else fmt_m(rr$beta[1L], rr$support[1L])
      }, character(1L))
      lines <- c(lines, sprintf("A%d & %s & %.1f & %s \\\\", r$arch_rank, tex_esc(desc),
                                r$base_epc, paste(cells, collapse = " & ")))
    }
    # Council-stock reference rows (representative social housing), labelled block
    if ("is_reference" %in% names(cee)) {
      refm <- unique(cee[is_reference == TRUE & !is.na(ref_label), .(cell_id, ref_label)])
      if (nrow(refm) > 0L) {
        refm <- merge(refm, bll[, .(cell_id, base_epc = baseline_current_energy_efficiency)],
                      by = "cell_id")
        setorder(refm, ref_label)
        lines <- c(lines, "\\midrule",
                   paste0("\\multicolumn{", 3 + length(m_treats),
                          "}{l}{\\textit{Council-stock reference (representative social housing)}} \\\\"))
        for (i in seq_len(nrow(refm))) {
          r <- refm[i]
          cells <- vapply(m_treats, function(t) {
            rr <- cee[cell_id == r$cell_id & treatment_short_id == t]
            if (nrow(rr) == 0L) "--" else fmt_m(rr$beta[1L], rr$support[1L])
          }, character(1L))
          rlab <- if (grepl("flat", r$ref_label, ignore.case = TRUE)) "Council flat" else "Council semi"
          lines <- c(lines, sprintf("R & %s & %.1f & %s \\\\", tex_esc(rlab),
                                    r$base_epc, paste(cells, collapse = " & ")))
        }
      }
    }
    lines <- c(lines,
               "\\bottomrule", "\\end{tabular}",
               "\\begin{minipage}{0.95\\textwidth}", "\\vspace{4pt}", "\\footnotesize",
               "\\textit{Notes:} Each cell is the matched-pair CATE (treated $-$ control) in EPC",
               "score for that ownership type, holding the property fixed at the row archetype;",
               "positive = more efficient than the private-individual control. Baseline = control-pool",
               "mean EPC of the archetype (the level the effect shifts from). Effects are estimated",
               "per owner and flagged, not dropped: $^{\\dagger}$ = suspect (thin / few-cluster support,",
               "still reported); -- = no matched support. Archetypes (A) are the diverse representative",
               "control-pool cells of Table~\\ref{tab:hte_archetypes}; the council-stock reference (R)",
               "rows anchor the public / non-profit effect on representative social housing (the",
               "post-war gas semi and a post-war purpose-built gas flat; the flat's RdSAP built form",
               "is `semi-detached', i.e.\\ one party wall, not a house typology). Baseline spec, base",
               "core, LA-clustered. Source: \\texttt{hte\\_archetype\\_matrix\\_LA.csv} + \\texttt{hte\\_cell\\_baselines\\_LA.csv} (06c).",
               "\\end{minipage}", "\\end{table}")
    write_tex(lines, "hte_archetype_matrix.tex")
  } else {
    cat("Skipped hte_archetype_matrix.tex (no CEE matrix rows)\n")
  }
} else {
  cat("Skipped hte_archetype_matrix.tex (need hte_archetype_matrix + baselines + defs; run 06c)\n")
}

# --- Table H2: ATT vs reweighted ATT vs composition effect ---
if (file.exists(hte_rw_path)) {
  hr <- fread(hte_rw_path, na.strings = c("NA", ""))
  h2_treats <- c("fp", "ukfp", "frfp", "np", "th", "ps")
  h2_labels <- c(fp = "For-profit (all)", ukfp = "UK for-profit",
                 frfp = "Foreign for-profit", np = "Non-profit",
                 th = "Tax haven (all)", ps = "Public sector")
  for (oc in c("current_energy_efficiency", "bad_epc_c")) {
    hs <- hr[outcome == oc & treatment_short_id %in% h2_treats]
    hs <- hs[match(h2_treats, treatment_short_id)][!is.na(treatment_short_id)]
    if (nrow(hs) == 0L) next
    oc_lab <- if (oc == "bad_epc_c") "Below EPC C" else "EPC Score"
    mult <- if (oc == "bad_epc_c") 100 else 1
    unit <- if (oc == "bad_epc_c") " (pp)" else " (SAP pts)"
    lines <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      sprintf("\\caption{Composition vs.\\ Treatment: Reweighted Effects, %s}", oc_lab),
      sprintf("\\label{tab:hte_reweighted_%s}", oc),
      "\\small",
      "\\begin{tabular}{lrrrrr}",
      "\\toprule",
      sprintf("Treatment & ATT%s & $\\tau_{rw}$ (full) & $\\tau_{rw}$ (10 arch.) & Composition & Wald $p$ \\\\", unit),
      "\\midrule"
    )
    for (i in seq_len(nrow(hs))) {
      r <- hs[i]
      lines <- c(lines, sprintf("%s & %s%s & %s (%s) & %s (%s) & %s & %s \\\\",
                                h2_labels[r$treatment_short_id],
                                fmt_coef(r$att * mult), fmt_stars(r$att_p),
                                fmt_coef(r$tau_rw_full * mult),
                                fmt_std(r$tau_rw_full_se * mult),
                                fmt_coef(r$tau_rw_arch10 * mult),
                                fmt_std(r$tau_rw_arch10_se * mult),
                                fmt_coef(r$composition_effect * mult),
                                fmt_p(r$wald_p)))
    }
    lines <- c(lines,
               "\\bottomrule",
               "\\end{tabular}",
               "\\begin{minipage}{0.95\\textwidth}",
               "\\vspace{4pt}",
               "\\footnotesize",
               "\\textit{Notes:} Matched-pair design, Baseline spec, base core, LA-clustered.",
               "ATT = pooled pair-difference estimate (identical to the PSM + Subclass FE",
               "coefficient). $\\tau_{rw}$ = cell CATEs reweighted to the control-pool covariate",
               "distribution (delta-method SEs in parentheses); full = all estimated cells,",
               "10 arch. = the diverse narrative archetype set (Table~\\ref{tab:hte_archetypes}).",
               "Composition = ATT $-$ $\\tau_{rw}$ (full):",
               "the part of the pooled effect attributable to the treated group's covariate mix.",
               "Wald $p$: test that all cell CATEs are equal.",
               if (oc == "bad_epc_c") "Coefficients in percentage points." else "",
               "Source: \\texttt{hte\\_reweighted\\_LA.csv} (script 06c).",
               "\\end{minipage}",
               "\\end{table}")
    write_tex(lines, sprintf("hte_reweighted_%s.tex", oc))
  }
} else {
  cat("Skipped hte_reweighted tables (no hte_reweighted_LA.csv; run 06c)\n")
}

# --- Table H3: honest tree terminal nodes (fp / frfp, bad_epc_c; appendix) ---
if (file.exists(hte_tree_path)) {
  ht <- fread(hte_tree_path, na.strings = c("NA", ""))
  hts <- ht[treatment_short_id %in% c("fp", "frfp") & outcome == "bad_epc_c" & !is.na(beta)]
  if (nrow(hts) > 0L) {
    lines <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      "\\caption{Honest-Tree Terminal Nodes: Below EPC C (For-Profit and Foreign For-Profit)}",
      "\\label{tab:hte_tree_nodes}",
      "\\scriptsize",
      "\\begin{tabular}{llrrrr}",
      "\\toprule",
      "Treatment & Node rule & $N$ pairs & CATE (pp) & SE (pp) & $p$ \\\\",
      "\\midrule"
    )
    treat_lab <- c(fp = "For-profit", frfp = "Foreign FP")
    for (i in seq_len(nrow(hts))) {
      r <- hts[i]
      rule <- tex_esc(substr(gsub(">=", "$\\\\geq$", r$node_rule), 1, 80))
      lines <- c(lines, sprintf("%s & %s & %s & %s%s & %s & %s \\\\",
                                treat_lab[r$treatment_short_id],
                                rule, fmt_n(r$n_est),
                                fmt_coef(r$beta * 100), fmt_stars(r$p_value),
                                fmt_std(r$se * 100), fmt_p(r$p_value)))
    }
    lines <- c(lines,
               "\\bottomrule",
               "\\end{tabular}",
               "\\begin{minipage}{0.95\\textwidth}",
               "\\vspace{4pt}",
               "\\footnotesize",
               "\\textit{Notes:} Honest CART: the tree is grown on a random half of the matched",
               "pairs (ITL1-stratified split; rpart, max depth 4, min bucket 200, 1-SE pruned)",
               "and node CATEs are re-estimated on the held-out half with LA-clustered SEs.",
               "Outcome: pair difference in below-EPC-C status; coefficients in percentage points.",
               "Source: \\texttt{hte\\_tree\\_nodes\\_LA.csv} (script 06c).",
               "\\end{minipage}",
               "\\end{table}")
    write_tex(lines, "hte_tree_nodes.tex")
  } else {
    cat("Skipped hte_tree_nodes.tex (no estimable fp/frfp bad_epc_c nodes)\n")
  }
} else {
  cat("Skipped hte_tree_nodes.tex (no hte_tree_nodes_LA.csv; run 06c)\n")
}

cat("\n=== All tables generated ===\n")
