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

cat("\n=== All tables generated ===\n")
