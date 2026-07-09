# Script: 08b_offshore_nonprofit_profiles.R
# Purpose: Descriptive deep-dive on foreign non-profit and tax-haven owners
#          (PI feedback, workstream 4): universe tables by LA / jurisdiction /
#          region, top proprietors, outlier properties, and a salience-scored
#          case-study shortlist for qualitative follow-up.
#
#          Off the regression path (08x numbering). Streams per-LA parquets
#          with column projection; no matching or estimation dependencies.
#
# PRIVACY: uses company-level PUBLIC REGISTER data only (CCOD/OCOD proprietor
# names are registered companies). No individual beneficial-owner names are
# read or written. bo_* / owner_type columns, where present, are typology
# flags without names.
#
# Outputs (output/offshore_profiles/):
#   universe_by_la.csv, universe_by_jurisdiction.csv, universe_by_region.csv,
#   top_proprietors.csv, outliers.csv, case_studies.csv
# Plus LaTeX fragments in tables/:
#   offshore_universe_jurisdiction.tex, offshore_top_proprietors.tex
#
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 7, 2026.
rm(list = setdiff(ls(), c("script", "pipeline.start.time")))
gc()

set.seed(20230703)
start.time <- Sys.time()

library(data.table)
library(arrow)
library(here)

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "treatment_definitions.R"))

PROFILE_DIR <- file.path(OUTPUT_DIR, "offshore_profiles")
dir.create(PROFILE_DIR, showWarnings = FALSE, recursive = TRUE)

message("===================================================================")
message("  08b_offshore_nonprofit_profiles.R - Offshore / Non-Profit Profiles")
message(sprintf("  Started: %s", format(start.time, "%Y-%m-%d %H:%M:%S")))
message("===================================================================")


# 1. Column selection ------------------------------------------------------
# Treatment-definition inputs + identifiers + descriptive attributes.
wanted_cols <- c(
  # Identifiers / location
  "uprn", "title_number", "local_authority", "local_authority_label",
  "ITL1", "ITL1_name", "property_address", "admin_postcode", "posttown",
  # Proprietor (public register, company-level)
  "proprietor_name_1", "proprietor_name_2", "proprietor_name_3", "proprietor_name_4",
  "company_registration_no_1", "proprietorship_category_1",
  "country_incorporated_1", "date_proprietor_added",
  "additional_proprietor_indicator", "multiple_address_indicator",
  # Treatment-definition inputs
  "source", "tenure_2", "coarse_proprietorship",
  "country_incorporated_tax_haven", "country_incorporated_british_haven",
  "country_incorporated_european_haven", "country_incorporated_caribbean_haven",
  "country_incorporated_other_haven",
  # EPC / property attributes
  "current_energy_efficiency", "current_energy_rating",
  "potential_energy_efficiency", "energy_efficiency_potential_gap",
  "total_floor_area", "number_habitable_rooms", "property_type", "built_form",
  "construction_age_band", "lodgement_year",
  # Price
  "price", "ppd_price_sqm", "ppd_year_transfer",
  # Owner-typology patch columns (optional; guarded by intersect below)
  "owner_type", "bo_offshore", "bo_nat_class", "owner_reg_year"
)

la_files <- list.files(EPC_LA_REFINED_DIR, pattern = "\\.parquet$", full.names = TRUE)
unknown_mask <- grepl("unknown", basename(la_files), ignore.case = TRUE)
if (any(unknown_mask)) la_files <- la_files[!unknown_mask]
message(sprintf("  %d per-LA parquet files.", length(la_files)))

schema_cols <- names(arrow::open_dataset(la_files[1], format = "parquet"))
sel_cols <- intersect(wanted_cols, schema_cols)
missing_cols <- setdiff(wanted_cols, sel_cols)
if (length(missing_cols) > 0L) {
  message(sprintf("  NOTE: %d requested columns absent (skipped): %s",
                  length(missing_cols), paste(missing_cols, collapse = ", ")))
}


# 2. Stream parquets: collect group members + control aggregates -----------
# Groups (via define_treatments):
#   frnp = foreign non-profit          (treat_foreign_non_profit == 1)
#   thnp = tax-haven non-profit        (treat_tax_haven_non_profit == 1)
#   th   = tax haven, all              (treat_tax_haven == 1)
# Reference: private-rental control (treat_tax_haven == 0), aggregates only.
group_defs <- list(
  frnp = "treat_foreign_non_profit",
  thnp = "treat_tax_haven_non_profit",
  th   = "treat_tax_haven"
)

members_list <- vector("list", length(la_files))
control_agg  <- vector("list", length(la_files))
t_stream <- Sys.time()

summarise_stats <- function(d) {
  list(
    n_properties  = uniqueN(d$uprn),
    n_titles      = if ("title_number" %in% names(d)) uniqueN(na.omit(d$title_number)) else NA_integer_,
    mean_epc      = mean(d$current_energy_efficiency, na.rm = TRUE),
    median_epc    = median(as.numeric(d$current_energy_efficiency), na.rm = TRUE),
    share_bad_epc_c = mean(d$current_energy_efficiency < 69, na.rm = TRUE),
    share_bad_epc_e = mean(d$current_energy_efficiency < 39, na.rm = TRUE),
    mean_floor_area = mean(d$total_floor_area, na.rm = TRUE),
    median_price  = median(as.numeric(d$price), na.rm = TRUE),
    n_price_valid = sum(!is.na(d$price))
  )
}

for (fi in seq_along(la_files)) {
  f <- la_files[fi]
  dt <- as.data.table(arrow::read_parquet(f, col_select = dplyr::all_of(sel_cols)))
  if (nrow(dt) == 0L) next
  # Deduplicate by UPRN (multiple EPC lodgements per property)
  if (anyDuplicated(dt$uprn)) dt <- dt[!duplicated(uprn)]
  define_treatments(dt)

  # Union of group members (a property can be in several groups)
  is_member <- rep(FALSE, nrow(dt))
  for (g in names(group_defs)) {
    gv <- dt[[group_defs[[g]]]]
    dt[, paste0("grp_", g) := fifelse(!is.na(gv) & gv == 1L, 1L, 0L)]
    is_member <- is_member | (!is.na(gv) & gv == 1L)
  }
  mem <- dt[is_member]
  if (nrow(mem) > 0L) {
    drop_treats <- grep("^treat_", names(mem), value = TRUE)
    if (length(drop_treats) > 0L) mem[, (drop_treats) := NULL]
    members_list[[fi]] <- mem
  }

  # Control (private rental, unknown ownership) aggregates for reference
  ctrl <- dt[!is.na(treat_tax_haven) & treat_tax_haven == 0L]
  if (nrow(ctrl) > 0L) {
    control_agg[[fi]] <- as.data.table(c(
      list(local_authority = ctrl$local_authority[1L],
           local_authority_label = if ("local_authority_label" %in% names(ctrl))
             ctrl$local_authority_label[1L] else NA_character_,
           ITL1 = if ("ITL1" %in% names(ctrl)) ctrl$ITL1[1L] else NA_character_,
           group = "control_private_rental"),
      summarise_stats(ctrl)
    ))
  }
  rm(dt, mem, ctrl)

  if (fi %% 50L == 0L || fi == length(la_files)) {
    message(sprintf("  [%d/%d] parquets streamed (%.0f s)", fi, length(la_files),
                    as.numeric(difftime(Sys.time(), t_stream, units = "secs"))))
  }
}

members <- rbindlist(Filter(Negate(is.null), members_list), fill = TRUE)
ctrl_la <- rbindlist(Filter(Negate(is.null), control_agg), fill = TRUE)
rm(members_list, control_agg); gc()

message(sprintf("  Group members: %s properties (union across groups)",
                formatC(nrow(members), big.mark = ",")))
for (g in names(group_defs)) {
  message(sprintf("    %s: %s", g,
                  formatC(sum(members[[paste0("grp_", g)]] == 1L), big.mark = ",")))
}

if (nrow(members) == 0L) stop("No group members found — check treatment inputs.")

# Normalised proprietor name (registered company name; keep raw alongside)
squish <- function(x) gsub("\\s+", " ", trimws(x))
members[, proprietor_norm := squish(toupper(proprietor_name_1))]
members[proprietor_norm == "" | is.na(proprietor_norm), proprietor_norm := NA_character_]

# Jurisdiction sub-breakdown within tax havens
members[, haven_class := fcase(
  !is.na(country_incorporated_british_haven)   & country_incorporated_british_haven == 1,   "british",
  !is.na(country_incorporated_european_haven)  & country_incorporated_european_haven == 1,  "european",
  !is.na(country_incorporated_caribbean_haven) & country_incorporated_caribbean_haven == 1, "caribbean",
  !is.na(country_incorporated_other_haven)     & country_incorporated_other_haven == 1,     "other",
  default = NA_character_
)]

# Parse date_proprietor_added (admin format DD-MM-YYYY)
if ("date_proprietor_added" %in% names(members)) {
  members[, date_prop_added := as.Date(as.character(date_proprietor_added), format = "%d-%m-%Y")]
  if (all(is.na(members$date_prop_added))) {
    members[, date_prop_added := as.Date(as.character(date_proprietor_added))]
  }
} else {
  members[, date_prop_added := as.Date(NA)]
}

# Data-quality suspect flag (glitchy floor areas / prices are flagged, not headlined)
members[, data_quality_suspect := fifelse(
  (!is.na(total_floor_area) & (total_floor_area < 10 | total_floor_area > 1000)) |
    (!is.na(price) & price < 1000), TRUE, FALSE)]


# 3. Universe tables --------------------------------------------------------
group_long <- rbindlist(lapply(names(group_defs), function(g) {
  d <- members[get(paste0("grp_", g)) == 1L]
  d[, group := g]
  d
}))

# 3a. By LA (+ control reference rows)
universe_by_la <- group_long[, c(list(local_authority_label =
                                        if ("local_authority_label" %in% names(group_long))
                                          local_authority_label[1L] else NA_character_,
                                      ITL1 = if ("ITL1" %in% names(group_long)) ITL1[1L] else NA_character_),
                                 summarise_stats(.SD)),
                             by = .(local_authority, group)]
universe_by_la <- rbind(universe_by_la, ctrl_la, fill = TRUE)
setorder(universe_by_la, group, -n_properties, na.last = TRUE)
fwrite(universe_by_la, file.path(PROFILE_DIR, "universe_by_la.csv"))
message(sprintf("  universe_by_la.csv: %d rows", nrow(universe_by_la)))

# 3b. By jurisdiction of incorporation
universe_by_jurisdiction <- group_long[, c(
  summarise_stats(.SD),
  list(n_proprietors = uniqueN(na.omit(proprietor_norm)),
       n_las = uniqueN(local_authority),
       haven_class = haven_class[1L])
), by = .(group, country_incorporated_1)]
setorder(universe_by_jurisdiction, group, -n_properties)
fwrite(universe_by_jurisdiction, file.path(PROFILE_DIR, "universe_by_jurisdiction.csv"))
message(sprintf("  universe_by_jurisdiction.csv: %d rows", nrow(universe_by_jurisdiction)))

# 3c. By region (ITL1)
if ("ITL1" %in% names(group_long)) {
  universe_by_region <- group_long[, c(
    list(ITL1_name = if ("ITL1_name" %in% names(group_long)) ITL1_name[1L] else NA_character_),
    summarise_stats(.SD),
    list(n_proprietors = uniqueN(na.omit(proprietor_norm)))
  ), by = .(group, ITL1)]
  # Control reference aggregated from per-LA rows (approximate: sums/weighted means)
  setorder(universe_by_region, group, -n_properties)
  fwrite(universe_by_region, file.path(PROFILE_DIR, "universe_by_region.csv"))
  message(sprintf("  universe_by_region.csv: %d rows", nrow(universe_by_region)))
}

# 3d. Tax-haven sub-class breakdown (within group th)
haven_breakdown <- group_long[group == "th", c(
  summarise_stats(.SD),
  list(n_proprietors = uniqueN(na.omit(proprietor_norm)))
), by = .(haven_class)]
setorder(haven_breakdown, -n_properties)
fwrite(haven_breakdown, file.path(PROFILE_DIR, "universe_haven_breakdown.csv"))


# 4. Top proprietors ---------------------------------------------------------
TOP_N_PROPRIETORS <- 50L

top_proprietors <- group_long[!is.na(proprietor_norm), c(
  list(proprietor_name_raw = proprietor_name_1[1L],
       jurisdiction = names(sort(table(country_incorporated_1), decreasing = TRUE))[1L],
       category = if ("proprietorship_category_1" %in% names(group_long))
         names(sort(table(proprietorship_category_1), decreasing = TRUE))[1L] else NA_character_,
       company_registration_no = na.omit(company_registration_no_1)[1L],
       n_las = uniqueN(local_authority),
       date_added_min = suppressWarnings(min(date_prop_added, na.rm = TRUE)),
       date_added_max = suppressWarnings(max(date_prop_added, na.rm = TRUE))),
  summarise_stats(.SD)
), by = .(group, proprietor_norm)]
setorder(top_proprietors, group, -n_properties)
top_proprietors <- top_proprietors[, head(.SD, TOP_N_PROPRIETORS), by = group]
fwrite(top_proprietors, file.path(PROFILE_DIR, "top_proprietors.csv"))
message(sprintf("  top_proprietors.csv: %d rows (top %d per group)",
                nrow(top_proprietors), TOP_N_PROPRIETORS))


# 5. Outliers -----------------------------------------------------------------
# Property-level case columns reused for outliers + case studies
case_cols <- intersect(c(
  "group", "outlier_reason", "data_quality_suspect",
  "proprietor_name_1", "proprietor_norm", "company_registration_no_1",
  "country_incorporated_1", "haven_class", "proprietorship_category_1",
  "coarse_proprietorship",
  "property_address", "admin_postcode", "posttown", "local_authority",
  "local_authority_label", "title_number", "uprn",
  "date_proprietor_added", "price", "ppd_year_transfer", "ppd_price_sqm",
  "current_energy_efficiency", "current_energy_rating",
  "potential_energy_efficiency", "total_floor_area", "number_habitable_rooms",
  "property_type", "built_form", "construction_age_band", "lodgement_year",
  "owner_type", "bo_offshore", "bo_nat_class"
), c(names(group_long), "group", "outlier_reason"))

pick_outliers <- function(d, n = 5L) {
  outs <- list()
  # Worst EPC scores
  o <- head(d[!is.na(current_energy_efficiency)][order(current_energy_efficiency)], n)
  if (nrow(o) > 0L) { o[, outlier_reason := "worst_epc"]; outs[["w"]] <- o }
  # Largest floor area
  o <- head(d[!is.na(total_floor_area)][order(-total_floor_area)], n)
  if (nrow(o) > 0L) { o[, outlier_reason := "largest_floor_area"]; outs[["f"]] <- o }
  # Highest price
  o <- head(d[!is.na(price)][order(-price)], n)
  if (nrow(o) > 0L) { o[, outlier_reason := "highest_price"]; outs[["p"]] <- o }
  # Representative property (highest-price) of the largest portfolios
  port <- head(d[!is.na(proprietor_norm),
                 .(n_port = uniqueN(uprn)), by = proprietor_norm][order(-n_port)], n)
  o <- d[proprietor_norm %in% port$proprietor_norm][order(proprietor_norm, -fifelse(is.na(price), -Inf, as.numeric(price)))]
  o <- o[!duplicated(proprietor_norm)]
  if (nrow(o) > 0L) { o[, outlier_reason := "largest_portfolio_member"]; outs[["l"]] <- o }
  # Unusual combination: below-E EPC but a >= £1m sale price
  o <- head(d[!is.na(current_energy_efficiency) & current_energy_efficiency < 39 &
                !is.na(price) & price >= 1e6][order(-price)], n)
  if (nrow(o) > 0L) { o[, outlier_reason := "unusual_combo"]; outs[["u"]] <- o }
  rbindlist(outs, fill = TRUE)
}

outliers <- group_long[, pick_outliers(.SD), by = group]
outliers <- outliers[, intersect(case_cols, names(outliers)), with = FALSE]
fwrite(outliers, file.path(PROFILE_DIR, "outliers.csv"))
message(sprintf("  outliers.csv: %d rows", nrow(outliers)))


# 6. Case-study shortlist -----------------------------------------------------
# Salience score over the member union: portfolio size + bad EPC + price
# extremity. Final curation is manual; ~20 rows, distinct proprietors,
# data-quality suspects excluded.
cs <- unique(group_long, by = c("group", "uprn"))
cs <- cs[data_quality_suspect == FALSE]

port_sizes <- cs[!is.na(proprietor_norm), .(portfolio_n = uniqueN(uprn)), by = proprietor_norm]
cs <- merge(cs, port_sizes, by = "proprietor_norm", all.x = TRUE)
cs[is.na(portfolio_n), portfolio_n := 1L]

z <- function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  out <- (x - mean(x, na.rm = TRUE)) / s
  out[is.na(out)] <- 0
  out
}
med_lp <- median(log(cs$price[!is.na(cs$price) & cs$price > 0]), na.rm = TRUE)
cs[, salience_score :=
     z(log1p(portfolio_n)) +
     z(-as.numeric(current_energy_efficiency)) +
     z(abs(fifelse(!is.na(price) & price > 0, log(as.numeric(price)), med_lp) - med_lp))]

# Group priority: rarer groups (frnp, thnp) first so they are represented
setorder(cs, -salience_score)
shortlist <- rbindlist(list(
  head(cs[group == "frnp"], 7L),
  head(cs[group == "thnp"], 7L),
  head(cs[group == "th"], 10L)
), fill = TRUE)
shortlist <- shortlist[!duplicated(proprietor_norm) | is.na(proprietor_norm)]
shortlist[, outlier_reason := NA_character_]
shortlist_out <- shortlist[, intersect(c(case_cols, "portfolio_n", "salience_score"),
                                       names(shortlist)), with = FALSE]
fwrite(shortlist_out, file.path(PROFILE_DIR, "case_studies.csv"))
message(sprintf("  case_studies.csv: %d rows (salience-scored shortlist)", nrow(shortlist_out)))


# 7. QA: reconcile group totals against psm_n_eligible -----------------------
results_path <- file.path(SUMMARY_TABLES_DIR, "results_table_LA.csv")
if (file.exists(results_path)) {
  res <- tryCatch(fread(results_path, na.strings = c("NA", ""),
                        select = c("treatment_short_id", "regression_core",
                                   "psm_n_eligible", "model")),
                  error = function(e) NULL)
  if (!is.null(res)) {
    for (g in names(group_defs)) {
      elig <- res[treatment_short_id == g & regression_core == "base" &
                    !is.na(psm_n_eligible), psm_n_eligible][1L]
      n_here <- sum(members[[paste0("grp_", g)]] == 1L)
      if (!is.na(elig)) {
        ratio <- n_here / elig
        flag <- if (ratio > 0.5 && ratio < 2) "OK" else "CHECK"
        message(sprintf("  QA %s: profile n=%s vs psm_n_eligible=%s (ratio %.2f) [%s]",
                        g, formatC(n_here, big.mark = ","),
                        formatC(elig, big.mark = ","), ratio, flag))
      } else {
        message(sprintf("  QA %s: no psm_n_eligible row found in results_table.", g))
      }
    }
  }
} else {
  message("  QA skipped: results_table_LA.csv not found.")
}


# 8. LaTeX fragments ----------------------------------------------------------
tex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x
}
fmt_i <- function(x) ifelse(is.na(x), "--", formatC(x, format = "d", big.mark = ","))
fmt_1 <- function(x) ifelse(is.na(x) | is.nan(x), "--", formatC(x, format = "f", digits = 1))

group_labels <- c(frnp = "Foreign non-profit", thnp = "Tax-haven non-profit",
                  th = "Tax haven (all)")

# 8a. Universe by jurisdiction (top 15 jurisdictions within tax-haven group,
#     plus the two non-profit groups' totals)
th_jur <- head(universe_by_jurisdiction[group == "th"][order(-n_properties)], 15L)
lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Tax-Haven Owned Rental Properties by Jurisdiction of Incorporation}",
  "\\label{tab:offshore_universe_jurisdiction}",
  "\\small",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Jurisdiction & Properties & Titles & Proprietors & Mean EPC & Share $<$C \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(th_jur))) {
  r <- th_jur[i]
  lines <- c(lines, sprintf("%s & %s & %s & %s & %s & %s\\%% \\\\",
                            tex_escape(r$country_incorporated_1),
                            fmt_i(r$n_properties), fmt_i(r$n_titles),
                            fmt_i(r$n_proprietors), fmt_1(r$mean_epc),
                            fmt_1(r$share_bad_epc_c * 100)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.9\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} Properties in the tax-haven ownership group (IMF tax-haven",
           "jurisdiction lists applied to CCOD/OCOD country of incorporation), deduplicated",
           "by UPRN. Top 15 jurisdictions by property count. Share $<$C = share of properties",
           "below EPC band C (SAP $<$ 69). Source: \\texttt{universe\\_by\\_jurisdiction.csv} (script 08b).",
           "\\end{minipage}",
           "\\end{table}")
writeLines(lines, here::here("tables", "offshore_universe_jurisdiction.tex"))
message("  Written: tables/offshore_universe_jurisdiction.tex")

# 8b. Top proprietors (top 10 for tax-haven group + top 5 foreign non-profit)
top_tex <- rbind(head(top_proprietors[group == "th"], 10L),
                 head(top_proprietors[group == "frnp"], 5L))
lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Largest Tax-Haven and Foreign Non-Profit Proprietors (Public Register)}",
  "\\label{tab:offshore_top_proprietors}",
  "\\scriptsize",
  "\\begin{tabular}{llrrrr}",
  "\\toprule",
  "Group & Proprietor (registered company) & Properties & LAs & Mean EPC & Share $<$C \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(top_tex))) {
  r <- top_tex[i]
  lines <- c(lines, sprintf("%s & %s (%s) & %s & %s & %s & %s\\%% \\\\",
                            group_labels[r$group],
                            tex_escape(substr(r$proprietor_name_raw, 1, 45)),
                            tex_escape(r$jurisdiction),
                            fmt_i(r$n_properties), fmt_i(r$n_las),
                            fmt_1(r$mean_epc), fmt_1(r$share_bad_epc_c * 100)))
}
lines <- c(lines,
           "\\bottomrule",
           "\\end{tabular}",
           "\\begin{minipage}{0.95\\textwidth}",
           "\\vspace{4pt}",
           "\\footnotesize",
           "\\textit{Notes:} Proprietor names are registered company names from the public",
           "CCOD/OCOD registers (company-level data only; no individual beneficial owners).",
           "Properties = distinct UPRNs held under the (normalised) proprietor name.",
           "Source: \\texttt{top\\_proprietors.csv} (script 08b).",
           "\\end{minipage}",
           "\\end{table}")
writeLines(lines, here::here("tables", "offshore_top_proprietors.tex"))
message("  Written: tables/offshore_top_proprietors.tex")

rm(members, group_long, cs); gc()

end.time <- Sys.time()
message(sprintf("\n  08b_offshore_nonprofit_profiles.R complete. Runtime: %.1f min.",
                as.numeric(difftime(end.time, start.time, units = "mins"))))
