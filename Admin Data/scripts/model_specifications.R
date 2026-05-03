# Script: model_specifications.R
# Purpose: Define shared model specifications, filters, and configurations for matching and regressions.
# Used by: 05_create_matched_pairs.R, 06_run_regressions.R

# Core Filters ------------------------------------------------------------
# These define the subsets of data used for different analysis "Cores" (PAP Section 2.4)
matching_core_filters <- list(
  base = quote(rep(TRUE, .N)),
  council_tax = quote(!is.na(tax_band)),
  ppd = quote(!is.na(ppd_price_sqm)),
  ppd_counciltax = quote(!is.na(tax_band) & !is.na(ppd_price_sqm))
)

# Regression filters are currently identical to matching filters
regression_core_filters <- matching_core_filters

# Short IDs for core names (used in per-unit .rds filenames)
core_short_ids <- c(base = "b", council_tax = "ct", ppd = "ppd", ppd_counciltax = "pct")

# Specification Configurations ---------------------------------------------
# Defines the exact covariates used for each family of models
spec_configs <- list(
  list(
    name = "Baseline",
    short_id = "bl",
    continuous_vars = c("number_habitable_rooms", "total_floor_area"),
    exact_vars = c("lodgement_year", "property_type", "main_fuel", "construction_age_band", "built_form", "local_authority")
  ),
  list(
    name = "Council Tax",
    short_id = "ctax",
    continuous_vars = c("number_habitable_rooms", "total_floor_area"),
    exact_vars = c("lodgement_year", "property_type", "main_fuel", "tax_band", "construction_age_band", "built_form", "local_authority")
  ),
  list(
    name = "Price Paid",
    short_id = "ppd",
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "ppd_price_sqm"),
    exact_vars = c("lodgement_year", "property_type", "main_fuel", "ppd_year_transfer", "construction_age_band", "built_form", "local_authority")
  ),
  list(
    name = "Council Tax + Price Paid",
    short_id = "cp",
    continuous_vars = c("number_habitable_rooms", "total_floor_area", "ppd_price_sqm"),
    exact_vars = c("lodgement_year", "property_type", "main_fuel", "tax_band", "ppd_year_transfer", "construction_age_band", "built_form", "local_authority")
  )
)

# Reverse Lookups ---------------------------------------------------------
# Short ID → long name. Used by 05b and 06b to write/read long names.
core_long_from_short <- setNames(names(core_short_ids), core_short_ids)
spec_long_from_short <- setNames(
  vapply(spec_configs, `[[`, "", "name"),
  vapply(spec_configs, `[[`, "", "short_id")
)

# Valid Core Pairs --------------------------------------------------------
# Maps which cores are valid for which specification
spec_core_pairs <- list(
  Baseline = c("base", "council_tax", "ppd", "ppd_counciltax"),
  `Council Tax` = c("council_tax", "ppd_counciltax"),
  `Price Paid` = c("ppd", "ppd_counciltax"),
  `Council Tax + Price Paid` = c("ppd_counciltax")
)

# Construction-Age-Band Cutoff Helper -------------------------------------
# Used by 06_run_regressions.R when BUILD_YEAR_CUTOFF is set, to filter the
# matched dataset to rows whose construction-age band has a START year
# strictly less than the cutoff. Returns one integer per input element
# (NA for unparseable bands like "NO DATA!", "INVALID!", or empty strings —
# those rows are excluded by the < cutoff comparison).
#
# Handles four EPC band shapes seen in the parquet:
#   "England and Wales: before 1900"    -> 1899
#   "England and Wales: 1996-2002"      -> 1996
#   "England and Wales: 2007 onwards"   -> 2007
#   raw 4-digit year e.g. "2021"        -> 2021
construction_age_band_start_year <- function(x) {
  if (is.null(x) || length(x) == 0L) return(integer(0))
  s <- tolower(trimws(as.character(x)))
  s <- sub("^england and wales:\\s*", "", s)
  s <- sub("^scotland:\\s*",          "", s)
  s <- sub("^northern ireland:\\s*",  "", s)
  out <- rep(NA_integer_, length(s))
  out[s == "before 1900"] <- 1899L
  rng_idx <- grepl("^[0-9]{4}-[0-9]{4}$", s)
  if (any(rng_idx)) out[rng_idx] <- as.integer(sub("^([0-9]{4})-.*$", "\\1", s[rng_idx]))
  on_idx <- grepl("^[0-9]{4}\\s+onwards$", s)
  if (any(on_idx))  out[on_idx]  <- as.integer(sub("^([0-9]{4}).*$", "\\1", s[on_idx]))
  yr_idx <- grepl("^[0-9]{4}$", s)
  if (any(yr_idx))  out[yr_idx]  <- as.integer(s[yr_idx])
  out
}