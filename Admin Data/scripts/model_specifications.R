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