# Script: treatment_definitions.R
# Purpose: Define treatment variables for EPC data.
# Contains function `define_treatments` that adds treatment columns to a
# data.table.

define_treatments <- function(dt) {
  # Common control group: privately rented properties with unknown source
  control_group_condition <- quote(
    source == "Unknown" &
      grepl("rental \\(private\\)|Rented \\(private\\)", tenure_2, ignore.case = TRUE)
  )

  # For-Profit treatments
  dt[, treat_for_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE), 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_uk_for_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_foreign_for_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_tax_haven_for_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_tax_haven) & country_incorporated_tax_haven == 1, 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  # Non-Profit treatments
  dt[, treat_non_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE), 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_uk_non_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_foreign_non_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_tax_haven_non_profit := fcase(
    !is.na(coarse_proprietorship) & grepl("Non-Profit/Community Organisations", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_tax_haven) & country_incorporated_tax_haven == 1, 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  # Public sector treatment
  dt[, treat_public_sector := fcase(
    !is.na(coarse_proprietorship) & grepl("Public Sector", coarse_proprietorship, ignore.case = TRUE), 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  # Abroad vs Domestic (For-Profit only)
  dt[, treat_abroad_domestic := fcase(
    !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_1) & country_incorporated_1 != "UNITED KINGDOM", 1L,
    !is.na(coarse_proprietorship) & grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
      !is.na(country_incorporated_1) & country_incorporated_1 == "UNITED KINGDOM", 0L,
    default = NA_integer_
  )]

  # Tax haven treatments
  dt[, treat_tax_haven := fcase(
    country_incorporated_tax_haven == 1, 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_british_haven := fcase(
    country_incorporated_british_haven == 1, 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_european_haven := fcase(
    country_incorporated_european_haven == 1, 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_caribbean_haven := fcase(
    country_incorporated_caribbean_haven == 1, 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  dt[, treat_other_haven := fcase(
    country_incorporated_other_haven == 1, 1L,
    eval(control_group_condition), 0L,
    default = NA_integer_
  )]

  invisible(dt)
}

