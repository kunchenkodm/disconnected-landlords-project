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

# Master list of treatments, file identifiers, short IDs, and titles used across matching and regressions.
treatment_metadata <- list(
  list(var = "treat_for_profit",           file_id = "for_profit_vs_private_rental",           short_id = "fp",   title = "Effect of For-Profit Ownership"),
  list(var = "treat_uk_for_profit",        file_id = "uk_for_profit_vs_private_rental",        short_id = "ukfp", title = "Effect of UK For-Profit Ownership"),
  list(var = "treat_foreign_for_profit",   file_id = "foreign_for_profit_vs_private_rental",   short_id = "frfp", title = "Effect of Foreign For-Profit Ownership"),
  list(var = "treat_tax_haven_for_profit", file_id = "tax_haven_for_profit_vs_private_rental", short_id = "thfp", title = "Effect of Tax Haven For-Profit Ownership"),
  list(var = "treat_non_profit",           file_id = "non_profit_vs_private_rental",           short_id = "np",   title = "Effect of Non-Profit Ownership"),
  list(var = "treat_uk_non_profit",        file_id = "uk_non_profit_vs_private_rental",        short_id = "uknp", title = "Effect of UK Non-Profit Ownership"),
  list(var = "treat_foreign_non_profit",   file_id = "foreign_non_profit_vs_private_rental",   short_id = "frnp", title = "Effect of Foreign Non-Profit Ownership"),
  list(var = "treat_tax_haven_non_profit", file_id = "tax_haven_non_profit_vs_private_rental", short_id = "thnp", title = "Effect of Tax Haven Non-Profit Ownership"),
  list(var = "treat_public_sector",        file_id = "public_sector_vs_private_rental",        short_id = "ps",   title = "Effect of Public Sector Ownership"),
  list(var = "treat_tax_haven",            file_id = "tax_haven_vs_private_rental",            short_id = "th",   title = "Effect of Tax Haven Ownership"),
  list(var = "treat_british_haven",        file_id = "british_haven_vs_private_rental",        short_id = "bh",   title = "Effect of British Haven Ownership"),
  list(var = "treat_european_haven",       file_id = "european_haven_vs_private_rental",       short_id = "eh",   title = "Effect of European Haven Ownership"),
  list(var = "treat_caribbean_haven",      file_id = "caribbean_haven_vs_private_rental",      short_id = "ch",   title = "Effect of Caribbean Haven Ownership"),
  list(var = "treat_other_haven",          file_id = "other_haven_vs_private_rental",          short_id = "oh",   title = "Effect of Other Haven Ownership")
)
