# Script: archetype_names.R
# Purpose: Deterministic short, human-readable names for the HTE archetypes,
#          derived from their covariates (era adjective + optional fuel +
#          built-form noun), with a floor-tercile qualifier appended only to
#          break ties within the set. Sourced by the exhibit scripts so the
#          forest plot, the archetype x ownership matrix and the covariate
#          legend all use one consistent naming.
# Authors: Dmytro Kunchenko
# Date: July 9, 2026.

.era_adjective <- function(age_band) {
  yr <- suppressWarnings(as.integer(sub(".*?([0-9]{4}).*", "\\1", age_band)))
  before1900 <- grepl("before 1900", age_band, ignore.case = TRUE)
  data.table::fcase(
    before1900,                    "Victorian",
    !is.na(yr) & yr <= 1929,       "Edwardian",
    !is.na(yr) & yr <= 1949,       "Interwar",
    !is.na(yr) & yr <= 1966,       "Post-war",
    !is.na(yr) & yr <= 1982,       "1970s",
    !is.na(yr) & yr <= 1990,       "1980s",
    !is.na(yr) & yr <= 2002,       "1990s",
    !is.na(yr),                    "New-build",
    default = "Other-era")
}

.form_noun <- function(property_type, built_form) {
  data.table::fcase(
    property_type == "Flat",                                       "flat",
    property_type == "Bungalow",                                   "bungalow",
    property_type == "Maisonette",                                 "maisonette",
    property_type == "House" & grepl("Terrace", built_form),       "terrace",
    property_type == "House" & grepl("Semi", built_form),          "semi",
    property_type == "House" & grepl("Detached", built_form),      "detached",
    default = tolower(property_type))
}

.fuel_qual <- function(main_fuel) {
  data.table::fcase(
    grepl("electric", main_fuel, ignore.case = TRUE), "electric ",
    grepl("oil",      main_fuel, ignore.case = TRUE), "oil ",
    default = "")  # mains gas is the default: no qualifier
}

# Vectorised over a data.table/data.frame `dt` with columns property_type,
# built_form, construction_age_band, main_fuel, floor_tercile. Returns a short
# name per row; base names that collide within `dt` get their tercile appended.
make_archetype_names <- function(dt) {
  era  <- .era_adjective(dt$construction_age_band)
  noun <- .form_noun(dt$property_type, dt$built_form)
  fuel <- .fuel_qual(dt$main_fuel)
  base <- paste0(era, " ", fuel, noun)
  dup  <- base %in% base[duplicated(base)]
  ifelse(dup & !is.na(dt$floor_tercile), paste0(base, " (", dt$floor_tercile, ")"), base)
}
