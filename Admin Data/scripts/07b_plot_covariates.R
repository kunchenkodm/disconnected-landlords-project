# Script: 07b_plot_covariates.R
# Purpose: Visualise covariate distributions across the 14-treatment + control
#          hierarchy, pre-matching (raw) vs post-matching across multiple PSM
#          configurations (Baseline/base and the rich Council-Tax + Price-Paid
#          core). Supports Task 2.4 in next-steps.md.
# Approach: Stream per-LA parquet files to build accumulators, then render
#          stacked-bar (categorical) and violin/ridge (continuous) plots.
# Authors: Dmytro Kunchenko
# Date: 2026-04-18

rm(list = setdiff(ls(), c("script", "pipeline.start.time")))
gc()

start.time <- Sys.time()

source(here::here("scripts", "00_setup.R"))
source(here::here("scripts", "treatment_definitions.R"))
source(here::here("scripts", "model_specifications.R"))

library(data.table)
library(ggplot2)
library(arrow)

HAS_GGRIDGES <- requireNamespace("ggridges", quietly = TRUE)
if (HAS_GGRIDGES) library(ggridges)

# Membership tests use data.table::`%chin%` (hash-based) on character UPRNs.
# A previous binary-search helper using findInterval misclassified character
# UPRNs because findInterval's comparison is numeric while sort() is
# lexicographic, silently dropping valid hits.

# Runtime controls --------------------------------------------------------
# Trim these during iteration; all defaults reproduce the full paper output.
PLOT_GEO          <- Sys.getenv("PLOT_GEO",          "LA")
RESERVOIR_SIZE    <- as.integer(Sys.getenv("PLOT_RESERVOIR_SIZE", "50000"))
FORCE_RECOMPUTE   <- as.logical(Sys.getenv("PLOT_FORCE_RECOMPUTE", "FALSE"))
MAX_FILES         <- {
  v <- suppressWarnings(as.integer(Sys.getenv("PLOT_MAX_FILES", "")))
  if (is.na(v) || v <= 0L) NA_integer_ else v
}

# Spec covariate lookup (from model_specifications.R) --------------------
spec_cov_vars <- setNames(
  lapply(spec_configs, function(sc) unique(c(sc$continuous_vars, sc$exact_vars))),
  vapply(spec_configs, `[[`, "", "name")
)

# Panel configurations ----------------------------------------------------
# 5 panels from broadest (real raw) to most filtered (PSM matched).
# is_matched = TRUE panels additionally restrict rows to matched UPRN pools.
# core_filter = NULL  → no extra row filter beyond the treatment predicate.
# cov_vars = character(0) → no complete-case mask applied.
# NOTE: PSM panel N < regression psm_n_matched because the regression join
# (EPC_matched_combined[matched_dt, on="uprn"]) multiplies rows by subclass
# appearances; this accumulator counts unique matched UPRNs × EPC certs.
# Distribution shapes are correct; counts are unique-UPRN based.
all_panels <- list(
  list(
    key        = "raw",
    label      = "Raw (pre-regression)",
    is_matched = FALSE,
    core_filter = NULL,
    cov_vars   = character(0)
  ),
  list(
    key        = "ols_base",
    label      = "OLS (base / Baseline)",
    is_matched = FALSE,
    core_filter = matching_core_filters[["base"]],
    cov_vars   = spec_cov_vars[["Baseline"]]
  ),
  list(
    key        = "ols_pct",
    label      = "OLS (ppd_counciltax / CT+PPD)",
    is_matched = FALSE,
    core_filter = matching_core_filters[["ppd_counciltax"]],
    cov_vars   = spec_cov_vars[["Council Tax + Price Paid"]]
  ),
  list(
    key        = "psm_base_bl",
    label      = "PSM matched (base / Baseline)",
    is_matched = TRUE,
    core       = "base",
    spec_name  = "Baseline",
    core_filter = matching_core_filters[["base"]],
    cov_vars   = spec_cov_vars[["Baseline"]]
  ),
  list(
    key        = "psm_pct_cp",
    label      = "PSM matched (ppd_counciltax / CT+PPD)",
    is_matched = TRUE,
    core       = "ppd_counciltax",
    spec_name  = "Council Tax + Price Paid",
    core_filter = matching_core_filters[["ppd_counciltax"]],
    cov_vars   = spec_cov_vars[["Council Tax + Price Paid"]]
  )
)

# Output directory --------------------------------------------------------
output_dir <- file.path(FIGURES_DIR, "covariates")
cache_dir  <- file.path(output_dir, "_accum_cache")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir,  recursive = TRUE, showWarnings = FALSE)

# Input locations ---------------------------------------------------------
la_refined_dir   <- EPC_LA_REFINED_DIR
matched_data_dir <- file.path(MATCHED_DATA_DIR, PLOT_GEO)
ccod_version     <- CCOD_VERSION

# Treatment display configuration ----------------------------------------
# Fixed vertical order: control first, then For-Profit family, Non-Profit,
# Public, Tax-Haven cross-cut. `depth` controls indentation; `family` controls
# colour; `predicate` is a quoted expression evaluated in a data.table with
# treatment indicator columns from define_treatments() (returns a logical vector).

build_display_config <- function() {
  list(
    list(key = "control",     label = "Privately rented, Unknown",
         family = "Control",   depth = 0,
         predicate = quote(source == "Unknown" &
                           grepl("rental \\(private\\)|Rented \\(private\\)",
                                 tenure_2, ignore.case = TRUE))),

    list(key = "fp",    label = "For-Profit (all)",
         family = "For-Profit", depth = 0, bold = TRUE,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE))),
    list(key = "ukfp",  label = "UK For-Profit",
         family = "For-Profit", depth = 1,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
                           !is.na(country_incorporated_1) &
                           country_incorporated_1 == "UNITED KINGDOM")),
    list(key = "frfp",  label = "Foreign For-Profit",
         family = "For-Profit", depth = 1,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
                           !is.na(country_incorporated_1) &
                           country_incorporated_1 != "UNITED KINGDOM")),
    list(key = "thfp",  label = "Tax-Haven For-Profit",
         family = "For-Profit", depth = 2,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("For-Profit", coarse_proprietorship, ignore.case = TRUE) &
                           !is.na(country_incorporated_tax_haven) &
                           country_incorporated_tax_haven == 1)),

    list(key = "np",    label = "Non-Profit / Community (all)",
         family = "Non-Profit", depth = 0, bold = TRUE,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("Non-Profit/Community Organisations",
                                 coarse_proprietorship, ignore.case = TRUE))),
    list(key = "uknp",  label = "UK Non-Profit",
         family = "Non-Profit", depth = 1,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("Non-Profit/Community Organisations",
                                 coarse_proprietorship, ignore.case = TRUE) &
                           !is.na(country_incorporated_1) &
                           country_incorporated_1 == "UNITED KINGDOM")),
    list(key = "frnp",  label = "Foreign Non-Profit",
         family = "Non-Profit", depth = 1,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("Non-Profit/Community Organisations",
                                 coarse_proprietorship, ignore.case = TRUE) &
                           !is.na(country_incorporated_1) &
                           country_incorporated_1 != "UNITED KINGDOM")),
    list(key = "thnp",  label = "Tax-Haven Non-Profit",
         family = "Non-Profit", depth = 2,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("Non-Profit/Community Organisations",
                                 coarse_proprietorship, ignore.case = TRUE) &
                           !is.na(country_incorporated_tax_haven) &
                           country_incorporated_tax_haven == 1)),

    list(key = "ps",    label = "Public Sector",
         family = "Public", depth = 0, bold = TRUE,
         predicate = quote(!is.na(coarse_proprietorship) &
                           grepl("Public Sector", coarse_proprietorship, ignore.case = TRUE))),

    list(key = "th",    label = "Tax Haven (any)",
         family = "Tax-Haven", depth = 0, bold = TRUE,
         predicate = quote(!is.na(country_incorporated_tax_haven) &
                           country_incorporated_tax_haven == 1)),
    list(key = "bh",    label = "British Haven",
         family = "Tax-Haven", depth = 1,
         predicate = quote(!is.na(country_incorporated_british_haven) &
                           country_incorporated_british_haven == 1)),
    list(key = "eh",    label = "European Haven",
         family = "Tax-Haven", depth = 1,
         predicate = quote(!is.na(country_incorporated_european_haven) &
                           country_incorporated_european_haven == 1)),
    list(key = "ch",    label = "Caribbean Haven",
         family = "Tax-Haven", depth = 1,
         predicate = quote(!is.na(country_incorporated_caribbean_haven) &
                           country_incorporated_caribbean_haven == 1)),
    list(key = "oh",    label = "Other Haven",
         family = "Tax-Haven", depth = 1,
         predicate = quote(!is.na(country_incorporated_other_haven) &
                           country_incorporated_other_haven == 1))
  )
}

treatment_display_config <- build_display_config()

# Treatment-file_id mapping (matches treatment_metadata) -----------------
# Used only for loading matched .RData; "control" is not a treatment file.
treat_file_id <- list(
  fp    = "for_profit_vs_private_rental",
  ukfp  = "uk_for_profit_vs_private_rental",
  frfp  = "foreign_for_profit_vs_private_rental",
  thfp  = "tax_haven_for_profit_vs_private_rental",
  np    = "non_profit_vs_private_rental",
  uknp  = "uk_non_profit_vs_private_rental",
  frnp  = "foreign_non_profit_vs_private_rental",
  thnp  = "tax_haven_non_profit_vs_private_rental",
  ps    = "public_sector_vs_private_rental",
  th    = "tax_haven_vs_private_rental",
  bh    = "british_haven_vs_private_rental",
  eh    = "european_haven_vs_private_rental",
  ch    = "caribbean_haven_vs_private_rental",
  oh    = "other_haven_vs_private_rental"
)

# Family colour palette ---------------------------------------------------
family_colors <- c(
  "Control"    = "grey40",
  "For-Profit" = "#1f77b4",
  "Non-Profit" = "#2ca02c",
  "Public"     = "#ff7f0e",
  "Tax-Haven"  = "#9467bd"
)

# Indented row labels (unicode NBSP so ggplot respects spacing) ----------
make_display_label <- function(cfg_entry) {
  indent <- strrep("\u00a0\u00a0\u00a0", cfg_entry$depth)  # 3 NBSPs per depth
  paste0(indent, cfg_entry$label)
}

# Covariate lists ---------------------------------------------------------
# `number_habitable_rooms` is integer-valued (typically 1-10) so it is
# plotted as a discrete distribution alongside the other categorical
# covariates rather than as a density ridge.
categorical_covariates <- c(
  "construction_age_band",
  "property_type",
  "built_form",
  "main_fuel",
  "tax_band",
  "current_energy_rating",
  "number_habitable_rooms"
)

continuous_covariates <- c(
  "lodgement_year",
  "total_floor_area",
  "ppd_price_sqm",
  "current_energy_efficiency"
)

# Helper: structural columns needed from parquet -------------------------
ownership_cols <- c(
  "source", "tenure_2", "coarse_proprietorship", "country_incorporated_1",
  "country_incorporated_tax_haven", "country_incorporated_british_haven",
  "country_incorporated_european_haven", "country_incorporated_caribbean_haven",
  "country_incorporated_other_haven"
)

all_covariates <- unique(c(categorical_covariates, continuous_covariates))

# Consolidated main_fuel categories ---------------------------------------
# EPC `main_fuel` has ~28 near-duplicate variants (community/non-community,
# backward-compat clones, unspecified tariffs, etc.). Matching uses the raw
# column, but for display we collapse to ~7 interpretable buckets.
consolidate_main_fuel <- function(x) {
  x <- as.character(x)
  out <- rep("Other / unspecified", length(x))
  lx <- tolower(x)
  out[grepl("mains gas", lx)]                              <- "Mains gas"
  out[grepl("lpg|bottled", lx)]                            <- "LPG / bottled gas"
  out[grepl("electricity", lx)]                            <- "Electricity"
  out[grepl("^oil|oil - this|oil \\(|oil$", lx)]           <- "Oil"
  out[grepl("coal|anthracite|smokeless", lx)]              <- "Solid fuel (coal)"
  out[grepl("wood|biomass|pellet", lx)]                    <- "Biomass / wood"
  out[grepl("heat network|community.*b30d|b30d", lx)]      <- "Heat network"
  out[grepl("no heating|no.hot.water|invalid|no data", lx)] <- "None / invalid"
  out[is.na(x)] <- NA_character_
  out
}

# Categorical level ordering & palettes ---------------------------------
# Explicit per-covariate level order. Real/valid levels appear first in the
# listed order; any unlisted real levels are appended alphabetically; the
# invalid/missing bucket always sorts last and is always rendered in grey.
covariate_level_orders <- list(
  construction_age_band = c(
    "Before 1900", "1900-1929", "1930-1949", "1950-1966", "1967-1975",
    "1976-1982",   "1983-1990", "1991-1995", "1996-2002", "2003-2006",
    "2007-2011",   "2012 onwards"
  ),
  current_energy_rating  = c("A", "B", "C", "D", "E", "F", "G"),
  tax_band               = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
  property_type          = c("House", "Flat", "Bungalow", "Maisonette",
                             "Park home"),
  built_form             = c("Detached", "Semi-Detached", "End-Terrace",
                             "Mid-Terrace", "Enclosed End-Terrace",
                             "Enclosed Mid-Terrace"),
  main_fuel              = c("Mains gas", "LPG / bottled gas", "Electricity",
                             "Oil", "Solid fuel (coal)", "Biomass / wood",
                             "Heat network", "Other / unspecified"),
  number_habitable_rooms = c("0", "1", "2", "3", "4", "5", "6", "7", "8",
                             "9", "10+")
)

INVALID_LEVELS <- c("(missing)", "(missing/invalid)", "None / invalid",
                    "INVALID!", "NO DATA!", "Unknown", "NA")
INVALID_COLOR  <- "grey75"

is_invalid_level <- function(x) x %in% INVALID_LEVELS

order_levels <- function(cov, observed) {
  observed  <- unique(observed)
  invalid   <- observed[is_invalid_level(observed)]
  real      <- setdiff(observed, invalid)
  preferred <- covariate_level_orders[[cov]]
  if (!is.null(preferred)) {
    real_ord <- c(intersect(preferred, real), sort(setdiff(real, preferred)))
  } else {
    real_ord <- sort(real)
  }
  c(real_ord, sort(invalid))
}

# Sequential palette for ordinal covariates, qualitative for nominal.
# Invalid levels always map to INVALID_COLOR.
build_level_palette <- function(cov, levels) {
  invalid <- levels[is_invalid_level(levels)]
  real    <- setdiff(levels, invalid)
  n       <- length(real)
  pal_real <- if (n == 0L) character(0)
    else if (cov == "construction_age_band")
      grDevices::hcl.colors(n, palette = "Viridis")
    else if (cov == "current_energy_rating")
      grDevices::hcl.colors(n, palette = "RdYlGn", rev = TRUE)
    else if (cov == "tax_band")
      grDevices::hcl.colors(n, palette = "Plasma")
    else if (cov == "number_habitable_rooms") {
      # 0 rooms is implausible/data-quality — render in red so it is
      # visibly distinct from the dark end of the sequential 1-10+ ramp.
      is_zero <- real == "0"
      pal     <- character(length(real))
      pal[is_zero] <- "#D62728"
      n_seq <- sum(!is_zero)
      if (n_seq > 0L)
        pal[!is_zero] <- grDevices::hcl.colors(n_seq, palette = "Mako",
                                               rev = TRUE)
      pal
    }
    else
      grDevices::hcl.colors(max(n, 3L), palette = "Set 2")[seq_len(n)]
  out <- setNames(rep(INVALID_COLOR, length(levels)), levels)
  out[real] <- pal_real
  out
}

# EPC `construction_age_band` has 12 canonical England-and-Wales bands plus
# legacy junk: "INVALID!", "NO DATA!", "(missing)", and stray year codes.
# Matching uses the raw column; for display we collapse to the 12 canonical
# bands plus a single "(missing/invalid)" bucket.
# `number_habitable_rooms` is integer-valued. Cap the long right tail at
# "10+" so the legend stays readable; preserve missing values as NA so they
# fall into the "(missing)" bucket via merge_cat_counts.
consolidate_habitable_rooms <- function(x) {
  v <- suppressWarnings(as.integer(x))
  out <- as.character(v)
  out[!is.na(v) & v >= 10L] <- "10+"
  out[is.na(v)] <- NA_character_
  out
}

consolidate_construction_age <- function(x) {
  x_chr <- as.character(x)
  lx <- tolower(x_chr)
  out <- rep(NA_character_, length(x_chr))
  out[grepl("before 1900",  lx)] <- "Before 1900"
  out[grepl("1900-1929",    lx)] <- "1900-1929"
  out[grepl("1930-1949",    lx)] <- "1930-1949"
  out[grepl("1950-1966",    lx)] <- "1950-1966"
  out[grepl("1967-1975",    lx)] <- "1967-1975"
  out[grepl("1976-1982",    lx)] <- "1976-1982"
  out[grepl("1983-1990",    lx)] <- "1983-1990"
  out[grepl("1991-1995",    lx)] <- "1991-1995"
  out[grepl("1996-2002",    lx)] <- "1996-2002"
  out[grepl("2003-2006",    lx)] <- "2003-2006"
  out[grepl("2007-2011|2007 onwards", lx)] <- "2007-2011"
  out[grepl("2012 onwards", lx)] <- "2012 onwards"
  out[is.na(out)]   <- "(missing/invalid)"
  out[is.na(x_chr)] <- NA_character_
  out
}

# Reservoir sampling (Vitter's Algorithm R, streaming) -------------------
# Usage:
#   res <- reservoir_new(size)
#   res <- reservoir_update(res, new_values)
#   values <- reservoir_values(res)
# Maintains a fixed-size simple random sample from an unbounded stream.
reservoir_new <- function(size) {
  list(buffer = rep(NA_real_, size), size = size, seen = 0L)
}

reservoir_update <- function(res, new_values) {
  new_values <- new_values[!is.na(new_values)]
  if (!length(new_values)) return(res)
  n <- length(new_values)
  size <- res$size
  start_seen <- res$seen
  end_seen <- start_seen + n

  if (end_seen <= size) {
    res$buffer[(start_seen + 1L):end_seen] <- new_values
  } else if (start_seen < size) {
    take <- size - start_seen
    res$buffer[(start_seen + 1L):size] <- new_values[seq_len(take)]
    remaining <- new_values[(take + 1L):n]
    idx_in_stream <- (size + 1L):end_seen
    u <- runif(length(remaining))
    keep <- u < (size / idx_in_stream)
    if (any(keep)) {
      slots <- sample.int(size, sum(keep), replace = TRUE)
      res$buffer[slots] <- remaining[keep]
    }
  } else {
    idx_in_stream <- (start_seen + 1L):end_seen
    u <- runif(n)
    keep <- u < (size / idx_in_stream)
    if (any(keep)) {
      slots <- sample.int(size, sum(keep), replace = TRUE)
      res$buffer[slots] <- new_values[keep]
    }
  }
  res$seen <- end_seen
  res
}

reservoir_values <- function(res) {
  if (res$seen == 0L) return(numeric(0))
  res$buffer[seq_len(min(res$seen, res$size))]
}

# Complete-case mask across a set of covariates --------------------------
# Returns a logical vector: TRUE where ALL listed covariates are non-NA.
# Covariates absent from dt are treated as missing (returns all FALSE).
make_cc_mask <- function(dt, cov_vars) {
  present <- intersect(cov_vars, names(dt))
  if (!length(present)) return(rep(FALSE, nrow(dt)))
  Reduce(`&`, lapply(present, function(v) !is.na(dt[[v]])))
}

# Build per-group row predicates as data.table masks ---------------------
# Each cfg entry's predicate is a captured expression evaluated in the
# data.table's environment.
apply_row_predicates <- function(dt) {
  # Use with() instead of dt[, eval(...)] to skip [.data.table overhead.
  lapply(treatment_display_config, function(cfg) {
    tryCatch(eval(cfg$predicate, envir = dt, enclos = parent.frame()),
             error = function(e) rep(FALSE, nrow(dt)))
  })
}

# Accumulator initialisation ---------------------------------------------
init_accumulators <- function() {
  cat_counts <- list()
  for (cov in categorical_covariates) {
    cat_counts[[cov]] <- list()
    for (cfg in treatment_display_config) {
      cat_counts[[cov]][[cfg$key]] <- integer(0)
      names(cat_counts[[cov]][[cfg$key]]) <- character(0)
    }
  }
  cont_reservoirs <- list()
  for (cov in continuous_covariates) {
    cont_reservoirs[[cov]] <- list()
    for (cfg in treatment_display_config) {
      cont_reservoirs[[cov]][[cfg$key]] <- reservoir_new(RESERVOIR_SIZE)
    }
  }
  row_counts <- setNames(rep(0L, length(treatment_display_config)),
                         vapply(treatment_display_config, `[[`, "", "key"))
  list(cat = cat_counts, cont = cont_reservoirs, n = row_counts)
}

merge_cat_counts <- function(existing, new_values) {
  if (!length(new_values)) return(existing)
  x <- as.character(new_values)
  x[is.na(x) | x == ""] <- "(missing)"
  tab <- table(x)
  tab_names <- as.character(names(tab))
  tab_vals  <- as.integer(tab)

  if (!length(existing)) return(setNames(tab_vals, tab_names))

  # Extend `existing` with any new levels (zero-init), then add.
  new_names <- tab_names[!tab_names %in% names(existing)]
  if (length(new_names)) {
    existing <- c(existing,
                  setNames(integer(length(new_names)), new_names))
  }
  existing[tab_names] <- existing[tab_names] + tab_vals
  existing
}

# Phase B — stream parquet once, producing one accumulator per panel.
# panel_configs: all_panels list (both matched and unmatched entries).
# matched_uprns: named list (panel_key → named list (treat_key → char UPRNs))
#   for is_matched panels only; ignored for unmatched panels.
stream_parquet <- function(parquet_files, panel_configs, matched_uprns = list()) {
  accums <- setNames(
    replicate(length(panel_configs), init_accumulators(), simplify = FALSE),
    vapply(panel_configs, `[[`, "", "key")
  )

  ds_schema <- names(arrow::open_dataset(parquet_files[1], format = "parquet"))
  available_covariates <- intersect(all_covariates, ds_schema)
  missing_covariates   <- setdiff(all_covariates, available_covariates)
  if (length(missing_covariates)) {
    message("  NOTE: parquet is missing ", length(missing_covariates),
            " covariates: ", paste(missing_covariates, collapse = ", "))
  }
  available_ownership <- intersect(ownership_cols, ds_schema)

  # Include any extra spec-mask columns not already in all_covariates
  # (e.g. ppd_year_transfer used as an exact var in CT+PPD spec CC mask).
  all_mask_cols  <- unique(unlist(lapply(panel_configs, `[[`, "cov_vars")))
  extra_mask_cols <- intersect(setdiff(all_mask_cols, all_covariates), ds_schema)
  col_select <- unique(c("uprn", available_ownership, available_covariates,
                         extra_mask_cols))

  # Pre-build union pools for matched panels.
  union_pools <- list()
  for (p in panel_configs) {
    if (isTRUE(p$is_matched) && !is.null(matched_uprns[[p$key]])) {
      union_pools[[p$key]] <- unique(unlist(matched_uprns[[p$key]], use.names = FALSE))
    }
  }

  accumulate_for <- function(accum, mask, dt) {
    for (cfg_idx in seq_along(treatment_display_config)) {
      cfg <- treatment_display_config[[cfg_idx]]
      m <- mask[[cfg_idx]]
      if (!length(m) || !any(m)) next
      accum$n[[cfg$key]] <- accum$n[[cfg$key]] + sum(m)
      for (cov in intersect(categorical_covariates, available_covariates)) {
        vals <- dt[[cov]][m]
        accum$cat[[cov]][[cfg$key]] <- merge_cat_counts(
          accum$cat[[cov]][[cfg$key]], vals
        )
      }
      for (cov in intersect(continuous_covariates, available_covariates)) {
        vals <- suppressWarnings(as.numeric(dt[[cov]][m]))
        accum$cont[[cov]][[cfg$key]] <- reservoir_update(
          accum$cont[[cov]][[cfg$key]], vals
        )
      }
    }
    accum
  }

  total <- length(parquet_files)
  for (i in seq_along(parquet_files)) {
    f <- parquet_files[i]
    dt <- as.data.table(arrow::read_parquet(f, col_select = col_select))
    if (!nrow(dt)) { rm(dt); next }
    define_treatments(dt)

    if ("main_fuel" %in% names(dt))
      dt[, main_fuel := consolidate_main_fuel(main_fuel)]
    if ("construction_age_band" %in% names(dt))
      dt[, construction_age_band := consolidate_construction_age(construction_age_band)]
    if ("number_habitable_rooms" %in% names(dt))
      dt[, number_habitable_rooms := consolidate_habitable_rooms(number_habitable_rooms)]
    if ("built_form" %in% names(dt))
      dt[built_form %in% c("NO DATA!", "INVALID!"), built_form := "(missing/invalid)"]
    if ("current_energy_rating" %in% names(dt))
      dt[current_energy_rating %in% c("INVALID!", "NO DATA!"),
         current_energy_rating := "(missing/invalid)"]

    masks    <- apply_row_predicates(dt)
    uprn_vec <- dt$uprn

    for (p in panel_configs) {
      pkey <- p$key

      if (isTRUE(p$is_matched)) {
        # Matched panel: restrict to UPRN pool, then core filter + CC mask.
        union_pool <- union_pools[[pkey]]
        if (is.null(union_pool)) next
        in_pool <- uprn_vec %chin% union_pool
        if (!any(in_pool)) next

        core_ok  <- if (!is.null(p$core_filter))
          tryCatch(dt[, eval(p$core_filter)], error = function(e) rep(TRUE, nrow(dt)))
          else rep(TRUE, nrow(dt))
        cc_mask  <- if (length(p$cov_vars)) make_cc_mask(dt, p$cov_vars)
          else rep(TRUE, nrow(dt))
        eligible <- in_pool & core_ok & cc_mask
        if (!any(eligible)) next

        small_uprn  <- uprn_vec[eligible]
        treat_pools <- matched_uprns[[pkey]]

        panel_masks <- lapply(seq_along(treatment_display_config), function(j) {
          cfg      <- treatment_display_config[[j]]
          m        <- masks[[j]]
          pool_chr <- treat_pools[[cfg$key]]
          if (is.null(pool_chr)) return(logical(nrow(dt)))
          m_eligible <- m & eligible
          if (!any(m_eligible)) return(m_eligible)
          if (cfg$key == "control") return(m_eligible)
          out <- logical(length(uprn_vec))
          out[eligible] <- small_uprn %chin% pool_chr
          m & out
        })
        accums[[pkey]] <- accumulate_for(accums[[pkey]], panel_masks, dt)

      } else {
        # Unmatched panel: all parquet rows, filtered by core filter + CC mask.
        core_ok <- if (!is.null(p$core_filter))
          tryCatch(dt[, eval(p$core_filter)], error = function(e) rep(TRUE, nrow(dt)))
          else rep(TRUE, nrow(dt))
        cc_mask  <- if (length(p$cov_vars)) make_cc_mask(dt, p$cov_vars)
          else rep(TRUE, nrow(dt))

        panel_masks <- lapply(masks, function(m) m & core_ok & cc_mask)
        accums[[pkey]] <- accumulate_for(accums[[pkey]], panel_masks, dt)
      }
    }

    if (i %% 25L == 0L || i == total) {
      message(sprintf("  [%3d/%3d] processed %s (raw rows: %s)",
                      i, total, basename(f),
                      format(sum(accums[["raw"]]$n), big.mark = ",")))
    }
    rm(dt, masks); gc(verbose = FALSE)
  }

  for (pkey in names(accums)) {
    accums[[pkey]]$missing_covariates <- missing_covariates
  }
  accums
}

# Phase C — load matched UPRNs for a specific (core, spec) pair ----------
load_matched_uprns <- function(core, spec_name) {
  out <- list()
  for (key in names(treat_file_id)) {
    fid <- treat_file_id[[key]]
    fpath <- file.path(
      matched_data_dir,
      paste0("matched_pairs_", fid, "_matching_core_", core,
             "_", ccod_version, ".RData")
    )
    if (!file.exists(fpath)) {
      message("    Skipping ", key, ": no matched RData at ", basename(fpath))
      next
    }
    env <- new.env(parent = emptyenv())
    load(fpath, envir = env)
    spec_dt <- env$matched_results[[spec_name]]
    rm(env)
    if (is.null(spec_dt) || !nrow(spec_dt)) {
      message("    Skipping ", key, ": no rows for spec ", spec_name)
      next
    }
    out[[key]] <- unique(spec_dt$uprn[!is.na(spec_dt$uprn)])
    message(sprintf("    Loaded matched UPRNs: %s  (n=%s)", key,
                    format(length(out[[key]]), big.mark = ",")))
  }
  if (length(out)) {
    # Control pool: union of matched UPRN sets across treatments. The control
    # predicate (source=="Unknown" & private rental) naturally separates
    # controls from treated units during accumulation.
    out[["control"]] <- unique(unlist(out, use.names = FALSE))
  }
  out
}

# Plotting: categorical ---------------------------------------------------
render_categorical_long <- function(cat_counts_list, panel_label, n_row) {
  rows <- list()
  for (cfg in treatment_display_config) {
    counts <- cat_counts_list[[cfg$key]]
    if (!length(counts)) next
    total <- sum(counts)
    if (!total) next
    rows[[length(rows) + 1L]] <- data.table(
      row_key  = cfg$key,
      row_lab  = make_display_label(cfg),
      family   = cfg$family,
      depth    = cfg$depth,
      n_group  = as.integer(n_row[[cfg$key]]),
      level    = names(counts),
      count    = as.integer(counts),
      share    = as.integer(counts) / total,
      panel    = panel_label
    )
  }
  if (!length(rows)) return(NULL)
  rbindlist(rows)
}

plot_categorical <- function(cov, accums, out_file) {
  panel_labels <- vapply(all_panels, `[[`, "", "label")
  pieces <- list()
  for (p in all_panels) {
    acc <- accums[[p$key]]
    if (is.null(acc)) next
    pieces[[length(pieces) + 1L]] <- render_categorical_long(
      acc$cat[[cov]], p$label, acc$n
    )
  }
  dt <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(dt)) return(invisible(NULL))

  config_labels <- vapply(treatment_display_config, make_display_label, "")
  dt[, row_lab := factor(row_lab, levels = config_labels)]
  dt[, panel := factor(panel, levels = panel_labels)]
  ordered_levels <- order_levels(cov, dt$level)
  dt[, level := factor(level, levels = ordered_levels)]
  level_palette <- build_level_palette(cov, ordered_levels)

  n_ann <- unique(dt[, list(row_lab, panel, n_group)])
  n_ann[, label := sprintf("n=%s", format(n_group, big.mark = ","))]

  n_panels <- length(unique(dt$panel))
  legend_rows <- max(1L, ceiling(length(ordered_levels) / 6L))
  p <- ggplot(dt, aes(y = row_lab, x = share, fill = level)) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.85) +
    geom_text(data = n_ann, inherit.aes = FALSE,
              aes(y = row_lab, x = 1.02, label = label),
              hjust = 0, size = 3, colour = "grey30") +
    facet_grid(. ~ panel) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1.40),
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = level_palette, drop = FALSE,
                      breaks = ordered_levels) +
    guides(fill = guide_legend(nrow = legend_rows, byrow = TRUE,
                               title.position = "top")) +
    coord_cartesian(clip = "off") +
    labs(title = paste0("Distribution of '", cov, "' by ownership group"),
         subtitle = paste0("Raw vs OLS/PSM panels (", PLOT_GEO, ")"),
         x = "Share", y = NULL, fill = cov) +
    theme_bw(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          legend.position    = "bottom",
          legend.key.height  = unit(8, "pt"),
          legend.key.width   = unit(14, "pt"),
          strip.background   = element_rect(fill = "grey92", colour = NA),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"),
          plot.margin        = margin(5.5, 20, 5.5, 5.5))
  ggsave(out_file, p, width = 6 + 4.2 * n_panels, height = 9, dpi = 300)
  message("  Saved: ", basename(out_file))
}

# Plotting: continuous ----------------------------------------------------
render_continuous_long <- function(reservoir_list, panel_label, n_row) {
  rows <- list()
  for (cfg in treatment_display_config) {
    vals <- reservoir_values(reservoir_list[[cfg$key]])
    if (!length(vals)) next
    rows[[length(rows) + 1L]] <- data.table(
      row_key = cfg$key,
      row_lab = make_display_label(cfg),
      family  = cfg$family,
      depth   = cfg$depth,
      n_group = as.integer(n_row[[cfg$key]]),
      value   = vals,
      panel   = panel_label
    )
  }
  if (!length(rows)) return(NULL)
  rbindlist(rows)
}

plot_continuous <- function(cov, accums, out_file) {
  panel_labels <- vapply(all_panels, `[[`, "", "label")
  pieces <- list()
  for (p in all_panels) {
    acc <- accums[[p$key]]
    if (is.null(acc)) next
    pieces[[length(pieces) + 1L]] <- render_continuous_long(
      acc$cont[[cov]], p$label, acc$n
    )
  }
  dt <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
  if (!nrow(dt)) return(invisible(NULL))

  # 1st/99th-pct cap for heavy-tailed covariates (floor area, price).
  q99 <- quantile(dt$value, 0.99, na.rm = TRUE)
  q01 <- quantile(dt$value, 0.01, na.rm = TRUE)
  dt <- dt[value >= q01 & value <= q99]

  config_labels <- vapply(treatment_display_config, make_display_label, "")
  dt[, row_lab := factor(row_lab, levels = config_labels)]
  dt[, panel := factor(panel, levels = panel_labels)]
  dt[, family := factor(family, levels = names(family_colors))]

  n_ann <- unique(dt[, list(row_lab, panel, n_group)])
  n_ann[, label := sprintf("n=%s", format(n_group, big.mark = ","))]

  x_range <- range(dt$value, na.rm = TRUE)
  x_pad   <- diff(x_range) * 0.32
  x_ann   <- x_range[2] + diff(x_range) * 0.02

  n_panels <- length(unique(dt$panel))
  base <- ggplot(dt, aes(y = row_lab, x = value, fill = family)) +
    facet_grid(. ~ panel) +
    scale_fill_manual(values = family_colors, drop = FALSE) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(limits = c(x_range[1], x_range[2] + x_pad)) +
    coord_cartesian(clip = "off") +
    geom_text(data = n_ann, inherit.aes = FALSE,
              aes(y = row_lab, x = x_ann, label = label),
              hjust = 0, size = 3, colour = "grey30") +
    labs(title = paste0("Distribution of '", cov, "' by ownership group"),
         subtitle = paste0("Raw vs OLS/PSM panels (", PLOT_GEO, ")"),
         x = cov, y = NULL, fill = "Family") +
    theme_bw(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          legend.position    = "bottom",
          strip.background   = element_rect(fill = "grey92", colour = NA),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"),
          plot.margin        = margin(5.5, 20, 5.5, 5.5))

  p <- if (HAS_GGRIDGES) {
    base + ggridges::geom_density_ridges(alpha = 0.75, scale = 0.9,
                                         rel_min_height = 0.01)
  } else {
    base + geom_violin(alpha = 0.75, scale = "width")
  }
  ggsave(out_file, p, width = 6 + 4.2 * n_panels, height = 9, dpi = 300)
  message("  Saved: ", basename(out_file))
}

# Main driver -------------------------------------------------------------
main <- function() {
  parquet_files <- list.files(la_refined_dir, pattern = "\\.parquet$",
                              full.names = TRUE)
  parquet_files <- parquet_files[
    !grepl("unknown", basename(parquet_files), ignore.case = TRUE)
  ]
  if (!length(parquet_files)) stop("No parquet files in ", la_refined_dir)
  if (!is.na(MAX_FILES)) {
    parquet_files <- head(parquet_files, MAX_FILES)
    message("PLOT_MAX_FILES=", MAX_FILES, " — truncating to ",
            length(parquet_files), " files for smoke test.")
  }
  message("Found ", length(parquet_files), " LA parquet files.")

  # One cache file per panel.
  panel_caches <- setNames(
    vapply(all_panels, function(p)
      file.path(cache_dir, paste0("accum_", PLOT_GEO, "_", p$key, ".rds")),
      ""),
    vapply(all_panels, `[[`, "", "key")
  )

  accums <- setNames(vector("list", length(all_panels)),
                     vapply(all_panels, `[[`, "", "key"))

  all_caches_exist <- all(vapply(panel_caches, file.exists, logical(1)))

  if (!FORCE_RECOMPUTE && all_caches_exist) {
    message("Loading cached accumulators...")
    for (pkey in names(panel_caches)) {
      accums[[pkey]] <- readRDS(panel_caches[[pkey]])
    }
  } else {
    message("\n--- Phase C: loading matched UPRNs for PSM panels ---")
    matched_uprns <- list()
    for (p in all_panels) {
      if (!isTRUE(p$is_matched)) next
      message(sprintf("  Panel '%s' (core=%s, spec=%s):",
                      p$key, p$core, p$spec_name))
      up <- load_matched_uprns(core = p$core, spec_name = p$spec_name)
      if (length(up)) matched_uprns[[p$key]] <- up
    }
    n_matched <- sum(vapply(all_panels, `[[`, FALSE, "is_matched"))
    n_loaded  <- length(matched_uprns)
    message(sprintf("Matched UPRN pools loaded for %d / %d PSM panels.", n_loaded, n_matched))

    message("\n--- Phase B: streaming parquet (", length(all_panels), " panels) ---")
    accums <- stream_parquet(parquet_files,
                             panel_configs  = all_panels,
                             matched_uprns  = matched_uprns)

    for (pkey in names(panel_caches)) {
      saveRDS(accums[[pkey]], panel_caches[[pkey]])
      message("Saved accumulator (", pkey, ") to ", basename(panel_caches[[pkey]]))
    }
  }

  # Phase D — render plots
  message("\n--- Phase D: rendering plots ---")
  available_covariates <- setdiff(all_covariates,
                                  accums[["raw"]]$missing_covariates %||% character())

  for (cov in intersect(categorical_covariates, available_covariates)) {
    out_file <- file.path(output_dir, paste0("cat_", cov, ".png"))
    plot_categorical(cov, accums, out_file)
  }
  for (cov in intersect(continuous_covariates, available_covariates)) {
    out_file <- file.path(output_dir, paste0("cont_", cov, ".png"))
    plot_continuous(cov, accums, out_file)
  }

  elapsed <- difftime(Sys.time(), start.time, units = "mins")
  message(sprintf("\nDone. Total runtime: %.1f min.", as.numeric(elapsed)))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

main()
