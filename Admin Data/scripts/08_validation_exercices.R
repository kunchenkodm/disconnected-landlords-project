# Script: 08_validation_exercices.R
# Purpose: Run LSOA level property share validations.
#          Streams per-LA Parquet files to accumulate LSOA-level aggregates
#          without loading the full dataset into memory.
# Author(s): Dmytro Kunchenko
# Date: December 13, 2025. Last Updated: Febuary 20, 2026.
rm(list=setdiff(ls(), c("script", "pipeline.start.time")))
gc()


# Setup & Dependencies ----------------------------------------------------
start.time <- Sys.time()
source(here::here("scripts", "00_setup.R"))

library(data.table)
library(ggplot2)
library(xtable)
library(arrow)

ccod_version <- CCOD_VERSION
la_refined_dir <- EPC_LA_REFINED_DIR
raw_dir <- RAW_DATA_DIR

census_tenure_file <- file.path(raw_dir, "/census/TS054-2021-1-filtered-2025-12-18T13_41_32Z.csv")
lookup_file <- file.path(RAW_LOOKUPS_DIR, "PCD_OA21_LSOA21_MSOA21_LAD_AUG24_UK_LU.csv")

tables_dir <- file.path(OUTPUT_DIR, "tables")
if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
  message("Created tables directory: ", tables_dir)
}

# Load Lookup and Census Datasets -----------------------------------------
if (!file.exists(census_tenure_file)) stop("Census file does not exist: ", census_tenure_file)
message("Loading Census data...")
census_tenure <- fread(census_tenure_file)

if (!file.exists(lookup_file)) stop("Lookup file does not exist: ", lookup_file)
message("Loading Lookup data...")
postcode_lookup <- fread(lookup_file)

# Treatment definitions (needed per-LA)
source(here::here("scripts", "treatment_definitions.R"))


# Stream per-LA Parquets and accumulate LSOA aggregates --------------------
message("Streaming per-LA Parquet files for LSOA aggregation...")

la_files <- list.files(la_refined_dir, pattern = "\\.parquet$", full.names = TRUE)
if (length(la_files) == 0L) stop("No per-LA Parquet files found in: ", la_refined_dir)

epc_agg_accum <- data.table()

for (i in seq_along(la_files)) {
  if (i == 1L || i %% 50L == 0L) {
    elapsed <- as.numeric(difftime(Sys.time(), start.time, units = "secs"))
    message(sprintf("[%d/%d] Processing validation (%.0f s)", i, length(la_files), elapsed))
  }

  dt <- as.data.table(arrow::read_parquet(la_files[i]))
  dt <- define_treatments(dt)

  # Select needed columns
  la_val <- dt[, .(epc_postcode, tenure_2, treat_for_profit, treat_non_profit, treat_public_sector)]

  # Join with geography
  la_val <- merge(
    la_val,
    postcode_lookup[, .(pcds, lsoa21cd)],
    by.x = "epc_postcode",
    by.y = "pcds",
    all.x = TRUE
  )

  # Filter for valid LSOAs
  la_val <- la_val[!is.na(lsoa21cd)]

  if (nrow(la_val) == 0) {
    rm(dt, la_val)
    gc()
    next
  }

  # Aggregate per LSOA
  la_agg <- la_val[, .(
    total_epc = .N,
    count_treat_fp   = sum(treat_for_profit == 1, na.rm = TRUE),
    count_control    = sum(treat_for_profit == 0, na.rm = TRUE),
    count_treat_np   = sum(treat_non_profit == 1, na.rm = TRUE),
    count_treat_pub  = sum(treat_public_sector == 1, na.rm = TRUE),
    count_combined_private = sum(!is.na(treat_for_profit), na.rm = TRUE)
  ), by = lsoa21cd]

  epc_agg_accum <- rbindlist(list(epc_agg_accum, la_agg), use.names = TRUE)

  rm(dt, la_val, la_agg)
  gc()
}

# Final aggregation: sum across LAs for the same LSOA
# (an LSOA may have properties from different EPC LA folders)
message("Final LSOA aggregation...")
epc_agg <- epc_agg_accum[, .(
  total_epc = sum(total_epc),
  count_treat_fp = sum(count_treat_fp),
  count_control = sum(count_control),
  count_treat_np = sum(count_treat_np),
  count_treat_pub = sum(count_treat_pub),
  count_combined_private = sum(count_combined_private)
), by = lsoa21cd]

rm(epc_agg_accum)

# Calculate Proportions
epc_agg[, `:=`(
  prop_treat_fp        = count_treat_fp / total_epc,
  prop_control         = count_control / total_epc,
  prop_combined_priv   = count_combined_private / total_epc,
  prop_treat_np        = count_treat_np / total_epc,
  prop_treat_pub       = count_treat_pub / total_epc
)]

# Aggregate Census data ---------------------------------------------------
message("Aggregating Census Data...")

# Create flags
census_tenure[, `:=`(
  is_cat0 = `Tenure of household (9 categories) Code` == 0, # Owned Outright
  is_cat3 = `Tenure of household (9 categories) Code` == 3, # Social Council
  is_cat4 = `Tenure of household (9 categories) Code` == 4, # Social Other
  is_cat5 = `Tenure of household (9 categories) Code` == 5, # Private Landlord
  is_cat6 = `Tenure of household (9 categories) Code` == 6  # Other Private
)]

# Dynamically find the correct LSOA Code column
lsoa_col_name <- grep("Lower layer.*Code", names(census_tenure), value = TRUE, ignore.case = TRUE)
if (length(lsoa_col_name) == 0) lsoa_col_name <- grep("geography code", names(census_tenure), value = TRUE, ignore.case = TRUE)
if (length(lsoa_col_name) == 0) stop("Could not automatically find the LSOA Code column in Census data.")

message("Using Census Geography Column: ", lsoa_col_name)

# Sum up observations per LSOA
census_agg <- census_tenure[, .(
  total_census    = sum(Observation),
  count_census_0  = sum(Observation[is_cat0]),
  count_census_3  = sum(Observation[is_cat3]),
  count_census_4  = sum(Observation[is_cat4]),
  count_census_5  = sum(Observation[is_cat5]),
  count_census_6  = sum(Observation[is_cat6]),

  # Combined Census Private (5 + 6)
  count_census_56 = sum(Observation[is_cat5 | is_cat6])
), by = c(lsoa_col_name)]

setnames(census_agg, lsoa_col_name, "lsoa21cd")

# Calculate Proportions
census_agg[, `:=`(
  prop_census_0  = count_census_0 / total_census,
  prop_census_3  = count_census_3 / total_census,
  prop_census_4  = count_census_4 / total_census,
  prop_census_5  = count_census_5 / total_census,
  prop_census_6  = count_census_6 / total_census,
  prop_census_56 = count_census_56 / total_census
)]

# Merge and Calculate Measures of Mismatch --------------------------------
message("Merging Datasets and Calculating Mismatch Variables...")

# Check common IDs
common_ids <- intersect(epc_agg$lsoa21cd, census_agg$lsoa21cd)
if(length(common_ids) == 0) stop("Merge failed: No common LSOA codes found.")

validation_set <- merge(epc_agg, census_agg, by = "lsoa21cd")

# CALCULATE VALIDATION VARIABLE Y^(9) ---
# Y = R_data - R_census

# 1. Total Private
validation_set[, diff_priv_total := prop_combined_priv - prop_census_56]

# 2. For Profit
validation_set[, diff_fp_vs_5 := prop_treat_fp - prop_census_5]
validation_set[, diff_fp_vs_6 := prop_treat_fp - prop_census_6]
validation_set[, diff_fp_vs_56 := prop_treat_fp - prop_census_56]

# 3. Control
validation_set[, diff_ctrl_vs_5 := prop_control - prop_census_5]
validation_set[, diff_ctrl_vs_6 := prop_control - prop_census_6]
validation_set[, diff_ctrl_vs_56 := prop_control - prop_census_56]

# 4. Non-Profit
validation_set[, diff_np_vs_4 := prop_treat_np - prop_census_4]

# 5. Public Sector
validation_set[, diff_pub_vs_3 := prop_treat_pub - prop_census_3]

message("Merge successful. Rows: ", nrow(validation_set))

# Visualisation -----------------------------------------------------------
# Correlation Table -------------------------------------------------------
message("Calculating Correlations...")

tests <- list(
  list(grp="Total Private (Treat+Ctrl)", cens="Combined Priv (5+6)", epc_col="prop_combined_priv", cen_col="prop_census_56"),
  list(grp="For-Profit Only",            cens="Priv Landlord (5)",   epc_col="prop_treat_fp",      cen_col="prop_census_5"),
  list(grp="For-Profit Only",            cens="Other Private (6)",   epc_col="prop_treat_fp",      cen_col="prop_census_6"),
  list(grp="Control (Ind) Only",         cens="Priv Landlord (5)",   epc_col="prop_control",       cen_col="prop_census_5"),
  list(grp="Control (Ind) Only",         cens="Other Private (6)",   epc_col="prop_control",       cen_col="prop_census_6"),
  list(grp="Non-Profit",                 cens="Social Other (4)",    epc_col="prop_treat_np",      cen_col="prop_census_4"),
  list(grp="Public Sector",              cens="Social Council (3)",  epc_col="prop_treat_pub",     cen_col="prop_census_3")
)

cor_results <- rbindlist(lapply(tests, function(x) {
  if (nrow(validation_set) > 0) {
    cor_val <- cor(validation_set[[x$epc_col]], validation_set[[x$cen_col]], use = "complete.obs")
  } else { cor_val <- NA }
  data.table(`EPC Group` = x$grp, `Census Category` = x$cens, `Correlation` = round(cor_val, 4))
}))

message("\n=======================================================")
message("             VALIDATION CORRELATION MATRIX             ")
message("=======================================================")
print(cor_results)
message("=======================================================\n")

# Mismatch Statistics -----------------------------------------------------
message("\n=======================================================")
message("       MISMATCH STATISTICS (EPC Prop - Census Prop)    ")
message("=======================================================")

# Helper function
get_stats_row <- function(desc, col_name) {
  vals <- validation_set[[col_name]]
  data.table(
    Comparison = desc,
    Mean   = round(mean(vals, na.rm=T), 4),
    SD     = round(sd(vals, na.rm=T), 4),
    Q1     = round(quantile(vals, 0.25, na.rm=T), 4),
    Median = round(median(vals, na.rm=T), 4),
    Q3     = round(quantile(vals, 0.75, na.rm=T), 4)
  )
}

# Bind rows with descriptive labels
mismatch_table <- rbind(
  # Total
  get_stats_row("Total Private vs Combined Private (5+6)", "diff_priv_total"),

  # For Profit
  get_stats_row("For-Profit vs Census 5 (Priv Landlord)",  "diff_fp_vs_5"),
  get_stats_row("For-Profit vs Census 6 (Other Private)",  "diff_fp_vs_6"),
  get_stats_row("For-Profit vs Combined Private (5+6)",    "diff_fp_vs_56"),

  # Control
  get_stats_row("Control vs Census 5 (Priv Landlord)",     "diff_ctrl_vs_5"),
  get_stats_row("Control vs Census 6 (Other Private)",     "diff_ctrl_vs_6"),
  get_stats_row("Control vs Combined Private (5+6)",       "diff_ctrl_vs_56"),

  # Non-Profit & Public
  get_stats_row("Non-Profit vs Census 4 (Social Other)",   "diff_np_vs_4"),
  get_stats_row("Public Sector vs Census 3 (Social Council)", "diff_pub_vs_3")
)

print(mismatch_table)
message("=======================================================\n")

# Validation Plots --------------------------------------------------------
message("Generating Validation Plots...")

# Define and create the validation figures directory
validation_fig_dir <- file.path(FIGURES_DIR, "validation")
if (!dir.exists(validation_fig_dir)) {
  dir.create(validation_fig_dir, recursive = TRUE)
  message("Created validation figures directory: ", validation_fig_dir)
}

plot_validation <- function(data, x_col, y_col, title, color, filename) {
  c_val <- cor(data[[x_col]], data[[y_col]], use="complete.obs")
  p <- ggplot(data, aes_string(x = x_col, y = y_col)) +
    geom_point(alpha = 0.1, size = 0.5, color = color) +
    geom_smooth(method = "lm", color = "red", se = FALSE, linewidth = 0.5) +
    theme_bw() +
    labs(
      title = paste0(title, "\nR = ", round(c_val, 3)),
      x = paste0("Census: ", x_col),
      y = paste0("EPC: ", y_col)
    )

  # Output file path
  out_path <- file.path(validation_fig_dir, filename)

  # Save the plot
  ggsave(out_path, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
  message("Saved figure: ", filename)
}

# 1. TOTAL PRIVATE
plot_validation(validation_set, "prop_census_56", "prop_combined_priv",
                "Total Private vs Census 5+6", "black",
                "01_total_private_vs_census_56.png")

# 2. INDIVIDUAL COMPONENTS
plot_validation(validation_set, "prop_census_5", "prop_treat_fp",
                "For-Profit vs Census 5 (Priv Landlord)", "blue",
                "02_for_profit_vs_census_5.png")

plot_validation(validation_set, "prop_census_5", "prop_control",
                "Control vs Census 5 (Priv Landlord)", "darkgreen",
                "03_control_vs_census_5.png")

# 3. NON-PRIVATE
plot_validation(validation_set, "prop_census_4", "prop_treat_np",
                "Non-Profit vs Census 4 (Social Other)", "orange",
                "04_non_profit_vs_census_4.png")

plot_validation(validation_set, "prop_census_3", "prop_treat_pub",
                "Public Sector vs Census 3 (Social Council)", "brown",
                "05_public_sector_vs_census_3.png")


# Export to LaTeX ---------------------------------------------------------

## Mismatch table

# Clean names for LaTeX
mismatch_export <- copy(mismatch_table)
names(mismatch_export) <- c("Comparison Group", "Mean", "SD", "Q1", "Median", "Q3")

# Create xtable object (No caption/label needed here anymore)
latex_mismatch <- xtable(
  mismatch_export,
  digits = c(0, 0, 4, 4, 4, 4, 4)
)

# Save to file (floating = FALSE removes the \begin{table} wrapper)
print(
  latex_mismatch,
  file = file.path(tables_dir, "validation_mismatch.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  sanitize.text.function = function(x) x
)
message("Saved Mismatch Table to: ", file.path(tables_dir, "validation_mismatch.tex"))


## Correlation table

# Clean names for LaTeX
cor_export <- copy(cor_results)
names(cor_export) <- c("EPC Treatment Group", "Census Benchmark", "Correlation (R)")

# Create xtable object
latex_cor <- xtable(
  cor_export,
  digits = c(0, 0, 0, 4)
)

# Save to file
print(
  latex_cor,
  file = file.path(tables_dir, "validation_correlations.tex"),
  include.rownames = FALSE,
  floating = FALSE,
  booktabs = TRUE
)
message("Saved Correlation Table to: ", file.path(tables_dir, "validation_correlations.tex"))

message("Validation complete. Script finished in: ",
        round(difftime(Sys.time(), start.time, units="mins"), 2), " mins.")
