# Script: 06_run_regressions.R
# Purpose: Generate CSV summary from regression results.
# Authors: Dmytro Kunchenko, Thiemo Fetzer
# Date: July 15, 2025. Last Updated: October 8, 2025.

rm(list = ls())
gc()
start.time <- Sys.time()
source(here::here("scripts", "00_setup.R"))
library(data.table)
library(fixest)

ccod_version <- CCOD_VERSION
processed_data_dir <- PROCESSED_DATA_DIR
matched_data_dir <- MATCHED_DATA_DIR
summary_dir <- file.path(getwd(), "output", "summary_tables")
if (!dir.exists(summary_dir)) dir.create(summary_dir, recursive = TRUE)

expand_matched_data <- function(matched_list, full_data) {
  lapply(matched_list, function(matched_dt) {
    if (is.null(matched_dt) || nrow(matched_dt) == 0) return(NULL)
    full_data[matched_dt[!is.na(uprn)], on = "uprn", nomatch = 0]
  })
}

build_formula <- function(outcome, treatment_var, continuous_vars, exact_vars, model_type) {
  continuous_vars <- continuous_vars[continuous_vars != ""]
  exact_vars <- exact_vars[exact_vars != ""]
  rhs <- paste(c(treatment_var, continuous_vars), collapse = " + ")
  if (model_type == "OLS Additive FE") {
    fe <- paste(exact_vars, collapse = " + ")
    if (fe != "")
      return(as.formula(paste(outcome, "~", rhs, "|", fe)))
    else
      return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type == "OLS Interactive FE") {
    fe_interact <- paste(exact_vars, collapse = "^")
    if (fe_interact != "")
      return(as.formula(paste(outcome, "~", rhs, "|", fe_interact)))
    else
      return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type == "PSM (Matched)") {
    return(as.formula(paste(outcome, "~", rhs)))
  } else if (model_type == "PSM (Matched) + Subclass FE") {
    return(as.formula(paste(outcome, "~", rhs, "| subclass")))
  }
  stop("Unknown model type: ", model_type)
}

get_core_filter <- function(core_name) {
  switch(core_name,
         base = quote(rep(TRUE, .N)),
         council_tax = quote(!is.na(tax_band)),
         ppd = quote(!is.na(ppd_price_sqm)),
         ppd_counciltax = quote(!is.na(tax_band) & !is.na(ppd_price_sqm)),
         stop("Unknown core: ", core_name)
  )
}

message("Loading data...")
input_file <- file.path(processed_data_dir, paste0("epc_matched_refined_", CCOD_VERSION, ".RData"))
load(input_file)
source(here::here("scripts", "treatment_definitions.R"))
EPC_matched_combined <- define_treatments(EPC_matched_combined)
EPC_matched_combined[, energy_cons_curr_per_floor_area :=
                       fifelse(total_floor_area == 0, NA_real_, energy_consumption_current / total_floor_area)]
setkey(EPC_matched_combined, uprn)

outcome_variables <- c(
  "bad_epc", "energy_consumption_current", "energy_consumption_current_property",
  "el_mean_consumption_k_wh", "gas_mean_consumption_k_wh", "energy_consumption_gap",
  "energy_consumption_gap_property", "energy_efficiency_potential_gap",
  "energy_efficiency_bad_epc_gap", "energy_efficiency_worse_epc_gap",
  "borderline_good_epc", "borderline_better_epc"
)
continuous_controls <- list(
  c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
  c("number_habitable_rooms", "total_floor_area", "lodgement_year"),
  c("number_habitable_rooms", "total_floor_area", "lodgement_year", "ppd_price_sqm")
)
exact_controls <- list(
  c("property_type", "main_fuel", "construction_age_band", "built_form", "local_authority"),
  c("property_type", "main_fuel", "tax_band", "construction_age_band", "built_form", "local_authority"),
  c("property_type", "main_fuel", "tax_band", "ppd_year_transfer", "construction_age_band", "built_form", "local_authority")
)
analysis_configs <- list(
  list(var = "treat_for_profit", file_id = "for_profit_vs_private_rental", title = "Effect of For-Profit Ownership"),
  list(var = "treat_uk_for_profit", file_id = "uk_for_profit_vs_private_rental", title = "Effect of UK For-Profit Ownership"),
  list(var = "treat_foreign_for_profit", file_id = "foreign_for_profit_vs_private_rental", title = "Effect of Foreign For-Profit Ownership"),
  list(var = "treat_tax_haven_for_profit", file_id = "tax_haven_for_profit_vs_private_rental", title = "Effect of Tax Haven For-Profit Ownership"),
  list(var = "treat_non_profit", file_id = "non_profit_vs_private_rental", title = "Effect of Non-Profit Ownership"),
  list(var = "treat_uk_non_profit", file_id = "uk_non_profit_vs_private_rental", title = "Effect of UK Non-Profit Ownership"),
  list(var = "treat_foreign_non_profit", file_id = "foreign_non_profit_vs_private_rental", title = "Effect of Foreign Non-Profit Ownership"),
  list(var = "treat_tax_haven_non_profit", file_id = "tax_haven_non_profit_vs_private_rental", title = "Effect of Tax Haven Non-Profit Ownership"),
  list(var = "treat_public_sector", file_id = "public_sector_vs_private_rental", title = "Effect of Public Sector Ownership"),
  list(var = "treat_tax_haven", file_id = "tax_haven_vs_private_rental", title = "Effect of Tax Haven Ownership"),
  list(var = "treat_british_haven", file_id = "british_haven_vs_private_rental", title = "Effect of British Haven Ownership"),
  list(var = "treat_european_haven", file_id = "european_haven_vs_private_rental", title = "Effect of European Haven Ownership"),
  list(var = "treat_caribbean_haven", file_id = "caribbean_haven_vs_private_rental", title = "Effect of Caribbean Haven Ownership"),
  list(var = "treat_other_haven", file_id = "other_haven_vs_private_rental", title = "Effect of Other Haven Ownership")
)
model_types <- c("OLS Additive FE", "OLS Interactive FE", "PSM (Matched)", "PSM (Matched) + Subclass FE")
spec_names <- c("Baseline", "Council Tax", "Council Tax + Price Paid")
core_names <- c("base", "council_tax", "ppd", "ppd_counciltax")

message("Calculating total number of models...")
n_ols_models <- length(analysis_configs) * length(outcome_variables) * 2 * 3 * length(core_names)
n_psm_models <- length(analysis_configs) * length(outcome_variables) * 2 * 3 * length(core_names)
total_models <- n_ols_models + n_psm_models
message(sprintf(" OLS models to estimate: %d", n_ols_models))
message(sprintf(" PSM models to estimate: %d", n_psm_models))
message(sprintf(" Total models to estimate: %d", total_models))
message(sprintf(" Estimated time: %.1f-%.1f minutes (at 50-100 models/sec)\n",
                total_models / 100 / 60, total_models / 50 / 60))

master_results_list <- vector("list", total_models)
result_counter <- 0
models_counter <- 0
last_update_time <- Sys.time()
update_interval <- 5

for (current_model_name in model_types) {
  is_psm_model <- grepl("PSM", current_model_name)
  message(sprintf("\n--- Processing %s models ---", current_model_name))
  for (i in 1:3) {
    spec_name <- spec_names[i]
    message(sprintf(" Specification: %s", spec_name))
    for (core_name in core_names) {
      ols_data_filtered <- NULL
      if (!is_psm_model) {
        core_filter <- get_core_filter(core_name)
        ols_data_filtered <- EPC_matched_combined[eval(core_filter)]
        if (nrow(ols_data_filtered) == 0) {
          message(sprintf(" Skipping core '%s' - no data after filtering", core_name))
          next
        }
      }
      for (config in analysis_configs) {
        matched_data_for_spec <- NULL
        if (is_psm_model) {
          matched_file_path <- file.path(
            matched_data_dir,
            paste0("matched_pairs_", config$file_id, "_", core_name, "_", ccod_version, ".RData")
          )
          if (!file.exists(matched_file_path)) {
            message(sprintf(" Skipping: no matched pairs file for '%s', core '%s'", config$file_id, core_name))
            next
          }
          load(matched_file_path)
          matched_expanded <- expand_matched_data(matched_results, EPC_matched_combined)
          rm(matched_results)
          matched_data_for_spec <- matched_expanded[[i]]
          rm(matched_expanded)
          if (is.null(matched_data_for_spec) || nrow(matched_data_for_spec) == 0) {
            message(sprintf(" Skipping: no data for '%s', core '%s', spec %d", config$file_id, core_name, i))
            next
          }
        }
        data_to_use <- if (is_psm_model) matched_data_for_spec else ols_data_filtered
        for (current_outcome in outcome_variables) {
          models_counter <- models_counter + 1
          current_time <- Sys.time()
          if (as.numeric(difftime(current_time, last_update_time, units = "secs")) >= update_interval) {
            pct <- round(100 * models_counter / total_models, 1)
            elapsed <- as.numeric(difftime(current_time, start.time, units = "secs"))
            rate <- models_counter / elapsed
            eta_sec <- (total_models - models_counter) / rate
            eta_min <- eta_sec / 60
            message(sprintf(" [%d/%d] %.1f%% | %.1f models/sec | ETA: %.1f min | %s",
                            models_counter, total_models, pct, rate, eta_min, current_outcome))
            last_update_time <- current_time
          }
          fml <- build_formula(
            outcome = current_outcome,
            treatment_var = config$var,
            continuous_vars = continuous_controls[[i]],
            exact_vars = exact_controls[[i]],
            model_type = current_model_name
          )
          result <- tryCatch({
            model <- feols(fml, data = data_to_use, cluster = ~local_authority)
            ct <- coeftable(model)
            if (!(config$var %in% rownames(ct))) {
              NULL
            } else {
              row <- ct[config$var, ]
              data.table(
                coef = row["Estimate"],
                se = row["Std. Error"],
                nobs = nobs(model),
                r2 = r2(model, "r2"),
                outcome = current_outcome,
                treatment = config$title,
                model = current_model_name,
                spec = spec_name,
                core = core_name
              )
            }
          }, error = function(e) {
            warning(sprintf("Error in %s | %s | %s | %s | %s: %s",
                            current_model_name, spec_name, core_name,
                            config$title, current_outcome, e$message),
                    call. = FALSE)
            NULL
          })
          if (!is.null(result)) {
            result_counter <- result_counter + 1
            master_results_list[[result_counter]] <- result
          }
        }
        if (is_psm_model) rm(matched_data_for_spec); gc(verbose = FALSE)
      }
      if (!is_psm_model) rm(ols_data_filtered); gc(verbose = FALSE)
    }
  }
}

message("\nCombining results...")
master_results <- rbindlist(master_results_list[1:result_counter])
output_csv_path_master <- file.path(summary_dir, "master_results.csv")
fwrite(master_results, output_csv_path_master)
message(paste(" Saved all results to:", output_csv_path_master))

end.time <- Sys.time()
time.taken <- end.time - start.time
message(sprintf("\n=== Script Complete ==="))
message(sprintf("Models attempted: %d/%d", models_counter, total_models))
message(sprintf("Successful regressions: %d (%.1f%%)",
                result_counter, 100 * result_counter / models_counter))
message(sprintf("Runtime: %.2f %s (%.1f models/sec)",
                as.numeric(time.taken), units(time.taken),
                models_counter / as.numeric(time.taken, units = "secs")))
