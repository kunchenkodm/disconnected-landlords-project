# Script: diagnostic_cluster_counts.R
# Run this in the same environment where EPC_matched_combined is loaded

library(data.table)

# 1. Prepare a fast lookup table
treatment_vars <- sapply(treatment_metadata, function(x) x$var)
la_lookup <- EPC_matched_combined[, c("uprn", "local_authority", treatment_vars), with = FALSE]
setkey(la_lookup, uprn)

diagnostic_results <- list()
idx <- 1L

message("Scanning matched pair files for cluster counts across all calipers...")

for (config in treatment_metadata) {
  treat_var <- config$var
  file_id <- config$file_id
  
  for (matching_core in names(matching_core_filters)) {
    # Construct the file path exactly as your regression script does
    matched_file_path <- file.path(
      MATCHED_DATA_DIR,
      paste0("matched_pairs_", file_id, "_matching_core_", matching_core, "_", CCOD_VERSION, ".RData")
    )
    
    if (!file.exists(matched_file_path)) next
    
    # Load into an isolated environment
    local_env <- new.env()
    load(matched_file_path, envir = local_env)
    
    for (spec_config in spec_configs) {
      spec_name <- spec_config$name
      matched_dt <- local_env$matched_results[[spec_name]]
      
      if (is.null(matched_dt) || nrow(matched_dt) == 0) next
      
      # Join to get the local_authority and treatment status
      joined_data <- la_lookup[matched_dt[!is.na(uprn)], on = "uprn", nomatch = 0]
      if (nrow(joined_data) == 0) next
      
      # Define the 3 calipers evaluated in the pipeline
      thresholds <- list(
        "None" = Inf,
        "PS_0.2" = 0.2,
        "PS_0.1" = 0.1
      )
      
      for (thresh_name in names(thresholds)) {
        thresh_val <- thresholds[[thresh_name]]
        
        # Apply the distance filter if a caliper is specified
        if (thresh_val < Inf) {
          if ("distance" %in% names(joined_data)) {
            subset_data <- joined_data[distance <= thresh_val]
          } else {
            subset_data <- data.table() # Skip if no distance metric exists
          }
        } else {
          subset_data <- joined_data
        }
        
        # Calculate cluster counts and approximate pairs
        if (nrow(subset_data) == 0) {
          n_treated_clusters <- 0L
          n_total_obs <- 0L
        } else {
          treated_las <- unique(subset_data[get(treat_var) == 1, local_authority])
          n_treated_clusters <- length(treated_las)
          n_total_obs <- nrow(subset_data)
        }
        
        diagnostic_results[[idx]] <- data.table(
          Treatment = treat_var,
          Matching_Core = matching_core,
          Specification = spec_name,
          Caliper = thresh_name,
          Treated_Clusters = n_treated_clusters,
          Total_Observations = n_total_obs
        )
        idx <- idx + 1L
      }
    }
    rm(local_env)
  }
}

diagnostic_df <- rbindlist(diagnostic_results)

# 2. Reshape to wide format for side-by-side comparison
diagnostic_wide <- dcast(
  diagnostic_df, 
  Treatment + Matching_Core + Specification ~ Caliper, 
  value.var = c("Treated_Clusters", "Total_Observations")
)

# Sort by the strictest caliper's cluster count to find the problem models fast
setorder(diagnostic_wide, Treated_Clusters_PS_0.1)

print(head(diagnostic_wide, 15))

# 3. Export using paths defined in 00_setup.R
diagnostic_output_path <- file.path(SUMMARY_TABLES_DIR, "cluster_diagnostics_all_calipers.csv")
fwrite(diagnostic_wide, diagnostic_output_path)

message(sprintf("\nDiagnostic table successfully exported to: %s", diagnostic_output_path))