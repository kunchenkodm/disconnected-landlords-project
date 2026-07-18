# Script: 10c_generate_dashboard.R
# Purpose: Project 1 (Vintage of Buyer / Offshore Ownership) dashboard builder.
#          The single place dashboard artefacts are produced. It refits NOTHING:
#          it reads the fitted-model store from 10b_vintage_spec_clustering.R
#          (vintage_models_<suffix>.rds) and serialises every dashboard asset
#          from those objects.
#
#          Steps:
#            1. Read the model store (rpart trees + both hclust objects +
#               dashboard-only tables).
#            2. Emit per-target tree-node JSON + cptable CSV.
#            3. Emit the dendrogram JSON(s) + cluster_membership CSV.
#            4. Write the dashboard-only CSVs (spec_features, ppd_contrast, ...).
#            5. Assemble the single bundle JSON the dashboard consumes.
#            6. Compile a self-contained HTML (bundle baked into the template).
#
# Reads (output/project1_vintage/):
#   vintage_models_<GEO>[..].rds                (from 10b)
#   signflip_pairs_<GEO>[..].csv                (from 10)
#   signflip_summary_<GEO>[..].csv              (from 10, optional)
#   feature_importance_<target>_<GEO>[..].csv   (from 10b, per target)
# Reads (output/summary_tables/): outcome_metadata.csv, treatment_taxonomy.csv (optional)
# Reads (project root): vintage_dashboard.html  (template with the embed slot)
#
# Writes (output/project1_vintage/):
#   tree_<target>_{cptable.csv, nodes_full.json, nodes_pruned.json}
#   dendrogram[_ppd]_<GEO>[..].json , cluster_membership_<GEO>[..].csv
#   spec_features_<GEO>[..].csv , ppd_contrast_<GEO>[..].csv
#   per_family_tree_summary_<GEO>[..].csv , vintage_bundle_<GEO>[..].json
# Writes (project root):
#   vintage_dashboard_<GEO>[..].html   (self-contained, data baked in)
#
# Env vars (must match the 10b run that wrote the store):
#   PROJECT1_GEO (default "LA") , PROJECT1_CORE_ONLY (default "TRUE")
#
# To produce both core and non-core dashboards:
#   for (co in c("TRUE","FALSE")) {
#     Sys.setenv(PROJECT1_GEO="LA", PROJECT1_CORE_ONLY=co)
#     source("scripts/10b_vintage_spec_clustering.R")
#     source("scripts/10c_generate_dashboard.R")
#   }

rm(list = setdiff(ls(), c("script", "pipeline.start.time"))); gc()

library(data.table)
library(here)
library(rpart)
library(jsonlite)

source(here::here("scripts", "00_setup.R"))

wc_suffix <- if (isTRUE(WITHIN_CORPORATE)) "_wc" else ""

WINNER_GEO <- Sys.getenv("PROJECT1_GEO", unset = "LA")
if (!nzchar(WINNER_GEO)) WINNER_GEO <- "LA"

# Core-only scope (default TRUE): must match scripts 10/10b so the right
# artifacts (suffixed _core) are discovered and bundled.
CORE_ONLY   <- Sys.getenv("PROJECT1_CORE_ONLY", unset = "TRUE") != "FALSE"
core_suffix <- if (CORE_ONLY) "_core" else ""

geo_suffix <- paste0("_", WINNER_GEO, wc_suffix, core_suffix)

OUT_DIR <- file.path(OUTPUT_ROOT, "project1_vintage")
SUM_DIR <- file.path(OUTPUT_ROOT, "summary_tables")

message("===================================================================")
message("  10c_generate_dashboard.R")
message(sprintf("  Geography:        %s", WINNER_GEO))
message(sprintf("  Within-corporate: %s", if (isTRUE(WITHIN_CORPORATE)) "TRUE" else "FALSE"))
message(sprintf("  Suffix:           %s", geo_suffix))
message("===================================================================")

# 1. Read the model store -------------------------------------------------

models_path <- file.path(OUT_DIR, paste0("vintage_models", geo_suffix, ".rds"))
if (!file.exists(models_path)) {
  stop("Model store not found: ", models_path,
       "\n  Run 10b_vintage_spec_clustering.R first with the matching ",
       "PROJECT1_GEO / PROJECT1_CORE_ONLY settings.")
}
MODELS <- readRDS(models_path)

# Fail fast if the env-var suffix disagrees with the store (e.g. running this
# with PROJECT1_CORE_ONLY=FALSE against a _core store would mix artefacts).
store_suffix <- MODELS$meta$geo_suffix
if (!identical(store_suffix, geo_suffix)) {
  stop("Suffix mismatch: model store was written for '", store_suffix,
       "' but this run computed '", geo_suffix,
       "'. Set PROJECT1_GEO / PROJECT1_CORE_ONLY to match the 10b run.")
}
message(sprintf("  Loaded model store: %d trees, dendro=%s, dendro_ppd=%s",
                length(MODELS$trees),
                if (is.null(MODELS$dendro)) "no" else "yes",
                if (is.null(MODELS$dendro_ppd)) "no" else "yes"))

# Helpers -----------------------------------------------------------------

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  fread(path, na.strings = c("NA", ""))
}
read_json_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::read_json(path, simplifyVector = FALSE)
}
nz <- function(x) !is.null(x) && length(x) > 0L

# Tree-node serialiser — moved verbatim from 10b so the JSON stays byte-identical.
write_tree_json <- function(tree, path) {
  if (is.null(tree)) return(invisible(NULL))
  frame <- cbind(node_id = as.integer(rownames(tree$frame)),
                 as.data.frame(tree$frame, stringsAsFactors = FALSE))
  # yval2 is a matrix (counts per class + probabilities) for classification
  # trees; flatten it so jsonlite can serialise it as a row-wise array.
  if (!is.null(frame$yval2)) {
    yval2_mat <- as.matrix(frame$yval2)
    frame$yval2 <- NULL
    frame$yval2 <- lapply(seq_len(nrow(yval2_mat)),
                          function(i) as.numeric(yval2_mat[i, ]))
  }
  splits <- if (!is.null(tree$splits) && nrow(tree$splits) > 0L) {
    cbind(split_idx = seq_len(nrow(tree$splits)),
          var_name  = rownames(tree$splits),
          as.data.frame(tree$splits, stringsAsFactors = FALSE))
  } else NULL
  csplit <- if (!is.null(tree$csplit)) {
    if (is.matrix(tree$csplit))
      lapply(seq_len(nrow(tree$csplit)),
             function(i) as.integer(tree$csplit[i, ]))
    else as.integer(tree$csplit)
  } else NULL
  jsonlite::write_json(
    list(frame   = frame,
         splits  = splits,
         csplit  = csplit,
         xlevels = attr(tree, "xlevels"),
         method  = tree$method,
         ylevels = attr(tree, "ylevels")),
    path = path, auto_unbox = TRUE, na = "null", dataframe = "rows",
    pretty = FALSE
  )
}

# Dendrogram serialiser — reproduces the exact field order 10b used (spec_axis,
# when present, is inserted just before geography), so the JSON stays identical.
write_dendro_json <- function(dd, path, spec_axis = NULL) {
  if (is.null(dd)) return(invisible(FALSE))
  hc <- dd$hc
  out <- list(
    merge        = hc$merge,
    order        = as.integer(hc$order),
    labels       = as.character(hc$labels),
    height       = as.numeric(hc$height),
    method       = hc$method,
    k_clusters   = as.integer(dd$k_clusters),
    palette      = as.character(dd$cluster_pal),
    leaves       = dd$cluster_summary,
    cluster_meta = dd$cluster_meta,
    row_sign     = data.frame(leaf_label  = names(dd$row_sign),
                              sign        = unname(dd$row_sign),
                              median_coef = unname(dd$row_med[names(dd$row_sign)]),
                              stringsAsFactors = FALSE)
  )
  if (!is.null(spec_axis)) out$spec_axis <- spec_axis
  out$geography        <- WINNER_GEO
  out$within_corporate <- WITHIN_CORPORATE
  jsonlite::write_json(out, path = path, auto_unbox = TRUE, na = "null",
                       dataframe = "rows", pretty = FALSE)
  invisible(TRUE)
}

# 2. Per-target tree assets (cptable CSV + node JSON) ---------------------

targets <- sort(names(MODELS$trees))
message(sprintf("  Emitting %d tree target(s):", length(targets)))
for (t in targets) {
  r <- MODELS$trees[[t]]
  fwrite(r$cptable,
         file.path(OUT_DIR, paste0("tree_", t, "_cptable", geo_suffix, ".csv")))
  write_tree_json(r$tree_full,
                  file.path(OUT_DIR, paste0("tree_", t, "_nodes_full",   geo_suffix, ".json")))
  write_tree_json(r$tree_pruned,
                  file.path(OUT_DIR, paste0("tree_", t, "_nodes_pruned", geo_suffix, ".json")))
  message("    - ", t)
}

# 3. Dendrogram JSON + cluster-membership CSV -----------------------------

if (!is.null(MODELS$dendro)) {
  fwrite(MODELS$dendro$cluster_summary,
         file.path(OUT_DIR, paste0("cluster_membership", geo_suffix, ".csv")))
  write_dendro_json(MODELS$dendro,
                    file.path(OUT_DIR, paste0("dendrogram", geo_suffix, ".json")))
  message(sprintf("  Wrote cluster_membership + dendrogram JSON (%d leaves)",
                  nrow(MODELS$dendro$cluster_summary)))
} else {
  message("  [SKIP] main dendrogram: none in store.")
}

if (!is.null(MODELS$dendro_ppd)) {
  write_dendro_json(MODELS$dendro_ppd,
                    file.path(OUT_DIR, paste0("dendrogram_ppd", geo_suffix, ".json")),
                    spec_axis = MODELS$dendro_ppd$spec_axis)
  message("  Wrote PPD dendrogram JSON")
} else {
  message("  [SKIP] PPD dendrogram: none in store.")
}

# 4. Dashboard-only CSVs from the store -----------------------------------

tbl <- MODELS$tables
if (nz(tbl$spec_features))
  fwrite(tbl$spec_features, file.path(OUT_DIR, paste0("spec_features", geo_suffix, ".csv")))
if (nz(tbl$ppd_contrast))
  fwrite(tbl$ppd_contrast,  file.path(OUT_DIR, paste0("ppd_contrast",  geo_suffix, ".csv")))
if (nz(tbl$per_family_tree_summary))
  fwrite(tbl$per_family_tree_summary,
         file.path(OUT_DIR, paste0("per_family_tree_summary", geo_suffix, ".csv")))
message("  Wrote dashboard-only CSVs (spec_features, ppd_contrast, summaries)")

# 5. Assemble the bundle (reads the freshly-written granular files) -------

pairs <- read_csv_safe(file.path(OUT_DIR, paste0("signflip_pairs", geo_suffix, ".csv")))
if (is.null(pairs) || nrow(pairs) == 0L) {
  stop("No signflip_pairs CSV found at ", geo_suffix,
       ". Run scripts/10_vintage_signflip_table.R first.")
}
message(sprintf("  pairs:            %d rows, %d cols", nrow(pairs), ncol(pairs)))

summary_dt <- read_csv_safe(file.path(OUT_DIR, paste0("signflip_summary", geo_suffix, ".csv")))
if (nz(summary_dt)) message(sprintf("  summary:          %d rows", nrow(summary_dt)))

features <- read_csv_safe(file.path(OUT_DIR, paste0("spec_features", geo_suffix, ".csv")))
if (nz(features)) message(sprintf("  features:         %d rows", nrow(features)))

clusters <- read_csv_safe(file.path(OUT_DIR, paste0("cluster_membership", geo_suffix, ".csv")))
if (nz(clusters)) message(sprintf("  clusters:         %d rows", nrow(clusters)))

per_family_summary <- read_csv_safe(file.path(OUT_DIR, paste0("per_family_tree_summary", geo_suffix, ".csv")))
if (nz(per_family_summary)) message(sprintf("  per_family_sum:   %d rows", nrow(per_family_summary)))

dendrogram <- read_json_safe(file.path(OUT_DIR, paste0("dendrogram", geo_suffix, ".json")))
if (nz(dendrogram)) message("  dendrogram:       loaded")

# PPD-layer tables (ppd_contrast/ppd_summary) and the PPD-collapsed dendrogram
# JSON are no longer bundled: the dashboard's old "Transaction info" tab was
# folded into the clustering tab (ppd_direction/ppd_lose_sig trees). The
# writeup still uses the PDFs/CSVs that 10b/10c write to OUT_DIR.

trees <- list()
for (t in targets) {
  trees[[t]] <- list(
    full       = read_json_safe(file.path(OUT_DIR, paste0("tree_", t, "_nodes_full",   geo_suffix, ".json"))),
    pruned     = read_json_safe(file.path(OUT_DIR, paste0("tree_", t, "_nodes_pruned", geo_suffix, ".json"))),
    cptable    = read_csv_safe(file.path(OUT_DIR, paste0("tree_", t, "_cptable",       geo_suffix, ".csv"))),
    importance = read_csv_safe(file.path(OUT_DIR, paste0("feature_importance_", t,     geo_suffix, ".csv")))
  )
}
message(sprintf("  trees:            %d targets bundled", length(trees)))

outcome_meta <- read_csv_safe(file.path(SUM_DIR, "outcome_metadata.csv"))
taxonomy     <- read_csv_safe(file.path(SUM_DIR, "treatment_taxonomy.csv"))
if (nz(outcome_meta)) message(sprintf("  outcome_meta:     %d rows", nrow(outcome_meta)))
if (nz(taxonomy))     message(sprintf("  taxonomy:         %d rows", nrow(taxonomy)))

bundle <- list(
  meta = list(
    geography         = WINNER_GEO,
    within_corporate  = isTRUE(WITHIN_CORPORATE),
    core_only         = isTRUE(CORE_ONLY),
    suffix            = geo_suffix,
    generated         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    n_pairs           = nrow(pairs),
    n_features        = if (nz(features)) nrow(features) else 0L,
    n_clusters_rows   = if (nz(clusters)) nrow(clusters) else 0L,
    targets           = targets
  ),
  pairs              = pairs,
  signflip_summary   = summary_dt,
  features           = features,
  clusters           = clusters,
  per_family_summary = per_family_summary,
  dendrogram         = dendrogram,
  trees              = trees,
  outcome_metadata   = outcome_meta,
  treatment_taxonomy = taxonomy
)

bundle_path <- file.path(OUT_DIR, paste0("vintage_bundle", geo_suffix, ".json"))

# auto_unbox=TRUE for scalars, dataframe="rows" so data.tables become arrays of
# objects (what the dashboard expects), na="null" so missing values are JSON null.
jsonlite::write_json(bundle, bundle_path,
                     auto_unbox = TRUE,
                     dataframe  = "rows",
                     na         = "null",
                     null       = "null",
                     pretty     = FALSE)

bundle_size_mb <- file.info(bundle_path)$size / 1024^2
message(sprintf("\n  Wrote bundle: %s (%.2f MB)", basename(bundle_path), bundle_size_mb))

# 6. Compile the self-contained HTML --------------------------------------

template_path <- here::here("vintage_dashboard.html")
if (!file.exists(template_path)) {
  warning("Dashboard template not found at ", template_path,
          " — skipping self-contained HTML compile.")
} else {
  template     <- readChar(template_path, file.info(template_path)$size, useBytes = TRUE)
  # Anchor the start to the actual assignment so stray marker tokens in comments
  # or prose can never misdirect the splice.
  start_anchor <- "window.__VINTAGE_BUNDLE__ = /*__BUNDLE_JSON__*/"
  end_tok      <- "/*__END__*/"
  s_pos <- regexpr(start_anchor, template, fixed = TRUE)
  # End token must be the first one AT OR AFTER the anchored start.
  e_rel <- if (s_pos > 0L)
    regexpr(end_tok, substr(template, s_pos, nchar(template)), fixed = TRUE) else -1L
  if (s_pos < 0L || e_rel < 0L) {
    warning("Embed slot not found in vintage_dashboard.html. ",
            "Expected the line: window.__VINTAGE_BUNDLE__ = /*__BUNDLE_JSON__*/null/*__END__*/; ",
            "Self-contained HTML NOT written.")
  } else {
    e_pos <- s_pos + e_rel - 1L
    # Pure fixed-string splice (no regex on the JSON, which contains backslashes
    # that sub() would mis-read as backreferences). Keep the start/end tokens so
    # the compiled file stays re-compilable; drop whatever sat between them (the
    # placeholder `null`).
    bundle_txt <- readChar(bundle_path, file.info(bundle_path)$size, useBytes = TRUE)
    pre        <- substr(template, 1L, s_pos + nchar(start_anchor) - 1L)  # up to & incl start token
    post       <- substr(template, e_pos, nchar(template))               # from end token onward
    compiled   <- paste0(pre, bundle_txt, post)
    html_out   <- here::here(paste0("vintage_dashboard", geo_suffix, ".html"))
    con <- file(html_out, open = "wb"); writeBin(charToRaw(compiled), con); close(con)
    html_size_mb <- file.info(html_out)$size / 1024^2
    message(sprintf("  Wrote self-contained dashboard: %s (%.2f MB)",
                    basename(html_out), html_size_mb))
  }
}

message(sprintf("\n  10c_generate_dashboard.R complete [%s].",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
