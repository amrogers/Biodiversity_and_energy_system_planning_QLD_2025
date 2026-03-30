# =============================================================================
# QLD Existing × Model Transmission — Max KV Spatial Join
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: For each biodiversity threshold, joins the modelled transmission
#          lines to the simplified existing network, recording the maximum
#          modelled kV and line count intersecting each existing corridor.
#          Outputs shapefiles and CSVs for downstream upgrade calculations.
#
# Input:  results/transmission_processing/tx2/TX_domestic_layers/
#              transmission_y2050_t{N}.shp
#         BESP_data_qld_2025/Spatial_reference_data/
#              QLD_existing_tx_simplified_buff_no_overlap2.shp
#         (paths$existing_tx_simp in _paths.R)
#
# Output: results/transmission_processing/tx2/existing_model_tx_max_join/
#              existing_model_tx_join_{N}.shp
#              existing_model_tx_join_{N}.csv
#              tx_join_summary.csv   (combined across thresholds)
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, parallel, purrr, readr, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
tx_scenario    <- "tx2"   # "tx1" or "tx2"
thresholds     <- c(0, 10, 30, 50, 70, 90)
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

tx_proc_base  <- if (tx_scenario == "tx1") tx1_processing else tx2_processing

model_tx_dir  <- file.path(tx_proc_base, "TX_domestic_layers")
output_dir    <- file.path(tx_proc_base, "existing_model_tx_max_join")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

if (!file.exists(paths$existing_tx_simp)) {
  stop("Simplified existing TX not found:\n  ", paths$existing_tx_simp,
       "\n\nPlace QLD_existing_tx_simplified_buff_no_overlap2.shp in:\n  ",
       dirname(paths$existing_tx_simp))
}

# =============================================================================
# 2. Per-threshold Processing Function
# =============================================================================

process_threshold <- function(threshold) {
  tx_file     <- file.path(model_tx_dir,
                           sprintf("transmission_y2050_t%d.shp", threshold))
  out_shp     <- file.path(output_dir,
                           sprintf("existing_model_tx_join_%d.shp", threshold))
  out_csv     <- file.path(output_dir,
                           sprintf("existing_model_tx_join_%d.csv", threshold))

  if (!file.exists(tx_file)) {
    warning(sprintf("Model TX not found for threshold %d: %s", threshold, tx_file))
    return(invisible(NULL))
  }

  if (file.exists(out_shp) && file.exists(out_csv) && !overwrite_mode) {
    cat(sprintf("  Exists (skip): threshold %d\n", threshold))
    return(invisible(NULL))
  }

  existing_tx <- sf::st_read(paths$existing_tx_simp, quiet = TRUE)
  model_tx    <- sf::st_read(tx_file, quiet = TRUE)

  if (sf::st_crs(existing_tx) != sf::st_crs(model_tx)) {
    model_tx <- sf::st_transform(model_tx, sf::st_crs(existing_tx))
  }

  results <- existing_tx %>%
    rowwise() %>%
    mutate(
      max_QLD_model_tx = {
        intersecting <- model_tx[sf::st_intersects(geometry, model_tx)[[1]], ]
        if (nrow(intersecting) > 0) max(intersecting$kv, na.rm = TRUE) else NA_real_
      },
      line_count = {
        intersecting <- model_tx[sf::st_intersects(geometry, model_tx)[[1]], ]
        nrow(intersecting)
      }
    ) %>%
    ungroup()

  sf::st_write(results, out_shp, append = FALSE, quiet = TRUE)
  results %>% sf::st_drop_geometry() %>% write.csv(out_csv, row.names = FALSE)
  cat(sprintf("  ✓ Completed: threshold %d\n", threshold))
}

# =============================================================================
# 3. Run (parallel)
# =============================================================================

num_cores <- min(parallel::detectCores() - 1, length(thresholds))
cat(sprintf(">>> Running %d thresholds on %d cores...\n",
            length(thresholds), num_cores))

cl <- makeCluster(num_cores)
clusterEvalQ(cl, { library(sf); library(dplyr) })
clusterExport(cl, c("process_threshold", "thresholds", "model_tx_dir",
                    "output_dir", "overwrite_mode", "paths"))
parLapply(cl, thresholds, process_threshold)
stopCluster(cl)

# =============================================================================
# 4. Combined Summary
# =============================================================================

cat("\n>>> Building combined summary...\n")
summary_csv <- file.path(output_dir, "tx_join_summary.csv")

if (!file.exists(summary_csv) || overwrite_mode) {
  combined <- map_dfr(thresholds, function(t) {
    f <- file.path(output_dir, sprintf("existing_model_tx_join_%d.csv", t))
    if (!file.exists(f)) return(NULL)
    read_csv(f, show_col_types = FALSE) %>%
      filter(!is.na(max_QLD_model_tx), max_QLD_model_tx != 0) %>%
      group_by(threshold = t, mx_nrb_) %>%
      summarise(total_length_km = sum(length_km, na.rm = TRUE), .groups = "drop")
  })

  write_csv(combined, summary_csv)
  cat("✓ Summary saved to:", summary_csv, "\n")
}

cat("\n=== MAX KV JOIN COMPLETE ===\n")
cat("Outputs:", output_dir, "\n")
