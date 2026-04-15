# =============================================================================
# QLD Transmission — Modelled vs Existing Network Summary
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: Alternative summary approach — for each threshold, clips modelled
#          TX to the existing network buffer and summarises segment lengths
#          and voltage class. Complements QLD_new_tx_processing_summary.R
#          which focuses on NEW transmission only.
#
# Input:  results/transmission_processing/tx1/TX_domestic_layers/
#              transmission_y2050_t{N}.shp
#         BESP_data_qld_2025/Spatial_reference_data/
#              Electricity_Transmission_Lines_1km_buff.shp
#         (paths$existing_tx_buff in _paths.R)
#
# Output: results/transmission_processing/tx1/model_existing_summaries/
#              threshold_{N}_model_existing_summary.csv
#              QLD_model_existing_combined_summary.csv
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, stringr, here)
source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# --- USER CONTROL ---
if (!exists("tx_scenario"))    tx_scenario    <- "tx1"    # "tx1" or "tx2"
if (!exists("overwrite_mode")) overwrite_mode <- FALSE
thresholds <- c(0, 10, 30, 50, 70, 90)

# =============================================================================
# 1. Path Configuration
# =============================================================================

tx_proc_base <- if (tx_scenario == "tx1") tx1_processing else tx2_processing

model_tx_dir <- file.path(tx_proc_base, "TX_domestic_layers")
output_dir   <- file.path(tx_proc_base, "model_existing_summaries")
summary_csv  <- file.path(output_dir, "QLD_model_existing_combined_summary.csv")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

model_files <- list.files(model_tx_dir,
                          pattern = "transmission_y2050_t\\d+\\.shp$",
                          full.names = TRUE)
cat(sprintf(">>> Found %d model files\n", length(model_files)))

# =============================================================================
# 2. Load Existing TX Buffer
# =============================================================================

cat(">>> Loading existing TX buffer...\n")
existing_tx <- sf::st_read(paths$existing_tx_buff, quiet = TRUE)

# =============================================================================
# 3. Process Each Threshold
# =============================================================================

process_threshold <- function(file_path) {
  threshold <- str_extract(basename(file_path), "t\\d+") %>%
    str_replace("t", "") %>%
    as.integer()

  out_csv <- file.path(output_dir,
                       paste0("threshold_", threshold, "_model_existing_summary.csv"))

  if (file.exists(out_csv) && !overwrite_mode) {
    cat(sprintf("  Exists (skip): threshold %d\n", threshold))
    return(read.csv(out_csv, stringsAsFactors = FALSE))
  }

  cat(sprintf("\n>>> Processing threshold %d\n", threshold))

  model_layer <- sf::st_read(file_path, quiet = TRUE)
  if (sf::st_crs(model_layer) != sf::st_crs(existing_tx)) {
    model_layer <- sf::st_transform(model_layer, sf::st_crs(existing_tx))
  }

  clipped <- sf::st_difference(model_layer, existing_tx)

  layer_summary <- clipped %>%
    filter(!is.na(kv) & kv != 0) %>%
    mutate(length_km = as.numeric(sf::st_length(geometry) / 1000)) %>%
    group_by(kv) %>%
    summarise(total_length_km = sum(length_km), .groups = "drop") %>%
    mutate(layer_name = paste0("threshold_", threshold)) %>%
    select(layer_name, capacity = kv, length_km = total_length_km) %>%
    sf::st_drop_geometry()

  write.csv(layer_summary, out_csv, row.names = FALSE)
  cat(sprintf("  ✓ Saved: threshold_%d_model_existing_summary.csv\n", threshold))

  layer_summary
}

# =============================================================================
# 4. Combine and Save
# =============================================================================

if (!file.exists(summary_csv) || overwrite_mode) {
  all_summaries    <- lapply(model_files, process_threshold)
  combined_summary <- do.call(rbind, all_summaries)
  write.csv(combined_summary, summary_csv, row.names = FALSE)
  cat("\n✓ Combined summary saved to:", summary_csv, "\n")
} else {
  cat(">>> Combined summary exists (overwrite_mode = FALSE).\n")
}

cat("\n=== MODEL/EXISTING TX SUMMARY COMPLETE ===\n")
