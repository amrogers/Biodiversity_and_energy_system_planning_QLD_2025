# =============================================================================
# Transmission Pipeline — Step 3: New Transmission Length Summary
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: For each biodiversity protection threshold, clips the modelled
#          transmission lines to remove segments overlapping with the existing
#          QLD transmission network (1 km buffer), then summarises total NEW
#          transmission line length (km) by voltage class. Produces the CSV
#          inputs consumed by transmission_length_tx1_tx2.R (Figure 3).
#
# Pipeline position: Step 3 of 4 for Figure 3
#   Step 1 → Transmission_processing.R
#   Step 2 → Transmission_save_layers_as_shapefiles.R
#   Step 3 → THIS SCRIPT
#   Step 4 → transmission_length_tx1_tx2.R (Figure 3)
#
# Input:  results/transmission_processing/tx1/TX_domestic_layers/
#              transmission_y2050_t{THRESHOLD}.shp
#         BESP_data_qld_2025/Spatial_reference_data/
#              Electricity_Transmission_Lines_1km_buff.shp
#         (paths$existing_tx_buff in _paths.R)
#
# Output: results/transmission_processing/tx1/QLD_threshold_tx_new/
#              threshold_{N}_tx_new.shp        (new-only TX per threshold)
#              threshold_{N}_summary.csv        (kV × length per threshold)
#         results/transmission_processing/tx1/QLD_threshold_tx_new_summaries/
#              QLD_threshold_tx_new_summary.csv (combined all thresholds)
#
# Note: Copy the combined CSV to:
#   BESP_data_qld_2025/Energy_system_model_outputs/tx1_new_transmission_summary.csv
#   (or tx2_...) for use by transmission_length_tx1_tx2.R.
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, stringr, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
tx_scenario    <- "tx1"    # "tx1" or "tx2"
thresholds     <- c(0, 10, 30, 50, 70, 90)
projected_crs  <- 7856     # GDA2020 MGA Zone 56
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

tx_proc_base   <- if (tx_scenario == "tx1") tx1_processing else tx2_processing

model_tx_dir   <- file.path(tx_proc_base, "TX_domestic_layers")
output_shp_dir <- file.path(tx_proc_base, "QLD_threshold_tx_new")
summaries_dir  <- file.path(tx_proc_base, "QLD_threshold_tx_new_summaries")
summary_csv    <- file.path(summaries_dir, "QLD_threshold_tx_new_summary.csv")

for (d in c(output_shp_dir, summaries_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

if (!file.exists(paths$existing_tx_buff)) {
  stop(
    "Existing TX buffer not found:\n  ", paths$existing_tx_buff, "\n\n",
    "Place Electricity_Transmission_Lines_1km_buff.shp in:\n  ",
    file.path(dirname(paths$gdb_tx1), "Spatial_reference_data")
  )
}

model_files <- list.files(model_tx_dir,
                          pattern = "transmission_y2050_t\\d+\\.shp$",
                          full.names = TRUE)
cat(sprintf(">>> Found %d model shapefiles\n", length(model_files)))

# =============================================================================
# 2. Load Existing TX Buffer (once, projected)
# =============================================================================

cat(">>> Loading existing TX buffer...\n")
existing_tx <- sf::st_read(paths$existing_tx_buff, quiet = TRUE) %>%
  sf::st_transform(crs = projected_crs)

# =============================================================================
# 3. Process Each Threshold
# =============================================================================

process_threshold <- function(file_path) {
  threshold <- str_extract(basename(file_path), "t\\d+") %>%
    str_replace("t", "") %>%
    as.integer()

  cat(sprintf("\n>>> Processing threshold %d\n", threshold))

  out_shp     <- file.path(output_shp_dir, paste0("threshold_", threshold, "_tx_new.shp"))
  out_csv     <- file.path(summaries_dir,  paste0("threshold_", threshold, "_summary.csv"))

  if (file.exists(out_shp) && file.exists(out_csv) && !overwrite_mode) {
    cat(sprintf("  Exists (skip): threshold_%d\n", threshold))
    return(read.csv(out_csv, stringsAsFactors = FALSE))
  }

  model_layer <- sf::st_read(file_path, quiet = TRUE) %>%
    sf::st_transform(crs = projected_crs)

  clipped <- sf::st_difference(model_layer, existing_tx)
  cat("  ✓ Spatial difference complete\n")

  # Remove and overwrite existing shapefile components
  for (ext in c(".shp", ".dbf", ".prj", ".shx")) {
    f <- sub("\\.shp$", ext, out_shp)
    if (file.exists(f)) file.remove(f)
  }
  sf::st_write(clipped, out_shp, driver = "ESRI Shapefile",
               delete_layer = TRUE, quiet = TRUE)
  cat(sprintf("  ✓ Saved: %s\n", basename(out_shp)))

  # Per-threshold summary
  layer_summary <- clipped %>%
    filter(!is.na(kv) & kv != 0) %>%
    mutate(length_km = as.numeric(sf::st_length(geometry) / 1000)) %>%
    group_by(kv) %>%
    summarise(total_length_km = sum(length_km), .groups = "drop") %>%
    mutate(layer_name = paste0("threshold_", threshold)) %>%
    select(layer_name, capacity = kv, length_km = total_length_km) %>%
    sf::st_drop_geometry()

  write.csv(layer_summary, out_csv, row.names = FALSE)
  cat(sprintf("  ✓ Summary saved: %s\n", basename(out_csv)))

  layer_summary
}

# =============================================================================
# 4. Combine and Save Overall Summary
# =============================================================================

if (!file.exists(summary_csv) || overwrite_mode) {
  all_summaries   <- lapply(model_files, process_threshold)
  combined_summary <- do.call(rbind, all_summaries)
  write.csv(combined_summary, summary_csv, row.names = FALSE)
  cat("\n✓ Combined summary saved to:", summary_csv, "\n")
} else {
  cat(">>> Combined summary exists (overwrite_mode = FALSE).\n")
}

cat(sprintf("\nNOTE: Copy %s to:\n  %s\nfor use by transmission_length_tx1_tx2.R\n",
            basename(summary_csv),
            file.path(paths$energy_outputs,
                      paste0(tx_scenario, "_new_transmission_summary.csv"))))

cat("\n=== NEW TX SUMMARY (STEP 3) COMPLETE ===\n")
