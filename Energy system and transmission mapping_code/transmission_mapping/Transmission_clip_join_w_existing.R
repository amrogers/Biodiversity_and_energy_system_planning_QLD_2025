# =============================================================================
# Transmission — Clip and Join Model TX with Existing Network
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: For each biodiversity threshold, clips modelled transmission lines
#          to segments within 1 km of the existing network, then spatially
#          joins those clipped segments back to the existing lines. Outputs
#          per-threshold CSV attribute tables used in upgrade area calculations.
#
# Input:  results/transmission_processing/tx1/TX_domestic_layers/
#              transmission_y2050_t{N}.shp
#         BESP_data_qld_2025/Spatial_reference_data/
#              Electricity_Transmission_Lines.shp
#              Electricity_Transmission_Lines_1km_buff.shp
#         (paths$existing_tx, paths$existing_tx_buff in _paths.R)
#
# Output: results/transmission_processing/tx1/tx_overlap_tables/
#              T{N}_existing_join.csv  (one per threshold)
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, purrr, stringr, future, furrr, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
tx_scenario    <- "tx1"   # "tx1" or "tx2"
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

tx_proc_base    <- if (tx_scenario == "tx1") tx1_processing else tx2_processing

shapefile_dir   <- file.path(tx_proc_base, "TX_domestic_layers")
output_dir      <- file.path(tx_proc_base, "tx_overlap_tables")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (p in c(paths$existing_tx, paths$existing_tx_buff, shapefile_dir)) {
  if (!file.exists(p) && !dir.exists(p)) {
    stop("Required input not found:\n  ", p,
         "\n\nCheck _paths.R and run Transmission_save_layers_as_shapefiles.R first.")
  }
}

threshold_layers <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE)
cat(sprintf(">>> Found %d threshold shapefiles\n", length(threshold_layers)))

# =============================================================================
# 2. Load Reference Data (CRS from first model layer)
# =============================================================================

cat(">>> Loading existing TX layers...\n")
ref_crs          <- sf::st_crs(sf::st_read(threshold_layers[1], quiet = TRUE))
existing_tx      <- sf::st_read(paths$existing_tx,      quiet = TRUE) %>%
  sf::st_transform(ref_crs)
existing_tx_buff <- sf::st_read(paths$existing_tx_buff, quiet = TRUE) %>%
  sf::st_transform(ref_crs)

# =============================================================================
# 3. Per-threshold Processing Function
# =============================================================================

process_layer <- function(layer_path) {
  layer_name <- str_remove(basename(layer_path), "\\.shp$")
  threshold  <- str_extract(layer_name, "t\\d+")
  csv_name   <- paste0("T", str_remove(threshold, "t"), "_existing_join.csv")
  out_path   <- file.path(output_dir, csv_name)

  if (file.exists(out_path) && !overwrite_mode) {
    message(sprintf("Exists (skip): %s", csv_name))
    return(invisible(NULL))
  }

  message(sprintf("Processing: %s", layer_name))

  model_tx <- sf::st_read(layer_path, quiet = TRUE) %>%
    sf::st_make_valid()

  if (nrow(model_tx) == 0) {
    message(sprintf("  Skipping empty layer: %s", layer_name))
    return(invisible(NULL))
  }

  # Clip modelled TX to existing TX 1 km buffer
  model_clip <- sf::st_intersection(model_tx, existing_tx_buff) %>%
    sf::st_make_valid()

  if (nrow(model_clip) == 0) {
    message(sprintf("  No intersection found for: %s", layer_name))
    return(invisible(NULL))
  }

  # Buffer clipped model TX then clip existing lines to that buffer
  model_buff    <- sf::st_buffer(model_clip, dist = 1000) %>% sf::st_make_valid()
  existing_clip <- sf::st_intersection(existing_tx, model_buff) %>% sf::st_make_valid()

  # Spatial join existing (clipped) back to model (clipped)
  joined <- sf::st_join(existing_clip, model_clip, join = sf::st_intersects) %>%
    mutate(threshold = threshold)

  sf::st_drop_geometry(joined) %>% write.csv(out_path, row.names = FALSE)
  message(sprintf("  ✓ Saved: %s", csv_name))
}

# =============================================================================
# 4. Parallel Execution
# =============================================================================

plan(multisession, workers = max(1, parallel::detectCores() - 1))
future_walk(threshold_layers, process_layer)
plan(sequential)

cat("\n=== CLIP / JOIN COMPLETE ===\n")
cat("Outputs:", output_dir, "\n")
