# =============================================================================
# QLD Existing Transmission Lines — Reproject to GDA2020
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: One-off utility. Reprojects the existing QLD transmission lines
#          shapefile to GDA2020 MGA Zone 55 (EPSG:7855) and calculates
#          line lengths in km. Output is used as reference data by the
#          transmission pipeline scripts.
#
# Input:  BESP_data_qld_2025/Spatial_reference_data/
#              Electricity_Transmission_Lines.shp
#         (paths$existing_tx in _paths.R)
#
# Output: BESP_data_qld_2025/Spatial_reference_data/
#              QLD_existing_tx_projected.shp
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, here)
source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# =============================================================================
# 1. Path Configuration
# =============================================================================

projected_crs <- 7855  # GDA2020 MGA Zone 55

out_shp <- file.path(dirname(paths$existing_tx), "QLD_existing_tx_projected.shp")

if (!file.exists(paths$existing_tx)) {
  stop("Existing TX shapefile not found:\n  ", paths$existing_tx,
       "\n\nPlace Electricity_Transmission_Lines.shp in:\n  ",
       dirname(paths$existing_tx))
}

# =============================================================================
# 2. Reproject and Calculate Lengths
# =============================================================================

cat(">>> Reading existing TX lines...\n")
lines <- sf::st_read(paths$existing_tx, quiet = TRUE)

cat(">>> Reprojecting to GDA2020 MGA Zone 55...\n")
lines_proj <- sf::st_transform(lines, crs = projected_crs) %>%
  dplyr::mutate(length_km_projected = as.numeric(sf::st_length(.) / 1000))

sf::st_write(lines_proj, out_shp, delete_dsn = TRUE, quiet = FALSE)

cat("✓ Projected shapefile saved to:", out_shp, "\n")
cat(sprintf("  Total features: %d\n", nrow(lines_proj)))
cat(sprintf("  Total length:   %.1f km\n", sum(lines_proj$length_km_projected)))
