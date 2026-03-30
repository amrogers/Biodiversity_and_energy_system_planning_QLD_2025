# =============================================================================
# QLD Modelled Transmission — Easement Area Calculation
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: For each biodiversity threshold, buffers modelled transmission
#          centrelines by a voltage-based easement width, dissolves overlapping
#          buffers, and calculates the total easement area (km²). Outputs
#          buffered shapefiles and a summary area table.
#
# Input:  results/transmission_processing/tx1/TX_domestic_layers/
#              transmission_y2050_t{N}.shp
#
# Output: results/transmission_processing/tx1/buffered_centerlines/
#              buffered_centerline_t{N}.shp
#         results/transmission_processing/tx1/tx_simplified_buffered/
#              tx_simplified_buffered_t{N}.shp
#              threshold_areas.csv
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, future.apply, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
tx_scenario    <- "tx1"   # "tx1" or "tx2"
thresholds     <- c(0, 10, 30, 50, 70, 90)
projected_crs  <- 28355   # GDA2020 MGA Zone 55 (for accurate area/length)
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

tx_proc_base        <- if (tx_scenario == "tx1") tx1_processing else tx2_processing
model_tx_dir        <- file.path(tx_proc_base, "TX_domestic_layers")
centerline_out_dir  <- file.path(tx_proc_base, "buffered_centerlines")
final_out_dir       <- file.path(tx_proc_base, "tx_simplified_buffered")

for (d in c(centerline_out_dir, final_out_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# =============================================================================
# 2. Easement Width Lookup (metres)
# =============================================================================

easement_radius_m <- function(max_kv) {
  if (is.na(max_kv) || !is.finite(max_kv)) return(15)
  dplyr::case_when(
    max_kv > 500             ~ 35,
    max_kv >= 275            ~ 30,
    max_kv >= 60 & max_kv <= 220 ~ 20,
    max_kv > 0               ~ 15,
    TRUE                     ~ 15
  )
}

# =============================================================================
# 3. Processing Function
# =============================================================================

process_threshold <- function(threshold) {
  in_file      <- file.path(model_tx_dir,
                             sprintf("transmission_y2050_t%d.shp", threshold))
  out_cl       <- file.path(centerline_out_dir,
                             sprintf("buffered_centerline_t%d.shp", threshold))
  out_final    <- file.path(final_out_dir,
                             sprintf("tx_simplified_buffered_t%d.shp", threshold))

  if (!file.exists(in_file)) {
    message(sprintf("Input not found for threshold %d — skipping", threshold))
    return(NULL)
  }

  if (file.exists(out_final) && !overwrite_mode) {
    message(sprintf("Exists (skip): threshold %d", threshold))
    area_km2 <- as.numeric(
      units::set_units(sf::st_area(sf::st_read(out_final, quiet = TRUE)), "km^2")
    )
    return(data.frame(threshold = threshold, area_sq_km = sum(area_km2)))
  }

  tx_lines <- sf::st_read(in_file, quiet = TRUE) %>%
    sf::st_transform(projected_crs) %>%
    sf::st_make_valid()

  # Merge lines and find centreline
  centerlines <- sf::st_combine(sf::st_geometry(tx_lines)) %>%
    sf::st_line_merge() %>%
    sf::st_make_valid()

  # Save 500 m buffered centreline
  cl_buf <- sf::st_buffer(centerlines, 500) %>% sf::st_make_valid()
  sf::st_write(sf::st_sf(geometry = cl_buf), out_cl,
               delete_layer = TRUE, quiet = TRUE)

  # Get max kV within centreline buffer
  cl_data <- sf::st_intersection(tx_lines, cl_buf) %>%
    dplyr::summarise(max_kv = max(kv, na.rm = TRUE), .groups = "drop")

  radius <- easement_radius_m(cl_data$max_kv)
  message(sprintf("  Threshold %d: max kV = %g, easement radius = %d m",
                  threshold, cl_data$max_kv, radius))

  final_buf <- sf::st_buffer(centerlines, radius) %>% sf::st_make_valid()
  sf::st_write(sf::st_sf(geometry = final_buf), out_final,
               delete_layer = TRUE, quiet = TRUE)

  area_km2 <- as.numeric(
    units::set_units(sf::st_area(final_buf), "km^2")
  )
  message(sprintf("  ✓ Saved threshold %d (area = %.2f km²)", threshold, sum(area_km2)))

  data.frame(threshold = threshold, area_sq_km = sum(area_km2))
}

# =============================================================================
# 4. Run in Parallel and Save Area Table
# =============================================================================

plan(multisession)
results <- future_lapply(thresholds, process_threshold)
plan(sequential)

valid_results <- Filter(Negate(is.null), results)
if (length(valid_results) > 0) {
  area_table <- do.call(rbind, valid_results)
  area_csv   <- file.path(final_out_dir, "threshold_areas.csv")
  write.csv(area_table, area_csv, row.names = FALSE)
  cat("\n✓ Area table saved to:", area_csv, "\n")
  print(area_table)
} else {
  message("No valid results to save.")
}

cat("\n=== EASEMENT AREA CALCULATION COMPLETE ===\n")
