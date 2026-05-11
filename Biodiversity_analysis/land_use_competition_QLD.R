################################################################################
# Biodiversity Exclusion Area Summary by Zonation Band
#
# Purpose: Recreate BV_exclusion_area_overlap.csv
#          Calculates wind and PV exclusion areas within each Zonation
#          priority band using pure raster cell counting — no polygonisation,
#          no vector intersection.
#
# Approach:
#   1. Load three rasters: rankmap, wind exclusion, PV exclusion
#   2. Verify projections and extents align
#   3. Resample exclusion rasters to rankmap grid if needed
#   4. Read all values into memory as vectors (one pass each)
#   5. For each band: count total cells, count excluded cells, compute areas
#
# Why this is fast:
#   - Exclusion layers are already rasterised TIFFs (no shapefile conversion)
#   - All arithmetic is plain R vector operations (no terra::global() loops)
#   - No polygonisation or vector intersection at any step
#
# Inputs:
#   - Zonation rankmap raster       (0-1 scale, EPSG:3577, 250m)
#   - Wind exclusion raster         (0/1, should be EPSG:3577, 250m)
#   - PV exclusion raster           (0/1, should be EPSG:3577, 250m)
#
# Output columns in CSV:
#   threshold, wind_exclusion, wind_available_area, wind_exclusion_percent,
#   pv_exclusion, pv_available_area, pv_exclusion_percent
#
# Band definitions (Zonation 0-1 scale, rounded to 2dp):
#   "Top 30%"       -> values >= 0.70 & <= 1.00
#   "Top 30-50%"    -> values >= 0.50 & <  0.70
#   "Bottom 50-70%" -> values >= 0.30 & <  0.50
#   "Bottom 70-90%" -> values >= 0.10 & <  0.30
#
# Note: Areas reported in hectares (ha)
#       Cell area = 250m x 250m = 6.25 ha
#
# Author: [Your Name]
# Date: 2025-12-08
################################################################################

library(terra)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

rankmap_path   <- "Z:/BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/rankmap.tif"
wind_excl_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters/combined_wind.tif"
pv_excl_path   <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters/combined_pv.tif"
output_path    <- "Z:/BESP_data_qld_2025/Energy_system_model_outputs/BV_exclusion_area_overlap.csv"

# Cell area in hectares (250m x 250m = 62,500 m2 = 6.25 ha)
CELL_AREA_HA <- (250 * 250) / 10000

# Zonation band definitions (on 0-1 scale, rounded to 2dp)
# Lower bound inclusive (>=), upper bound exclusive (<)
# except top band which is inclusive on both ends
BANDS <- list(
  "Top 30%"        = c(0.70, 1.00),
  "Top 30-50%"     = c(0.50, 0.70),
  "Bottom 50-70%"  = c(0.30, 0.50),
  "Bottom 70-90%"  = c(0.10, 0.30)
)

# ------------------------------------------------------------------------------
# Step 1: Load and check rasters
# ------------------------------------------------------------------------------

load_and_check_rasters <- function(rankmap_path, wind_excl_path, pv_excl_path) {
  
  cat("\n--- Loading rasters ---\n")
  
  rankmap   <- terra::rast(rankmap_path)
  wind_excl <- terra::rast(wind_excl_path)
  pv_excl   <- terra::rast(pv_excl_path)
  
  # Print key properties for each
  rasters <- list(rankmap = rankmap, wind = wind_excl, pv = pv_excl)
  for (name in names(rasters)) {
    r <- rasters[[name]]
    cat(sprintf("\n  %s:\n", name))
    cat(sprintf("    Dimensions : %d rows x %d cols\n", nrow(r), ncol(r)))
    cat(sprintf("    Resolution : %.1f x %.1f m\n", res(r)[1], res(r)[2]))
    cat(sprintf("    CRS        : %s\n", terra::crs(r, describe = TRUE)$code))
    cat(sprintf("    Extent     : xmin=%.0f xmax=%.0f ymin=%.0f ymax=%.0f\n",
                ext(r)$xmin, ext(r)$xmax, ext(r)$ymin, ext(r)$ymax))
    cat(sprintf("    Value range: %.4f - %.4f\n",
                terra::global(r, "min", na.rm = TRUE)$min,
                terra::global(r, "max", na.rm = TRUE)$max))
  }
  
  # ── CRS check ───────────────────────────────────────────────────────────────
  cat("\n--- Checking alignment ---\n")
  
  crs_match_wind <- terra::same.crs(rankmap, wind_excl)
  crs_match_pv   <- terra::same.crs(rankmap, pv_excl)
  cat(sprintf("  CRS match (rankmap vs wind): %s\n", crs_match_wind))
  cat(sprintf("  CRS match (rankmap vs PV):   %s\n", crs_match_pv))
  
  if (!crs_match_wind || !crs_match_pv) {
    stop("CRS mismatch detected — reproject exclusion rasters to EPSG:3577 before proceeding")
  }
  
  # ── Resolution check ────────────────────────────────────────────────────────
  res_match_wind <- all(res(rankmap) == res(wind_excl))
  res_match_pv   <- all(res(rankmap) == res(pv_excl))
  cat(sprintf("  Resolution match (rankmap vs wind): %s\n", res_match_wind))
  cat(sprintf("  Resolution match (rankmap vs PV):   %s\n", res_match_pv))
  
  # ── Extent check ────────────────────────────────────────────────────────────
  ext_match_wind <- all.equal(as.vector(ext(rankmap)), as.vector(ext(wind_excl)))
  ext_match_pv   <- all.equal(as.vector(ext(rankmap)), as.vector(ext(pv_excl)))
  cat(sprintf("  Extent match (rankmap vs wind): %s\n", isTRUE(ext_match_wind)))
  cat(sprintf("  Extent match (rankmap vs PV):   %s\n", isTRUE(ext_match_pv)))
  
  # ── Resample if resolution or extent differs ─────────────────────────────────
  # Uses nearest neighbour (method = "near") to preserve 0/1 values exactly
  if (!res_match_wind || !isTRUE(ext_match_wind)) {
    cat("  Resampling wind exclusion raster to match rankmap grid...\n")
    wind_excl <- terra::resample(wind_excl, rankmap, method = "near")
  }
  if (!res_match_pv || !isTRUE(ext_match_pv)) {
    cat("  Resampling PV exclusion raster to match rankmap grid...\n")
    pv_excl <- terra::resample(pv_excl, rankmap, method = "near")
  }
  
  cat("\n  All checks passed\n")
  return(list(rankmap = rankmap, wind = wind_excl, pv = pv_excl))
}

# ------------------------------------------------------------------------------
# Step 2: Read all raster values into memory and compute band statistics
#
# Loading all three rasters as plain R vectors and doing band arithmetic
# in memory is the fastest pure-R approach — terra::global() re-reads
# from disk on every call, whereas vectors allow all 4 bands x 3 layers
# to be computed in a single pass through memory.
# ------------------------------------------------------------------------------

calculate_band_areas <- function(rasters, bands, cell_area_ha) {
  
  cat("\n--- Reading raster values into memory (one pass each) ---\n")
  
  rankmap_vals <- terra::values(rasters$rankmap, mat = FALSE)
  wind_vals    <- terra::values(rasters$wind,    mat = FALSE)
  pv_vals      <- terra::values(rasters$pv,      mat = FALSE)
  
  # Round rankmap to 2dp — Zonation outputs 14-15 significant figures so
  # boundary values like 0.29999999 and 0.30000001 need rounding to land
  # in the correct band
  cat("  Rounding rankmap values to 2 decimal places...\n")
  rankmap_vals <- round(rankmap_vals, digits = 2)
  
  # NA mask from rankmap — only count cells where rankmap is valid
  valid <- !is.na(rankmap_vals)
  cat(sprintf("  Valid (non-NA) rankmap cells: %d\n", sum(valid)))
  cat(sprintf("  Total area covered: %.1f ha\n\n", sum(valid) * cell_area_ha))
  
  results <- list()
  
  for (band_label in names(bands)) {
    
    lo <- bands[[band_label]][1]
    hi <- bands[[band_label]][2]
    is_top_band <- hi == 1.00
    
    cat(sprintf("  Band %s (%.2f - %.2f):\n", band_label, lo, hi))
    
    # Band mask — lower inclusive, upper exclusive except top band
    if (is_top_band) {
      band_mask <- valid & (rankmap_vals >= lo) & (rankmap_vals <= hi)
    } else {
      band_mask <- valid & (rankmap_vals >= lo) & (rankmap_vals < hi)
    }
    
    total_cells   <- sum(band_mask)
    total_area_ha <- total_cells * cell_area_ha
    cat(sprintf("    Total area:     %.1f ha (%d cells)\n", total_area_ha, total_cells))
    
    # Wind — count cells where band is TRUE and exclusion raster is 1
    wind_excl_cells   <- sum(band_mask & !is.na(wind_vals) & wind_vals == 1)
    wind_excl_area_ha <- wind_excl_cells * cell_area_ha
    wind_avail_ha     <- total_area_ha - wind_excl_area_ha
    wind_excl_pct     <- ifelse(total_area_ha > 0, wind_excl_area_ha / total_area_ha, NA)
    cat(sprintf("    Wind exclusion: %.1f ha (%.1f%%)\n", wind_excl_area_ha, wind_excl_pct * 100))
    cat(sprintf("    Wind available: %.1f ha\n", wind_avail_ha))
    
    # PV — same pattern
    pv_excl_cells   <- sum(band_mask & !is.na(pv_vals) & pv_vals == 1)
    pv_excl_area_ha <- pv_excl_cells * cell_area_ha
    pv_avail_ha     <- total_area_ha - pv_excl_area_ha
    pv_excl_pct     <- ifelse(total_area_ha > 0, pv_excl_area_ha / total_area_ha, NA)
    cat(sprintf("    PV exclusion:   %.1f ha (%.1f%%)\n", pv_excl_area_ha, pv_excl_pct * 100))
    cat(sprintf("    PV available:   %.1f ha\n\n", pv_avail_ha))
    
    results[[band_label]] <- data.frame(
      threshold              = band_label,
      wind_exclusion         = wind_excl_area_ha,
      wind_available_area    = wind_avail_ha,
      wind_exclusion_percent = wind_excl_pct,
      pv_exclusion           = pv_excl_area_ha,
      pv_available_area      = pv_avail_ha,
      pv_exclusion_percent   = pv_excl_pct,
      stringsAsFactors       = FALSE
    )
  }
  
  output_df <- do.call(rbind, results)
  rownames(output_df) <- NULL
  return(output_df)
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

main <- function() {
  
  cat(strrep("#", 70), "\n")
  cat("# BIODIVERSITY EXCLUSION AREA SUMMARY BY ZONATION BAND\n")
  cat(strrep("#", 70), "\n")
  
  # Validate inputs
  for (p in c(rankmap_path, wind_excl_path, pv_excl_path)) {
    if (!file.exists(p)) stop(paste("File not found:", p))
  }
  cat("All input paths validated\n")
  
  # Load and check rasters
  rasters <- load_and_check_rasters(rankmap_path, wind_excl_path, pv_excl_path)
  
  # Calculate areas
  results_df <- calculate_band_areas(rasters, BANDS, CELL_AREA_HA)
  
  # Print summary
  cat("--- Summary Table ---\n")
  print(results_df)
  
  # Save CSV
  out_dir <- dirname(output_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  write.csv(results_df, output_path, row.names = FALSE)
  cat(sprintf("\nSaved CSV to: %s\n", output_path))
  cat("Done\n")
  
  return(results_df)
}

results <- main()