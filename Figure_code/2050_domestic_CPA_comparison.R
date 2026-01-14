# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================
# Purpose: Analyze renewable energy deployment scenarios across transmission
#          scenarios (TX1, TX2) and biodiversity avoidance thresholds.
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  sf, dplyr, tidyr, ggplot2, knitr, scales, viridis, 
  gridExtra, units, lwgeom, ozmaps, here, magick
)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# --- File Paths (Standardized with here) ---
# base_path points to where your domestic_tx1_shapefiles folders live
base_path   <- here("results", "figures", "energy_maps") 
output_path <- here("results", "transmission_scenario_comparison")

# --- Step-Specific Overwrite Flags ---
OVERWRITE_ALL <- FALSE # Master toggle
OVERWRITE_STEP1_SPATIAL_ANALYSIS <- OVERWRITE_ALL
OVERWRITE_STEP2_TECH_OVERLAP     <- OVERWRITE_ALL
OVERWRITE_STEP3_COOCCURRENCE     <- OVERWRITE_ALL
OVERWRITE_STEP4_VISUALIZATION    <- OVERWRITE_ALL

# --- Analysis Parameters ---
TARGET_CRS <- 4326 # WGS 84
MAP_DPI    <- 300  # Balanced for speed and quality

scenarios  <- c("tx1", "tx2")
thresholds <- c(0, 10, 30, 50, 70, 90)
technologies <- c("wind", "solar_pv", "offshore")

# Create output directory
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

log_message <- function(msg, type = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  prefix <- switch(type, "INFO" = "ℹ", "SUCCESS" = "✅", "WARNING" = "⚠", "ERROR" = "❌", "→")
  cat(sprintf("[%s] %s %s\n", timestamp, prefix, msg))
}

make_valid_safe <- function(geom) {
  tryCatch({
    st_make_valid(geom)
  }, error = function(e) {
    log_message(sprintf("Geometry error: %s", e$message), "WARNING")
    return(NULL)
  })
}

# ==============================================================================
# DATA LOADING (Updated for local structure)
# ==============================================================================

read_scenario_shapefile <- function(scenario, threshold, base_path, target_crs) {
  # Logic: Look in figures/energy_maps/shapefiles_tx1/...
  file_path <- file.path(
    base_path,
    sprintf("shapefiles_%s", scenario),
    sprintf("combined_renewables_2050_threshold_%d.shp", threshold)
  )
  
  if (!file.exists(file_path)) {
    log_message(sprintf("File not found: %s", file_path), "WARNING")
    return(NULL)
  }
  
  shp <- st_read(file_path, quiet = TRUE) %>%
    st_transform(target_crs) %>%
    mutate(scenario = scenario, threshold = threshold)
  
  return(shp)
}

# ==============================================================================
# ANALYSIS STEPS (Logic remains as per your requirements)
# ==============================================================================

run_step1_overall_analysis <- function() {
  output_file <- file.path(output_path, "overall_comparison_stats.csv")
  
  if (!OVERWRITE_STEP1_SPATIAL_ANALYSIS && file.exists(output_file)) {
    log_message("Step 1 results exist. Skipping.", "SUCCESS")
    return(read.csv(output_file))
  }
  
  log_message("Running Step 1: Overall Spatial Analysis", "INFO")
  
  # ... [Your existing calculate_overall_overlap logic continues here] ...
}

# [Steps 2, 3, and 4 follow the same pattern: 
# 1. Check overwrite 
# 2. Run logic 
# 3. Save to output_path]

# ==============================================================================
# MAIN WORKFLOW
# ==============================================================================

main <- function() {
  log_message("Starting analysis workflow", "INFO")
  
  # 1. Analysis
  overall_results <- run_step1_overall_analysis()
  # (Include calls to your step 2, 3 functions here)
  
  # 2. Visualization
  # (Include calls to your visualization functions here)
  
  log_message("All comparison outputs saved to results/transmission_scenario_comparison", "SUCCESS")
}

# Execute
main()