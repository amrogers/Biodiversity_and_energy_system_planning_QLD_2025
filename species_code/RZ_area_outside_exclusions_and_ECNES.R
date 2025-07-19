# Queensland Red Zones Spatial Analysis
# Analysis of red zones outside PV and Wind exclusion areas

# Load required libraries
library(sf)
library(dplyr)
library(units)
library(foreach)
library(doParallel)

# Function to perform spatial analysis
analyze_red_zones <- function(use_parallel = FALSE, n_cores = NULL) {
  
  # Set up parallel processing if requested
  if (use_parallel) {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    cat("Using parallel processing with", n_cores, "cores\n")
  }
  
  # Define file paths
  gdb_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Documents/ArcGIS/Projects/AUS_area_outside_exclusion_zones/AUS_area_outside_exclusion_zones.gdb"
  red_zones_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/Red_Zones/Red_zones_QLD.shp"
  
  # Define Queensland Albers Equal Area projection (EPSG:3577 - Australian Albers)
  # This is the most appropriate equal area projection for Australia/Queensland
  qld_proj <- "EPSG:3577"
  
  cat("Reading spatial data...\n")
  
  # Read the red zones shapefile
  red_zones <- st_read(red_zones_path, quiet = TRUE)
  cat("Red zones loaded:", nrow(red_zones), "features\n")
  
  # Read PV exclusion layer
  pv_exclusion <- st_read(dsn = gdb_path, layer = "PV_ECNES_Merge", quiet = TRUE)
  cat("PV exclusion zones loaded:", nrow(pv_exclusion), "features\n")
  
  # Read Wind exclusion layer  
  wind_exclusion <- st_read(dsn = gdb_path, layer = "WIND_ECNES_Merge", quiet = TRUE)
  cat("Wind exclusion zones loaded:", nrow(wind_exclusion), "features\n")
  
  cat("Reprojecting to Queensland Albers (EPSG:3577)...\n")
  
  # Reproject all layers to Queensland Albers Equal Area
  red_zones <- st_transform(red_zones, crs = qld_proj)
  pv_exclusion <- st_transform(pv_exclusion, crs = qld_proj)
  wind_exclusion <- st_transform(wind_exclusion, crs = qld_proj)
  
  # Calculate original red zones area
  red_zones$area_km2 <- as.numeric(st_area(red_zones)) / 1e6
  total_red_zones_area <- sum(red_zones$area_km2, na.rm = TRUE)
  
  cat("Original red zones total area:", round(total_red_zones_area, 2), "km²\n")
  
  # Function to perform difference operation
  perform_difference <- function(red_zones, exclusion_layer, exclusion_type) {
    cat("Processing", exclusion_type, "exclusions...\n")
    
    # Ensure valid geometries
    red_zones <- st_make_valid(red_zones)
    exclusion_layer <- st_make_valid(exclusion_layer)
    
    # Perform spatial difference (erase overlapping areas)
    red_zones_remaining <- st_difference(red_zones, st_union(exclusion_layer))
    
    # Calculate remaining area
    red_zones_remaining$remaining_area_km2 <- as.numeric(st_area(red_zones_remaining)) / 1e6
    
    return(red_zones_remaining)
  }
  
  # Perform analysis for both PV and Wind exclusions
  if (use_parallel) {
    cat("Running parallel analysis...\n")
    
    # Parallel processing
    results <- foreach(i = 1:2, .packages = c("sf", "dplyr", "units")) %dopar% {
      if (i == 1) {
        perform_difference(red_zones, pv_exclusion, "PV")
      } else {
        perform_difference(red_zones, wind_exclusion, "Wind")
      }
    }
    
    red_zones_after_pv <- results[[1]]
    red_zones_after_wind <- results[[2]]
    
    # Clean up parallel processing
    stopCluster(cl)
    
  } else {
    cat("Running sequential analysis...\n")
    
    # Sequential processing
    red_zones_after_pv <- perform_difference(red_zones, pv_exclusion, "PV")
    red_zones_after_wind <- perform_difference(red_zones, wind_exclusion, "Wind")
  }
  
  # Calculate summary statistics
  pv_remaining_area <- sum(red_zones_after_pv$remaining_area_km2, na.rm = TRUE)
  wind_remaining_area <- sum(red_zones_after_wind$remaining_area_km2, na.rm = TRUE)
  
  pv_excluded_area <- total_red_zones_area - pv_remaining_area
  wind_excluded_area <- total_red_zones_area - wind_remaining_area
  
  pv_percent_remaining <- (pv_remaining_area / total_red_zones_area) * 100
  wind_percent_remaining <- (wind_remaining_area / total_red_zones_area) * 100
  
  # Create results summary
  results_summary <- data.frame(
    Exclusion_Type = c("PV", "Wind"),
    Original_Area_km2 = c(total_red_zones_area, total_red_zones_area),
    Remaining_Area_km2 = c(pv_remaining_area, wind_remaining_area),
    Excluded_Area_km2 = c(pv_excluded_area, wind_excluded_area),
    Percent_Remaining = c(pv_percent_remaining, wind_percent_remaining),
    Percent_Excluded = c(100 - pv_percent_remaining, 100 - wind_percent_remaining)
  )
  
  # Print results
  cat("\n" + rep("=", 60) + "\n")
  cat("ANALYSIS RESULTS\n")
  cat(rep("=", 60) + "\n")
  print(results_summary, row.names = FALSE, digits = 2)
  
  cat("\nDETAILED SUMMARY:\n")
  cat("Original red zones area:", round(total_red_zones_area, 2), "km²\n")
  cat("\nAfter PV exclusions:\n")
  cat("  Remaining area:", round(pv_remaining_area, 2), "km²\n")
  cat("  Excluded area:", round(pv_excluded_area, 2), "km²\n")
  cat("  Percentage remaining:", round(pv_percent_remaining, 1), "%\n")
  
  cat("\nAfter Wind exclusions:\n")
  cat("  Remaining area:", round(wind_remaining_area, 2), "km²\n")
  cat("  Excluded area:", round(wind_excluded_area, 2), "km²\n")
  cat("  Percentage remaining:", round(wind_percent_remaining, 1), "%\n")
  
  # Return results list
  results_list <- list(
    summary = results_summary,
    red_zones_original = red_zones,
    red_zones_after_pv = red_zones_after_pv,
    red_zones_after_wind = red_zones_after_wind,
    total_original_area = total_red_zones_area,
    pv_remaining_area = pv_remaining_area,
    wind_remaining_area = wind_remaining_area
  )
  
  return(results_list)
}

# Function to save results to file
save_results <- function(results, output_dir = "output") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save summary table
  write.csv(results$summary, 
            file.path(output_dir, "red_zones_exclusion_summary.csv"), 
            row.names = FALSE)
  
  # Save spatial results
  st_write(results$red_zones_after_pv, 
           file.path(output_dir, "red_zones_after_pv_exclusion.shp"), 
           delete_dsn = TRUE, quiet = TRUE)
  
  st_write(results$red_zones_after_wind, 
           file.path(output_dir, "red_zones_after_wind_exclusion.shp"), 
           delete_dsn = TRUE, quiet = TRUE)
  
  cat("Results saved to", output_dir, "directory\n")
}

# USAGE EXAMPLES:

# 1. Run sequential analysis (default)
#results <- analyze_red_zones()

# 2. Run with parallel processing
results <- analyze_red_zones(use_parallel = TRUE)

# 3. Run with specific number of cores
results <- analyze_red_zones(use_parallel = TRUE, n_cores = 6)

# 4. Save results after analysis
# results <- analyze_red_zones()
# save_results(results)

# Run the analysis (sequential by default)
cat("Starting Queensland Red Zones Analysis...\n")
cat("Note: Set use_parallel = TRUE to enable parallel processing\n\n")

# Uncomment the line below to run the analysis
# results <- analyze_red_zones()