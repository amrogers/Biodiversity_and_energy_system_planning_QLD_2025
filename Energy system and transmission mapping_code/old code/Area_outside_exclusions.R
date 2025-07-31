library(sf)
library(terra)
# Optional parallel processing libraries
# library(parallel)
# library(foreach)
# library(doParallel)

# Set to TRUE if you want to use parallel processing (may crash on some systems)
use_parallel <- FALSE

# Choose which categories to process:
# "both" - process both pv and wind
# "pv" - process only pv
# "wind" - process only wind
process_categories <- "wind"  # Options: "both", "pv", "wind"

# Choose what to do:
# "extract" - extract rasters from GDB to individual files
# "process" - process existing rasters to shapefiles
# "both" - extract then process
operation_mode <- "process"  # Options: "extract", "process", "both"

# Set up parallel processing (only if requested)
if (use_parallel) {
  if (!require(parallel)) install.packages("parallel")
  if (!require(foreach)) install.packages("foreach") 
  if (!require(doParallel)) install.packages("doParallel")
  
  library(parallel)
  library(foreach)
  library(doParallel)
  
  n_cores <- detectCores() - 1  # Use all cores except one
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  cat("Parallel processing enabled with", n_cores, "cores\n")
} else {
  cat("Running in sequential mode\n")
}

# Define the GDB path
gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx1.gdb"

# File paths for layer names
pv_layer_file <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/arcpy_code/pv_layer_names.txt"
wind_layer_file <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/arcpy_code/wind_layer_names.txt"

# Directory for extracted rasters
raster_output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters"

# Directory for final shapefiles
shapefile_output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions"

# Function to extract rasters from GDB to individual files
extract_rasters <- function(category, layer_names, gdb_path, output_dir) {
  
  # Create output directory if it doesn't exist
  category_dir <- file.path(output_dir, category)
  if (!dir.exists(category_dir)) {
    dir.create(category_dir, recursive = TRUE)
    cat("Created directory:", category_dir, "\n")
  }
  
  cat("Extracting", length(layer_names), "rasters for", category, "\n")
  
  extracted_files <- c()
  
  for (i in seq_along(layer_names)) {
    layer_name <- layer_names[i]
    cat("Extracting layer:", layer_name, "\n")
    
    # Read raster from GDB using the correct OpenFileGDB format
    raster_obj <- tryCatch({
      # Use the OpenFileGDB path format for rasters
      gdb_layer_path <- paste0('OpenFileGDB:"', gdb_path, '":"', layer_name, '"')
      terra::rast(gdb_layer_path)
    }, error = function(e) {
      cat("Warning: Could not read", layer_name, "\n")
      cat("Error details:", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(raster_obj)) {
      # Save as GeoTIFF
      output_file <- file.path(category_dir, paste0(layer_name, ".tif"))
      tryCatch({
        terra::writeRaster(raster_obj, output_file, overwrite = TRUE)
        cat("  Saved to:", output_file, "\n")
        extracted_files <- c(extracted_files, output_file)
      }, error = function(e) {
        cat("  Error saving:", e$message, "\n")
      })
    }
  }
  
  cat("Successfully extracted", length(extracted_files), "out of", length(layer_names), "layers for", category, "\n\n")
  return(extracted_files)
}

# Function to process extracted rasters to create combined shapefile
process_extracted_rasters <- function(category, output_dir, shapefile_dir) {
  
  category_dir <- file.path(output_dir, category)
  
  if (!dir.exists(category_dir)) {
    cat("No extracted rasters found for", category, "in", category_dir, "\n")
    return(NULL)
  }
  
  # Get all .tif files for this category
  tif_files <- list.files(category_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    cat("No .tif files found for", category, "\n")
    return(NULL)
  }
  
  cat("Processing", length(tif_files), "extracted rasters for", category, "\n")
  
  # First create combined raster output path
  combined_raster_path <- file.path(output_dir, paste0("combined_", category, ".tif"))
  
  cat("Reading rasters...\n")
  
  # Read all the rasters
  raster_list <- list()
  for (i in seq_along(tif_files)) {
    cat("  Reading raster", i, "of", length(tif_files), ":", basename(tif_files[i]), "\n")
    raster_obj <- tryCatch({
      terra::rast(tif_files[i])
    }, error = function(e) {
      cat("    Warning: Could not read", basename(tif_files[i]), ":", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(raster_obj)) {
      raster_list[[i]] <- raster_obj
    }
  }
  
  # Remove NULL elements
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  
  if (length(raster_list) == 0) {
    cat("No valid rasters found for", category, "\n")
    return(NULL)
  }
  
  cat("Combining", length(raster_list), "rasters for", category, "\n")
  
  # Build combined raster incrementally to handle large datasets
  combined_raster <- raster_list[[1]]
  # Initialize with zeros
  combined_raster[is.na(combined_raster)] <- 0
  
  # Add each raster - any value > 0 becomes 1
  for (i in 2:length(raster_list)) {
    cat("  Adding raster", i, "of", length(raster_list), "\n")
    # Get current raster - replace NA with 0
    current <- raster_list[[i]]
    current[is.na(current)] <- 0
    
    # Update combined - if either has > 0, result is 1
    combined_raster <- (combined_raster > 0) | (current > 0)
    # Convert to integer 0/1
    combined_raster <- terra::ifel(combined_raster, 1, 0)
  }
  
  # Save the combined raster
  cat("Saving combined raster to:", combined_raster_path, "\n")
  terra::writeRaster(combined_raster, combined_raster_path, overwrite = TRUE)
  
  # Convert to polygons (only areas with value 1)
  cat("Converting areas with value > 0 to polygons for", category, "\n")
  
  # Set 0 values to NA before polygonization
  combined_raster[combined_raster == 0] <- NA
  
  polygons <- terra::as.polygons(combined_raster)
  
  # Convert to sf object
  sf_polygons <- sf::st_as_sf(polygons)
  
  # Add category column
  sf_polygons$category <- category
  sf_polygons$value <- 1  # All polygons represent areas with value 1
  
  # Create clean sf dataframe
  sf_polygons <- sf::st_sf(
    category = rep(category, nrow(sf_polygons)),
    value = rep(1, nrow(sf_polygons)), 
    geometry = sf::st_geometry(sf_polygons)
  )
  
  return(sf_polygons)
}

# Read layer names from files
cat("Reading layer names from files...\n")

# Function to read layer names from a text file
read_layer_names <- function(file_path) {
  if (file.exists(file_path)) {
    # Read the file and remove any empty lines or whitespace
    lines <- readLines(file_path, warn = FALSE)
    lines <- trimws(lines)  # Remove leading/trailing whitespace
    lines <- lines[lines != ""]  # Remove empty lines
    # Also remove any extra spaces within the names (like "ext1 _minesBin_pv" -> "ext1_minesBin_pv")
    lines <- gsub("\\s+", "", lines)  # Remove all whitespace within the names
    return(lines)
  } else {
    cat("Warning: File not found:", file_path, "\n")
    return(NULL)
  }
}

# Define parameters for each category with actual layer names
all_categories <- list()

# Read PV layers if file exists
if (process_categories %in% c("both", "pv")) {
  pv_layers <- read_layer_names(pv_layer_file)
  if (!is.null(pv_layers)) {
    all_categories$pv <- list(category = "pv", layers = pv_layers)
    cat("Read", length(pv_layers), "PV layer names\n")
  } else {
    cat("Could not read PV layer names\n")
  }
}

# Read Wind layers if file exists
if (process_categories %in% c("both", "wind")) {
  wind_layers <- read_layer_names(wind_layer_file)
  if (!is.null(wind_layers)) {
    all_categories$wind <- list(category = "wind", layers = wind_layers)
    cat("Read", length(wind_layers), "Wind layer names\n")
  } else {
    cat("Could not read Wind layer names\n")
  }
}

# Select categories to process based on user choice
if (process_categories == "both") {
  categories <- all_categories
  cat("Processing both PV and Wind categories\n")
} else if (process_categories == "pv") {
  if ("pv" %in% names(all_categories)) {
    categories <- list(pv = all_categories$pv)
    cat("Processing PV category only\n")
  } else {
    stop("PV layer file not found or could not be read")
  }
} else if (process_categories == "wind") {
  if ("wind" %in% names(all_categories)) {
    categories <- list(wind = all_categories$wind)
    cat("Processing Wind category only\n")
  } else {
    stop("Wind layer file not found or could not be read")
  }
} else {
  stop("Invalid process_categories value. Use 'both', 'pv', or 'wind'")
}

# Main processing logic based on operation mode
if (operation_mode %in% c("extract", "both")) {
  
  cat("=== EXTRACTING RASTERS FROM GDB ===\n")
  
  # Extract rasters for each category
  for (cat_name in names(categories)) {
    cat_info <- categories[[cat_name]]
    
    if (use_parallel) {
      # Note: Raster extraction is typically I/O bound, so parallel processing may not help much
      cat("Extracting", cat_info$category, "rasters (sequential for better I/O performance)...\n")
      extract_rasters(cat_info$category, cat_info$layers, gdb_path, raster_output_dir)
    } else {
      extract_rasters(cat_info$category, cat_info$layers, gdb_path, raster_output_dir)
    }
  }
  
  cat("=== RASTER EXTRACTION COMPLETE ===\n\n")
}

if (operation_mode %in% c("process", "both")) {
  
  cat("=== PROCESSING EXTRACTED RASTERS TO SHAPEFILES ===\n")
  
  # Process categories (parallel or sequential based on use_parallel setting)
  cat("Starting processing...\n")
  
  if (use_parallel) {
    # Parallel processing
    cat("Using parallel processing...\n")
    results <- foreach(cat_name = names(categories), 
                       .packages = c("sf", "terra")) %dopar% {
                         
                         cat_info <- categories[[cat_name]]
                         process_extracted_rasters(cat_info$category, raster_output_dir, shapefile_output_dir)
                       }
    
    # Stop parallel processing
    stopCluster(cl)
    
  } else {
    # Sequential processing
    cat("Using sequential processing...\n")
    results <- list()
    for (cat_name in names(categories)) {
      cat_info <- categories[[cat_name]]
      results[[cat_name]] <- process_extracted_rasters(cat_info$category, raster_output_dir, shapefile_output_dir)
    }
  }
  
  # Name the results
  names(results) <- names(categories)
  
  # Save shapefiles
  # Create output directory if it doesn't exist
  if (!dir.exists(shapefile_output_dir)) {
    dir.create(shapefile_output_dir, recursive = TRUE)
  }
  
  for (cat_name in names(results)) {
    if (!is.null(results[[cat_name]])) {
      output_path <- file.path(shapefile_output_dir, paste0("combined_", cat_name, ".shp"))
      cat("Saving", cat_name, "shapefile to:", output_path, "\n")
      sf::st_write(results[[cat_name]], output_path, delete_dsn = TRUE)
    }
  }
  
  cat("Processing complete!\n")
  
  # Optional: Create a single combined shapefile with all processed categories
  if (length(results) > 1 && all(sapply(results, function(x) !is.null(x)))) {
    combined_all <- do.call(rbind, results)
    output_path_all <- file.path(shapefile_output_dir, "combined_renewable_energy.shp")
    cat("Saving combined shapefile to:", output_path_all, "\n")
    sf::st_write(combined_all, output_path_all, delete_dsn = TRUE)
  } else if (length(results) == 1 && !is.null(results[[1]])) {
    cat("Only one category processed - combined shapefile not needed\n")
  }
}

# Function to analyze state-by-state areas
analyze_state_areas <- function(shapefile_dir, category) {
  # Check if the ozmaps package is installed
  if (!requireNamespace("ozmaps", quietly = TRUE)) {
    cat("Installing ozmaps package...\n")
    install.packages("ozmaps")
  }
  
  # Load required additional packages
  if (!requireNamespace("units", quietly = TRUE)) install.packages("units")
  
  library(ozmaps)
  library(units)
  
  # Path to the combined shapefile
  shapefile_path <- file.path(shapefile_dir, paste0("combined_", category, ".shp"))
  
  if (!file.exists(shapefile_path)) {
    cat("Shapefile not found for", category, "at", shapefile_path, "\n")
    cat("Please run the vectorize step first\n")
    return(NULL)
  }
  
  cat("=== ANALYZING STATE-BY-STATE AREAS FOR", toupper(category), "===\n")
  
  # Read the shapefile
  cat("Reading shapefile:", shapefile_path, "\n")
  exclusions <- sf::st_read(shapefile_path, quiet = TRUE)
  
  # Get Australia state boundaries from ozmaps
  cat("Getting Australia state boundaries...\n")
  oz_states <- ozmaps::ozmap_states
  
  # Define the Australian Albers Equal Area projection (EPSG:3577)
  # This is the best projection for area calculations in Australia
  cat("Reprojecting to Australian Albers Equal Area projection...\n")
  oz_equal_area_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  # Reproject state boundaries to equal area
  oz_states_equal <- sf::st_transform(oz_states, oz_equal_area_crs)
  
  # Make sure exclusions are also in the same projection
  # First check its current CRS
  current_crs <- sf::st_crs(exclusions)
  cat("Current shapefile CRS:", current_crs$input, "\n")
  
  # Reproject exclusions to equal area
  exclusions_equal <- sf::st_transform(exclusions, oz_equal_area_crs)
  
  # Initialize results table
  cat("Calculating area statistics...\n")
  results <- data.frame(
    state = oz_states_equal$NAME,
    state_area_km2 = NA,
    exclusion_area_km2 = NA,
    available_area_km2 = NA,
    percent_available = NA
  )
  
  # Calculate areas for each state
  for (i in 1:nrow(oz_states_equal)) {
    state_name <- oz_states_equal$NAME[i]
    cat("Processing state:", state_name, "\n")
    
    # Get state geometry
    state_geom <- oz_states_equal[i, ]
    
    # Calculate total state area
    state_area <- sf::st_area(state_geom)
    state_area_km2 <- units::set_units(state_area, "km^2")
    
    # Intersect state with exclusions to get excluded area
    state_exclusions <- tryCatch({
      sf::st_intersection(exclusions_equal, state_geom)
    }, error = function(e) {
      cat("  Warning: Error in intersection:", e$message, "\n")
      # Try with a small buffer to fix potential topology errors
      cat("  Trying with buffer...\n")
      state_buff <- sf::st_buffer(state_geom, dist = 0.1)
      tryCatch({
        sf::st_intersection(exclusions_equal, state_buff)
      }, error = function(e2) {
        cat("  Still failed:", e2$message, "\n")
        return(NULL)
      })
    })
    
    # Calculate excluded area
    if (!is.null(state_exclusions) && nrow(state_exclusions) > 0) {
      exclusion_area <- sum(sf::st_area(state_exclusions))
      exclusion_area_km2 <- units::set_units(exclusion_area, "km^2")
    } else {
      exclusion_area_km2 <- units::set_units(0, "km^2")
    }
    
    # Calculate available area (state minus exclusions)
    available_area_km2 <- state_area_km2 - exclusion_area_km2
    
    # Calculate percentage
    percent_available <- as.numeric(available_area_km2 / state_area_km2 * 100)
    
    # Store results
    results$state_area_km2[i] <- as.numeric(state_area_km2)
    results$exclusion_area_km2[i] <- as.numeric(exclusion_area_km2)
    results$available_area_km2[i] <- as.numeric(available_area_km2)
    results$percent_available[i] <- percent_available
  }
  
  # Print nicely formatted results
  cat("\nResults for", category, ":\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat(sprintf("%-20s %15s %15s %15s %15s\n", 
              "State", "State Area (km²)", "Excluded (km²)", "Available (km²)", "% Available"))
  cat(paste(rep("-", 80), collapse = ""), "\n")
  
  for (i in 1:nrow(results)) {
    cat(sprintf("%-20s %15.2f %15.2f %15.2f %15.1f%%\n", 
                results$state[i], results$state_area_km2[i], results$exclusion_area_km2[i], 
                results$available_area_km2[i], results$percent_available[i]))
  }
  
  # Calculate national totals
  total_state_area <- sum(results$state_area_km2)
  total_exclusion_area <- sum(results$exclusion_area_km2)
  total_available_area <- sum(results$available_area_km2)
  total_percent <- total_available_area / total_state_area * 100
  
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat(sprintf("%-20s %15.2f %15.2f %15.2f %15.1f%%\n", 
              "AUSTRALIA TOTAL", total_state_area, total_exclusion_area, 
              total_available_area, total_percent))
  cat(paste(rep("-", 80), collapse = ""), "\n")
  
  # Save results to CSV
  results_file <- file.path(shapefile_dir, paste0("area_analysis_", category, ".csv"))
  write.csv(results, results_file, row.names = FALSE)
  cat("\nSaved detailed results to:", results_file, "\n")
  
  # Create a summary table with both categories if both were processed
  if (category == "pv" && file.exists(file.path(shapefile_dir, "combined_wind.shp"))) {
    cat("\nBoth PV and Wind shapefiles exist. Consider running this analysis for 'wind' too,\n")
    cat("then the results can be compared side by side.\n")
  } else if (category == "wind" && file.exists(file.path(shapefile_dir, "area_analysis_pv.csv"))) {
    cat("\nCreating combined PV and Wind summary...\n")
    pv_results <- read.csv(file.path(shapefile_dir, "area_analysis_pv.csv"))
    wind_results <- results
    
    # Create combined table
    summary_table <- data.frame(
      State = pv_results$state,
      State_Area_km2 = pv_results$state_area_km2,
      PV_Available_km2 = pv_results$available_area_km2,
      PV_Available_Percent = pv_results$percent_available,
      Wind_Available_km2 = wind_results$available_area_km2,
      Wind_Available_Percent = wind_results$percent_available
    )
    
    # Calculate overlap (available for both)
    cat("Calculating overlap between PV and Wind...\n")
    
    # Read the PV and Wind shapefiles
    pv_file <- file.path(shapefile_dir, "combined_pv.shp")
    wind_file <- file.path(shapefile_dir, "combined_wind.shp")
    
    pv_shape <- sf::st_read(pv_file, quiet = TRUE) 
    wind_shape <- sf::st_read(wind_file, quiet = TRUE)
    
    # Reproject to equal area
    pv_shape_equal <- sf::st_transform(pv_shape, oz_equal_area_crs)
    wind_shape_equal <- sf::st_transform(wind_shape, oz_equal_area_crs)
    
    # Calculate state-by-state overlap
    overlap_areas <- numeric(nrow(oz_states_equal))
    
    for (i in 1:nrow(oz_states_equal)) {
      state_name <- oz_states_equal$NAME[i]
      cat("  Calculating overlap for", state_name, "\n")
      
      state_geom <- oz_states_equal[i, ]
      
      # Get areas within state only
      pv_in_state <- tryCatch({
        sf::st_intersection(pv_shape_equal, state_geom)
      }, error = function(e) {
        cat("    Warning: Error in PV intersection:", e$message, "\n")
        return(NULL)
      })
      
      wind_in_state <- tryCatch({
        sf::st_intersection(wind_shape_equal, state_geom)
      }, error = function(e) {
        cat("    Warning: Error in Wind intersection:", e$message, "\n")
        return(NULL)
      })
      
      # Calculate overlap only if both exist
      if (!is.null(pv_in_state) && !is.null(wind_in_state) && 
          nrow(pv_in_state) > 0 && nrow(wind_in_state) > 0) {
        
        overlap <- tryCatch({
          # Calculate areas NOT in PV (inverse)
          state_no_pv <- sf::st_difference(state_geom, sf::st_union(pv_in_state))
          
          # Calculate areas NOT in Wind (inverse)
          state_no_wind <- sf::st_difference(state_geom, sf::st_union(wind_in_state))
          
          # Union these to get all excluded areas
          all_exclusions <- sf::st_union(state_no_pv, state_no_wind)
          
          # Get areas available for both by finding what's not in the exclusions
          both_available <- sf::st_difference(state_geom, all_exclusions)
          
          # Calculate area
          if (!is.null(both_available)) {
            both_area <- sf::st_area(both_available)
            both_area_km2 <- as.numeric(units::set_units(both_area, "km^2"))
          } else {
            both_area_km2 <- 0
          }
          
          both_area_km2
        }, error = function(e) {
          cat("    Warning: Error calculating overlap:", e$message, "\n")
          return(0)
        })
        
        overlap_areas[i] <- overlap
      } else {
        overlap_areas[i] <- 0
      }
    }
    
    # Add to summary table
    summary_table$Both_Available_km2 <- overlap_areas
    summary_table$Both_Available_Percent <- overlap_areas / summary_table$State_Area_km2 * 100
    
    # Print summary table
    cat("\nCombined Summary (PV and Wind):\n")
    cat(paste(rep("-", 100), collapse = ""), "\n")
    cat(sprintf("%-20s %12s %12s %12s %12s %12s %12s\n", 
                "State", "State (km²)", "PV Avail (km²)", "PV %", 
                "Wind Avail (km²)", "Wind %", "Both %"))
    cat(paste(rep("-", 100), collapse = ""), "\n")
    
    for (i in 1:nrow(summary_table)) {
      cat(sprintf("%-20s %12.2f %12.2f %12.1f%% %12.2f %12.1f%% %12.1f%%\n", 
                  summary_table$State[i], 
                  summary_table$State_Area_km2[i],
                  summary_table$PV_Available_km2[i], 
                  summary_table$PV_Available_Percent[i],
                  summary_table$Wind_Available_km2[i], 
                  summary_table$Wind_Available_Percent[i],
                  summary_table$Both_Available_Percent[i]))
    }
    
    # Calculate national totals
    total_row <- c(
      "AUSTRALIA TOTAL",
      sum(summary_table$State_Area_km2),
      sum(summary_table$PV_Available_km2),
      sum(summary_table$PV_Available_km2) / sum(summary_table$State_Area_km2) * 100,
      sum(summary_table$Wind_Available_km2),
      sum(summary_table$Wind_Available_km2) / sum(summary_table$State_Area_km2) * 100,
      sum(summary_table$Both_Available_km2) / sum(summary_table$State_Area_km2) * 100
    )
    
    cat(paste(rep("-", 100), collapse = ""), "\n")
    cat(sprintf("%-20s %12.2f %12.2f %12.1f%% %12.2f %12.1f%% %12.1f%%\n", 
                total_row[1], as.numeric(total_row[2]), as.numeric(total_row[3]), 
                as.numeric(total_row[4]), as.numeric(total_row[5]), as.numeric(total_row[6]),
                as.numeric(total_row[7])))
    cat(paste(rep("-", 100), collapse = ""), "\n")
    
    # Save combined summary
    summary_file <- file.path(shapefile_dir, "combined_pv_wind_summary.csv")
    write.csv(summary_table, summary_file, row.names = FALSE)
    cat("\nSaved combined summary to:", summary_file, "\n")
  }
  
  cat("\nAnalysis complete!\n")
  return(results)
}library(sf)
library(terra)
# Optional parallel processing libraries
# library(parallel)
# library(foreach)
# library(doParallel)

# Set to TRUE if you want to use parallel processing (may crash on some systems)
use_parallel <- FALSE

# Choose which categories to process:
# "both" - process both pv and wind
# "pv" - process only pv
# "wind" - process only wind
process_categories <- "pv"  # Options: "both", "pv", "wind"

# Choose what to do:
# "extract" - extract rasters from GDB to individual files
# "combine" - combine extracted rasters into a single raster
# "vectorize" - convert combined raster to shapefile
# "analyze" - calculate state-by-state area statistics
# "all" - do all steps in sequence
operation_mode <- "analyze"  # Options: "extract", "combine", "vectorize", "analyze", "all"

# Set up parallel processing (only if requested)
if (use_parallel) {
  if (!require(parallel)) install.packages("parallel")
  if (!require(foreach)) install.packages("foreach") 
  if (!require(doParallel)) install.packages("doParallel")
  
  library(parallel)
  library(foreach)
  library(doParallel)
  
  n_cores <- detectCores() - 1  # Use all cores except one
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  cat("Parallel processing enabled with", n_cores, "cores\n")
} else {
  cat("Running in sequential mode\n")
}

# Define the GDB path
gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx1.gdb"

# File paths for layer names
pv_layer_file <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/arcpy_code/pv_layer_names.txt"
wind_layer_file <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/arcpy_code/wind_layer_names.txt"

# Directory for extracted rasters
raster_output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters"

# Directory for final shapefiles
shapefile_output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions"

# Function to extract rasters from GDB to individual files
extract_rasters <- function(category, layer_names, gdb_path, output_dir) {
  
  # Create output directory if it doesn't exist
  category_dir <- file.path(output_dir, category)
  if (!dir.exists(category_dir)) {
    dir.create(category_dir, recursive = TRUE)
    cat("Created directory:", category_dir, "\n")
  }
  
  cat("Extracting", length(layer_names), "rasters for", category, "\n")
  
  extracted_files <- c()
  
  for (i in seq_along(layer_names)) {
    layer_name <- layer_names[i]
    cat("Extracting layer:", layer_name, "\n")
    
    # Read raster from GDB using the correct OpenFileGDB format
    raster_obj <- tryCatch({
      # Use the OpenFileGDB path format for rasters
      gdb_layer_path <- paste0('OpenFileGDB:"', gdb_path, '":"', layer_name, '"')
      terra::rast(gdb_layer_path)
    }, error = function(e) {
      cat("Warning: Could not read", layer_name, "\n")
      cat("Error details:", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(raster_obj)) {
      # Save as GeoTIFF
      output_file <- file.path(category_dir, paste0(layer_name, ".tif"))
      tryCatch({
        terra::writeRaster(raster_obj, output_file, overwrite = TRUE)
        cat("  Saved to:", output_file, "\n")
        extracted_files <- c(extracted_files, output_file)
      }, error = function(e) {
        cat("  Error saving:", e$message, "\n")
      })
    }
  }
  
  cat("Successfully extracted", length(extracted_files), "out of", length(layer_names), "layers for", category, "\n\n")
  return(extracted_files)
}

# Function to combine extracted rasters into a single raster
combine_rasters <- function(category, output_dir) {
  
  category_dir <- file.path(output_dir, category)
  
  if (!dir.exists(category_dir)) {
    cat("No extracted rasters found for", category, "in", category_dir, "\n")
    return(NULL)
  }
  
  # Get all .tif files for this category
  tif_files <- list.files(category_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    cat("No .tif files found for", category, "\n")
    return(NULL)
  }
  
  cat("Processing", length(tif_files), "extracted rasters for", category, "\n")
  
  # First create combined raster output path
  combined_raster_path <- file.path(output_dir, paste0("combined_", category, ".tif"))
  
  cat("Reading rasters...\n")
  
  # Read all the rasters
  raster_list <- list()
  for (i in seq_along(tif_files)) {
    cat("  Reading raster", i, "of", length(tif_files), ":", basename(tif_files[i]), "\n")
    raster_obj <- tryCatch({
      terra::rast(tif_files[i])
    }, error = function(e) {
      cat("    Warning: Could not read", basename(tif_files[i]), ":", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(raster_obj)) {
      raster_list[[i]] <- raster_obj
    }
  }
  
  # Remove NULL elements
  raster_list <- raster_list[!sapply(raster_list, is.null)]
  
  if (length(raster_list) == 0) {
    cat("No valid rasters found for", category, "\n")
    return(NULL)
  }
  
  cat("Combining", length(raster_list), "rasters for", category, "\n")
  
  # Build combined raster incrementally to handle large datasets
  combined_raster <- raster_list[[1]]
  # Initialize with zeros
  combined_raster[is.na(combined_raster)] <- 0
  
  # Add each raster - any value > 0 becomes 1
  for (i in 2:length(raster_list)) {
    cat("  Adding raster", i, "of", length(raster_list), "\n")
    # Get current raster - replace NA with 0
    current <- raster_list[[i]]
    current[is.na(current)] <- 0
    
    # Update combined - if either has > 0, result is 1
    combined_raster <- (combined_raster > 0) | (current > 0)
    # Convert to integer 0/1
    combined_raster <- terra::ifel(combined_raster, 1, 0)
  }
  
  # Save the combined raster
  cat("Saving combined raster to:", combined_raster_path, "\n")
  terra::writeRaster(combined_raster, combined_raster_path, overwrite = TRUE)
  
  cat("Combined raster saved successfully!\n")
  return(combined_raster_path)
}

##############################################################
# STANDALONE AREA ANALYSIS SCRIPT
# This script analyzes renewable energy exclusion areas by state in Australia
##############################################################

library(sf)
library(units)

# CONFIGURATION - CHANGE THESE VALUES AS NEEDED
# ------------------------------------------------------------
# Path to the directory containing shapefiles
shapefile_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions"

# Categories to analyze 
# Set to: "pv", "wind", or "both"
categories_to_analyze <- "both"
##############################################################

analyze_state_areas <- function(shapefile_dir, category) {
  # Check if the ozmaps package is installed
  if (!requireNamespace("ozmaps", quietly = TRUE)) {
    cat("Installing ozmaps package...\n")
    install.packages("ozmaps")
  }
  
  library(ozmaps)
  
  # Path to the combined shapefile
  shapefile_path <- file.path(shapefile_dir, paste0("combined_", category, ".shp"))
  
  if (!file.exists(shapefile_path)) {
    cat("ERROR: Shapefile not found for", category, "at", shapefile_path, "\n")
    return(NULL)
  }
  
  cat("=== ANALYZING STATE-BY-STATE AREAS FOR", toupper(category), "===\n")
  
  # Read the shapefile
  cat("Reading shapefile:", shapefile_path, "\n")
  exclusions <- sf::st_read(shapefile_path, quiet = TRUE)
  
  # Get Australia state boundaries from ozmaps
  cat("Getting Australia state boundaries...\n")
  oz_states <- ozmaps::ozmap_states
  
  # Define the Australian Albers Equal Area projection (EPSG:3577)
  # This is the best projection for area calculations in Australia
  cat("Reprojecting to Australian Albers Equal Area projection...\n")
  oz_equal_area_crs <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  # Reproject state boundaries to equal area
  oz_states_equal <- sf::st_transform(oz_states, oz_equal_area_crs)
  
  # Make sure exclusions are also in the same projection
  # First check its current CRS
  current_crs <- sf::st_crs(exclusions)
  cat("Current shapefile CRS:", current_crs$input, "\n")
  
  # Reproject exclusions to equal area
  exclusions_equal <- sf::st_transform(exclusions, oz_equal_area_crs)
  
  # Initialize results table
  cat("Calculating area statistics...\n")
  results <- data.frame(
    state = oz_states_equal$NAME,
    state_area_km2 = NA,
    exclusion_area_km2 = NA,
    available_area_km2 = NA,
    percent_available = NA
  )
  
  # Calculate areas for each state
  for (i in 1:nrow(oz_states_equal)) {
    state_name <- oz_states_equal$NAME[i]
    cat("Processing state:", state_name, "\n")
    
    # Get state geometry
    state_geom <- oz_states_equal[i, ]
    
    # Calculate total state area
    state_area <- sf::st_area(state_geom)
    state_area_km2 <- units::set_units(state_area, "km^2")
    
    # Intersect state with exclusions to get excluded area
    state_exclusions <- tryCatch({
      sf::st_intersection(exclusions_equal, state_geom)
    }, error = function(e) {
      cat("  Warning: Error in intersection:", e$message, "\n")
      # Try with a small buffer to fix potential topology errors
      cat("  Trying with buffer...\n")
      state_buff <- sf::st_buffer(state_geom, dist = 0.1)
      tryCatch({
        sf::st_intersection(exclusions_equal, state_buff)
      }, error = function(e2) {
        cat("  Still failed:", e2$message, "\n")
        return(NULL)
      })
    })
    
    # Calculate excluded area
    if (!is.null(state_exclusions) && nrow(state_exclusions) > 0) {
      exclusion_area <- sum(sf::st_area(state_exclusions))
      exclusion_area_km2 <- units::set_units(exclusion_area, "km^2")
    } else {
      exclusion_area_km2 <- units::set_units(0, "km^2")
    }
    
    # Calculate available area (state minus exclusions)
    available_area_km2 <- state_area_km2 - exclusion_area_km2
    
    # Calculate percentage
    percent_available <- as.numeric(available_area_km2 / state_area_km2 * 100)
    
    # Store results
    results$state_area_km2[i] <- as.numeric(state_area_km2)
    results$exclusion_area_km2[i] <- as.numeric(exclusion_area_km2)
    results$available_area_km2[i] <- as.numeric(available_area_km2)
    results$percent_available[i] <- percent_available
  }
  
  # Print nicely formatted results
  cat("\nResults for", category, ":\n")
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat(sprintf("%-20s %15s %15s %15s %15s\n", 
              "State", "State Area (km²)", "Excluded (km²)", "Available (km²)", "% Available"))
  cat(paste(rep("-", 80), collapse = ""), "\n")
  
  for (i in 1:nrow(results)) {
    cat(sprintf("%-20s %15.2f %15.2f %15.2f %15.1f%%\n", 
                results$state[i], results$state_area_km2[i], results$exclusion_area_km2[i], 
                results$available_area_km2[i], results$percent_available[i]))
  }
  
  # Calculate national totals
  total_state_area <- sum(results$state_area_km2)
  total_exclusion_area <- sum(results$exclusion_area_km2)
  total_available_area <- sum(results$available_area_km2)
  total_percent <- total_available_area / total_state_area * 100
  
  cat(paste(rep("-", 80), collapse = ""), "\n")
  cat(sprintf("%-20s %15.2f %15.2f %15.2f %15.1f%%\n", 
              "AUSTRALIA TOTAL", total_state_area, total_exclusion_area, 
              total_available_area, total_percent))
  cat(paste(rep("-", 80), collapse = ""), "\n")
  
  # Save results to CSV
  results_file <- file.path(shapefile_dir, paste0("area_analysis_", category, ".csv"))
  write.csv(results, results_file, row.names = FALSE)
  cat("\nSaved detailed results to:", results_file, "\n")
  
  return(results)
}

# Function to create a combined summary if both PV and Wind have been analyzed
create_combined_summary <- function(shapefile_dir) {
  pv_file <- file.path(shapefile_dir, "area_analysis_pv.csv")
  wind_file <- file.path(shapefile_dir, "area_analysis_wind.csv")
  
  if (!file.exists(pv_file) || !file.exists(wind_file)) {
    cat("ERROR: Cannot create combined summary - need both PV and Wind analysis files\n")
    return(NULL)
  }
  
  cat("\nCreating combined PV and Wind summary...\n")
  pv_results <- read.csv(pv_file)
  wind_results <- read.csv(wind_file)
  
  # Create combined table
  summary_table <- data.frame(
    State = pv_results$state,
    State_Area_km2 = pv_results$state_area_km2,
    PV_Available_km2 = pv_results$available_area_km2,
    PV_Available_Percent = pv_results$percent_available,
    Wind_Available_km2 = wind_results$available_area_km2,
    Wind_Available_Percent = wind_results$percent_available
  )
  
  # Calculate overlap (available for both) - would need to read the shapefiles
  # For simplicity in this standalone script, we'll estimate overlap
  # as the minimum of the two availabilities
  
  summary_table$Both_Estimated_km2 <- pmin(summary_table$PV_Available_km2, summary_table$Wind_Available_km2)
  summary_table$Both_Estimated_Percent <- summary_table$Both_Estimated_km2 / summary_table$State_Area_km2 * 100
  
  # Print summary table
  cat("\nCombined Summary (PV and Wind):\n")
  cat(paste(rep("-", 100), collapse = ""), "\n")
  cat(sprintf("%-20s %12s %12s %12s %12s %12s %12s\n", 
              "State", "State (km²)", "PV Avail (km²)", "PV %", 
              "Wind Avail (km²)", "Wind %", "Both Est. %"))
  cat(paste(rep("-", 100), collapse = ""), "\n")
  
  for (i in 1:nrow(summary_table)) {
    cat(sprintf("%-20s %12.2f %12.2f %12.1f%% %12.2f %12.1f%% %12.1f%%\n", 
                summary_table$State[i], 
                summary_table$State_Area_km2[i],
                summary_table$PV_Available_km2[i], 
                summary_table$PV_Available_Percent[i],
                summary_table$Wind_Available_km2[i], 
                summary_table$Wind_Available_Percent[i],
                summary_table$Both_Estimated_Percent[i]))
  }
  
  # Calculate national totals
  total_row <- c(
    "AUSTRALIA TOTAL",
    sum(summary_table$State_Area_km2),
    sum(summary_table$PV_Available_km2),
    sum(summary_table$PV_Available_km2) / sum(summary_table$State_Area_km2) * 100,
    sum(summary_table$Wind_Available_km2),
    sum(summary_table$Wind_Available_km2) / sum(summary_table$State_Area_km2) * 100,
    sum(summary_table$Both_Estimated_km2) / sum(summary_table$State_Area_km2) * 100
  )
  
  cat(paste(rep("-", 100), collapse = ""), "\n")
  cat(sprintf("%-20s %12.2f %12.2f %12.1f%% %12.2f %12.1f%% %12.1f%%\n", 
              total_row[1], as.numeric(total_row[2]), as.numeric(total_row[3]), 
              as.numeric(total_row[4]), as.numeric(total_row[5]), as.numeric(total_row[6]),
              as.numeric(total_row[7])))
  cat(paste(rep("-", 100), collapse = ""), "\n")
  
  # Save combined summary
  summary_file <- file.path(shapefile_dir, "combined_pv_wind_summary.csv")
  write.csv(summary_table, summary_file, row.names = FALSE)
  cat("\nSaved combined summary to:", summary_file, "\n")
  
  return(summary_table)
}

# Run the analysis based on configuration
cat("STARTING AREA ANALYSIS\n")
cat("=====================\n")
cat("Shapefile directory:", shapefile_dir, "\n")
cat("Categories to analyze:", categories_to_analyze, "\n\n")

if (categories_to_analyze %in% c("pv", "both")) {
  pv_results <- analyze_state_areas(shapefile_dir, "pv")
}

if (categories_to_analyze %in% c("wind", "both")) {
  wind_results <- analyze_state_areas(shapefile_dir, "wind")
}

# Create combined summary if we have both
if (categories_to_analyze == "both") {
  combined_results <- create_combined_summary(shapefile_dir)
} else if (categories_to_analyze == "pv" && 
           file.exists(file.path(shapefile_dir, "area_analysis_wind.csv"))) {
  cat("\nFound existing Wind analysis. Creating combined summary...\n")
  combined_results <- create_combined_summary(shapefile_dir)
} else if (categories_to_analyze == "wind" && 
           file.exists(file.path(shapefile_dir, "area_analysis_pv.csv"))) {
  cat("\nFound existing PV analysis. Creating combined summary...\n")
  combined_results <- create_combined_summary(shapefile_dir)
}

cat("\nANALYSIS COMPLETE!\n")

##############################################################
# EAST COAST RENEWABLE ENERGY MAPPING SCRIPT
# Creates maps showing PV and Wind exclusion areas for east coast states
##############################################################

library(sf)
library(ggplot2)
library(dplyr)

# Check and install additional packages if needed
if (!requireNamespace("ozmaps", quietly = TRUE)) {
  install.packages("ozmaps")
}
if (!requireNamespace("viridis", quietly = TRUE)) {
  install.packages("viridis")
}

library(ozmaps)
library(viridis)

# CONFIGURATION - CHANGE THESE VALUES AS NEEDED
# ------------------------------------------------------------
# Path to the directory containing shapefiles
shapefile_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions"

# Output directory for maps
map_output_dir <- file.path(shapefile_dir, "maps")

# Categories to map 
# Set to: "pv", "wind", or "both"
categories_to_map <- "both"

# Map settings
map_width <- 12  # inches
map_height <- 14  # inches
map_dpi <- 300

# East coast states to include
east_coast_states <- c("Queensland", "New South Wales", "Victoria", "Tasmania", "Australian Capital Territory")
##############################################################

# Create map output directory if it doesn't exist
if (!dir.exists(map_output_dir)) {
  dir.create(map_output_dir, recursive = TRUE)
  cat("Created map output directory:", map_output_dir, "\n")
}

create_renewable_map <- function(category, shapefile_dir, output_dir, east_coast_states) {
  
  cat("Creating map for", toupper(category), "\n")
  
  # Check if shapefile exists
  shapefile_path <- file.path(shapefile_dir, paste0("combined_", category, ".shp"))
  if (!file.exists(shapefile_path)) {
    cat("ERROR: Shapefile not found for", category, "at", shapefile_path, "\n")
    return(NULL)
  }
  
  # Read the renewable energy polygons
  cat("Reading", category, "polygons...\n")
  renewable_polygons <- sf::st_read(shapefile_path, quiet = TRUE)
  
  # Get Australia state boundaries
  cat("Getting Australia state boundaries...\n")
  oz_states <- ozmaps::ozmap_states
  
  # Check the structure and fix any issues
  if (!inherits(oz_states, "sf")) {
    oz_states <- sf::st_as_sf(oz_states)
  }
  
  # Filter to east coast states only
  cat("Filtering to east coast states...\n")
  cat("Available state names:", paste(unique(oz_states$NAME), collapse = ", "), "\n")
  
  east_states <- oz_states[oz_states$NAME %in% east_coast_states, ]
  
  cat("Filtered to", nrow(east_states), "east coast states\n")
  
  # Use Australian Albers projection for better display
  cat("Reprojecting to Australian Albers...\n")
  crs_albers <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  east_states_proj <- sf::st_transform(east_states, crs_albers)
  renewable_proj <- sf::st_transform(renewable_polygons, crs_albers)
  
  # Clip renewable polygons to east coast states
  cat("Clipping renewable polygons to east coast states...\n")
  east_coast_boundary <- sf::st_union(east_states_proj)
  
  renewable_clipped <- tryCatch({
    sf::st_intersection(renewable_proj, east_coast_boundary)
  }, error = function(e) {
    cat("Warning: Error in clipping:", e$message, "\n")
    cat("Trying with buffer...\n")
    boundary_buffered <- sf::st_buffer(east_coast_boundary, dist = 100)
    sf::st_intersection(renewable_proj, boundary_buffered)
  })
  
  if (is.null(renewable_clipped) || nrow(renewable_clipped) == 0) {
    cat("ERROR: No renewable polygons found within east coast states\n")
    return(NULL)
  }
  
  cat("Found", nrow(renewable_clipped), "renewable energy polygons in east coast region\n")
  
  # Create the map
  cat("Creating map...\n")
  
  # Set color based on category
  fill_color <- ifelse(category == "pv", "#FFA500", "#4169E1")  # Orange for PV, Blue for Wind
  category_title <- ifelse(category == "pv", "Solar PV", "Wind")
  
  # Create the plot
  p <- ggplot() +
    # Add state boundaries (filled with light gray)
    geom_sf(data = east_states_proj, 
            fill = "gray95", 
            color = "black", 
            size = 0.5) +
    
    # Add renewable energy exclusion areas
    geom_sf(data = renewable_clipped, 
            fill = fill_color, 
            color = NA, 
            alpha = 0.7) +
    
    # Add state boundaries again on top (just outlines)
    geom_sf(data = east_states_proj, 
            fill = NA, 
            color = "black", 
            size = 0.8) +
    
    # Add state labels
    geom_sf_text(data = east_states_proj, 
                 aes(label = NAME), 
                 size = 3.5, 
                 fontface = "bold",
                 color = "black") +
    
    # Customize the map
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5),
      legend.position = "bottom",
      panel.background = element_rect(fill = "lightblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Add titles and labels
    labs(
      title = paste("Renewable Energy Exclusion Areas -", category_title),
      subtitle = "East Coast Australia (QLD, NSW, ACT, VIC, TAS)",
      caption = paste("Excluded areas shown in", ifelse(category == "pv", "orange", "blue"), 
                      "\nAvailable areas shown in gray\nProjection: Australian Albers Equal Area")
    ) +
    
    # Set coordinate system
    coord_sf(crs = crs_albers)
  
  # Save the map
  output_filename <- file.path(output_dir, paste0("east_coast_", category, "_exclusions.png"))
  
  cat("Saving map to:", output_filename, "\n")
  ggsave(
    filename = output_filename,
    plot = p,
    width = map_width,
    height = map_height,
    dpi = map_dpi,
    units = "in"
  )
  
  # Also save as PDF for high-quality printing
  output_pdf <- file.path(output_dir, paste0("east_coast_", category, "_exclusions.pdf"))
  ggsave(
    filename = output_pdf,
    plot = p,
    width = map_width,
    height = map_height,
    units = "in"
  )
  
  cat("Map saved successfully!\n")
  cat("PNG:", output_filename, "\n")
  cat("PDF:", output_pdf, "\n\n")
  
  return(p)
}

# Function to create a combined comparison map
create_comparison_map <- function(shapefile_dir, output_dir, east_coast_states) {
  
  cat("Creating PV vs Wind comparison map...\n")
  
  # Check if both shapefiles exist
  pv_path <- file.path(shapefile_dir, "combined_pv.shp")
  wind_path <- file.path(shapefile_dir, "combined_wind.shp")
  
  if (!file.exists(pv_path) || !file.exists(wind_path)) {
    cat("ERROR: Both PV and Wind shapefiles needed for comparison map\n")
    return(NULL)
  }
  
  # Read both polygon sets
  cat("Reading PV and Wind polygons...\n")
  pv_polygons <- sf::st_read(pv_path, quiet = TRUE)
  wind_polygons <- sf::st_read(wind_path, quiet = TRUE)
  
  # Get state boundaries
  oz_states <- ozmaps::ozmap_states
  
  # Check the structure and fix any issues
  if (!inherits(oz_states, "sf")) {
    oz_states <- sf::st_as_sf(oz_states)
  }
  
  east_states <- oz_states[oz_states$NAME %in% east_coast_states, ]
  
  # Project everything
  crs_albers <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  east_states_proj <- sf::st_transform(east_states, crs_albers)
  pv_proj <- sf::st_transform(pv_polygons, crs_albers)
  wind_proj <- sf::st_transform(wind_polygons, crs_albers)
  
  # Clip to east coast
  east_coast_boundary <- sf::st_union(east_states_proj)
  
  pv_clipped <- sf::st_intersection(pv_proj, east_coast_boundary)
  wind_clipped <- sf::st_intersection(wind_proj, east_coast_boundary)
  
  # Add category labels
  pv_clipped$tech <- "Solar PV"
  wind_clipped$tech <- "Wind"
  
  # Combine for plotting
  combined_polygons <- rbind(
    pv_clipped[, c("tech", "geometry")],
    wind_clipped[, c("tech", "geometry")]
  )
  
  # Create comparison map
  p_comparison <- ggplot() +
    # Add state boundaries
    geom_sf(data = east_states_proj, 
            fill = "gray95", 
            color = "black", 
            size = 0.5) +
    
    # Add renewable polygons with different colors
    geom_sf(data = combined_polygons, 
            aes(fill = tech), 
            color = NA, 
            alpha = 0.6) +
    
    # Add state boundaries on top
    geom_sf(data = east_states_proj, 
            fill = NA, 
            color = "black", 
            size = 0.8) +
    
    # Add state labels
    geom_sf_text(data = east_states_proj, 
                 aes(label = NAME), 
                 size = 3.5, 
                 fontface = "bold",
                 color = "black") +
    
    # Custom colors
    scale_fill_manual(
      values = c("Solar PV" = "#FFA500", "Wind" = "#4169E1"),
      name = "Technology"
    ) +
    
    # Customize theme
    theme_void() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      panel.background = element_rect(fill = "lightblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Add titles
    labs(
      title = "Renewable Energy Exclusion Areas - PV vs Wind Comparison",
      subtitle = "East Coast Australia (QLD, NSW, ACT, VIC, TAS)",
      caption = "Overlapping areas appear darker\nAvailable areas shown in gray\nProjection: Australian Albers Equal Area"
    ) +
    
    coord_sf(crs = crs_albers)
  
  # Save comparison map
  output_comparison <- file.path(output_dir, "east_coast_pv_wind_comparison.png")
  output_comparison_pdf <- file.path(output_dir, "east_coast_pv_wind_comparison.pdf")
  
  ggsave(output_comparison, p_comparison, width = map_width, height = map_height, dpi = map_dpi, units = "in")
  ggsave(output_comparison_pdf, p_comparison, width = map_width, height = map_height, units = "in")
  
  cat("Comparison map saved!\n")
  cat("PNG:", output_comparison, "\n")
  cat("PDF:", output_comparison_pdf, "\n\n")
  
  return(p_comparison)
}

# Main execution
cat("STARTING MAPPING PROCESS\n")
cat("========================\n")
cat("Shapefile directory:", shapefile_dir, "\n")
cat("Map output directory:", map_output_dir, "\n")
cat("Categories to map:", categories_to_map, "\n")
cat("East coast states:", paste(east_coast_states, collapse = ", "), "\n\n")

# Create individual maps
if (categories_to_map %in% c("pv", "both")) {
  pv_map <- create_renewable_map("pv", shapefile_dir, map_output_dir, east_coast_states)
}

if (categories_to_map %in% c("wind", "both")) {
  wind_map <- create_renewable_map("wind", shapefile_dir, map_output_dir, east_coast_states)
}

# Create comparison map if both categories are being mapped
if (categories_to_map == "both") {
  comparison_map <- create_comparison_map(shapefile_dir, map_output_dir, east_coast_states)
}

cat("MAPPING COMPLETE!\n")
cat("Maps saved to:", map_output_dir, "\n")
cat("Files created:\n")

# List created files
if (categories_to_map %in% c("pv", "both")) {
  cat("- east_coast_pv_exclusions.png\n")
  cat("- east_coast_pv_exclusions.pdf\n")
}

if (categories_to_map %in% c("wind", "both")) {
  cat("- east_coast_wind_exclusions.png\n")
  cat("- east_coast_wind_exclusions.pdf\n")
}

if (categories_to_map == "both") {
  cat("- east_coast_pv_wind_comparison.png\n")
  cat("- east_coast_pv_wind_comparison.pdf\n")
}