#rm(list = ls())

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr)

# Set up parallel processing
future::plan(multisession, workers = parallel::detectCores() - 1)

# Set paths
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx1.gdb"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/domestic_tx1_shapefiles"

# Create output directory if it doesn't exist
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Define thresholds
thresholds <- c( 30, 50, 70, 90) #0, 10, 30, 50, 70, 90

# Get the list of layers in the GDB
gdb_layers <- st_layers(input_gdb_path)$name

# Function to read and process layer with technology type
process_layer <- function(layer_name) {
  layer <- st_read(input_gdb_path, layer = layer_name)
  
  # Determine technology type
  tech_type <- case_when(
    grepl("^pv", layer_name) ~ "solar_pv",
    grepl("^wind", layer_name) ~ "wind",
    grepl("^off", layer_name) ~ "offshore",
    TRUE ~ "other"
  )
  
  # Filter domestic sites for PV and wind
  if (tech_type %in% c("solar_pv", "wind")) {
    if ("domestic" %in% colnames(layer)) {
      layer <- layer %>% filter(domestic == 1)
      cat(sprintf("Filtered %s layer for domestic sites\n", tech_type))
    } else {
      warning(sprintf("'domestic' field not found in %s layer\n", layer_name))
    }
  }
  
  # Select necessary columns and add technology type
  available_cols <- colnames(layer)
  selected_cols <- c("geometry")
  
  if ("areakm" %in% available_cols) selected_cols <- c(selected_cols, "areakm")
  if ("domestic" %in% available_cols) selected_cols <- c(selected_cols, "domestic")
  
  layer <- layer %>% 
    select(any_of(selected_cols)) %>%
    mutate(
      technology = tech_type,
      layer_name = layer_name  # Keep original layer name for reference
    )
  
  return(layer)
}

# Process each threshold
for (threshold in thresholds) {
  cat(sprintf("\nProcessing threshold: %d\n", threshold))
  
  # Get layers for each technology type with specific pattern matching
  # Pattern: technology_vtx1_threshold_..._2050_cpa
  wind_pattern <- sprintf("^wind.*_%d_.*_2050_cpa$", threshold)
  pv_pattern <- sprintf("^pv.*_%d_.*_2050_cpa$", threshold)
  offshore_pattern <- sprintf("^off.*_%d_.*_2050_cpa$", threshold)
  
  wind_layers <- gdb_layers[grep(wind_pattern, gdb_layers)]
  pv_layers <- gdb_layers[grep(pv_pattern, gdb_layers)]
  offshore_layers <- gdb_layers[grep(offshore_pattern, gdb_layers)]
  
  # Print found layers for debugging
  cat("Found layers:\n")
  cat("Wind:", paste(wind_layers, collapse=", "), "\n")
  cat("PV:", paste(pv_layers, collapse=", "), "\n")
  cat("Offshore:", paste(offshore_layers, collapse=", "), "\n")
  
  # Combine all layer names
  all_layers <- c(wind_layers, pv_layers, offshore_layers)
  
  if (length(all_layers) > 0) {
    # Process all layers and combine them
    combined_layers <- furrr::future_map(all_layers, function(layer_name) {
      cat(sprintf("Processing layer: %s\n", layer_name))
      processed_layer <- process_layer(layer_name)
      if (nrow(processed_layer) > 0) {
        return(processed_layer)
      } else {
        cat(sprintf("Warning: No features remain after filtering in layer %s\n", layer_name))
        return(NULL)
      }
    }) %>%
      compact() %>%  # Remove any NULL entries
      bind_rows()
    
    if (nrow(combined_layers) > 0) {
      # Create output filename
      output_filename <- file.path(output_folder, 
                                   sprintf("combined_renewables_2050_threshold_%d.shp", threshold))
      
      # Save combined layer as shapefile
      st_write(combined_layers, 
               output_filename, 
               driver = "ESRI Shapefile",
               append = FALSE)
      
      cat(sprintf("Saved combined layer for threshold %d to: %s\n", threshold, output_filename))
      cat(sprintf("Total features in combined layer: %d\n", nrow(combined_layers)))
    } else {
      cat(sprintf("No features remain after filtering for threshold %d\n", threshold))
    }
  } else {
    cat(sprintf("No layers found for threshold %d\n", threshold))
  }
}

cat("\nProcess complete. All combined shapefiles have been saved to:", output_folder, "\n")





##------------------------sppecies impact summary------------------------------
#rm(list = ls())
# Load required libraries
library(sf)
library(dplyr)
library(parallel)
library(tictoc)

# Set working directories
model_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/domestic_tx1_shapefiles"
species_shp <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/QLD_100m_SNES/QLD_100m_SNES_likely.shp"
output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_spp_clip_summaries"

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Define the target CRS (Map Grid of Australia Zone 55)
target_crs <- st_crs("EPSG:28355")

# Read species data from shapefile (with error handling)
tryCatch({
  species_data <- st_read(species_shp) %>%
    st_transform(target_crs)
}, error = function(e) {
  stop(paste("Error reading species shapefile:", e$message))
})

# Function to process a single threshold
process_threshold <- function(threshold, model_dir, output_dir, species_data, target_crs) {
  cat(sprintf("\nProcessing threshold %d%%...\n", threshold))
  
  # Read model layer with error handling
  model_file <- file.path(model_dir, paste0("combined_renewables_2050_threshold_", threshold, ".shp"))
  
  tryCatch({
    model_layer <- st_read(model_file) %>%
      st_transform(target_crs)
    
    # Improve performance with spatial indexing
    model_layer <- st_make_valid(model_layer)
    species_data_valid <- st_make_valid(species_data)
    
    # Clip species data with model layer
    cat("Performing spatial intersection...\n")
    clipped_species <- st_intersection(species_data_valid, model_layer)
    
    # Calculate area in square kilometers
    clipped_species$area_km2 <- as.numeric(st_area(clipped_species)) / 1000000
    
    # Perform spatial join with improved efficiency
    cat("Calculating statistics...\n")
    joined_data <- st_join(model_layer, clipped_species, join = st_intersects, left = FALSE)
    
    # Calculate summary statistics
    summary_stats <- joined_data %>%
      st_drop_geometry() %>%  # Remove geometry to speed up grouping
      group_by(CURRENT_NA, THREATENED) %>%
      summarise(
        total_area_km2 = sum(area_km2),
        n_polygons = n(),
        .groups = 'drop'
      ) %>%
      mutate(threshold = threshold)
    
    # Save outputs with error handling
    output_base <- file.path(output_dir, paste0("threshold_", threshold))
    
    tryCatch({
      # Save clipped species layer
      st_write(clipped_species,
               paste0(output_base, "_clipped.shp"),
               delete_layer = TRUE,
               quiet = TRUE)
      
      # Save joined data
      st_write(joined_data,
               paste0(output_base, "_joined.shp"),
               delete_layer = TRUE,
               quiet = TRUE)
      
      # Save threshold-specific summary
      write.csv(summary_stats,
                paste0(output_base, "_summary.csv"),
                row.names = FALSE)
      
    }, error = function(e) {
      warning(paste("Error saving outputs for threshold", threshold, ":", e$message))
    })
    
    return(summary_stats)
    
  }, error = function(e) {
    warning(paste("Error processing threshold", threshold, ":", e$message))
    return(NULL)
  })
}

# Process all thresholds with parallel processing
thresholds <- c(0, 10, 30, 50, 70, 90)

# Start timing
tic("Total processing time")

# Use parallel processing if multiple cores are available
num_cores <- min(length(thresholds), parallel::detectCores() - 1)
if (num_cores > 1) {
  cl <- makeCluster(num_cores)
  on.exit(stopCluster(cl))
  
  # Export required packages to the cluster
  clusterEvalQ(cl, {
    library(sf)
    library(dplyr)
  })
  
  # Export the process_threshold function and other necessary objects to the cluster
  clusterExport(cl, c("process_threshold", "model_dir", "output_dir", 
                      "species_data", "target_crs"))
  
  # Process thresholds in parallel
  all_summaries <- parLapply(cl, thresholds, function(t) {
    process_threshold(t, model_dir, output_dir, species_data, target_crs)
  })
  
  stopCluster(cl)
} else {
  # Sequential processing if parallel processing is not available
  all_summaries <- lapply(thresholds, function(t) {
    process_threshold(t, model_dir, output_dir, species_data, target_crs)
  })
}

# Remove NULL results from failed processes
all_summaries <- all_summaries[!sapply(all_summaries, is.null)]

# Combine all summaries
final_summary <- bind_rows(all_summaries) %>%
  arrange(threshold, CURRENT_NA)

# Save final combined summary
write.csv(final_summary,
          file.path(output_dir, "combined_summary.csv"),
          row.names = FALSE)

# End timing
toc()

cat("\nProcessing complete! Results saved to:", output_dir, "\n")


###----------------------summary stats
# Load required libraries
library(sf)
library(dplyr)
library(tidyr)

# Set working directory where the threshold outputs are stored
output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_spp_clip_summaries"

# Function to read and process a single threshold's joined file
process_threshold_summary <- function(threshold) {
  cat(sprintf("\nProcessing summary for threshold %d%%...\n", threshold))
  
  # Construct file path
  joined_file <- file.path(output_dir, paste0("threshold_", threshold, "_joined.shp"))
  
  tryCatch({
    # Read the joined shapefile
    joined_data <- st_read(joined_file, quiet = TRUE)
    
    # Create summary statistics
    summary_stats <- joined_data %>%
      st_drop_geometry() %>%  # Remove geometry to speed up grouping
      group_by(CURRENT, THREATE, tchnlgy_y) %>%
      summarise(
        total_area_km2 = sum(are_km2),
        n_polygons = n(),
        .groups = 'drop'
      ) %>%
      mutate(threshold = threshold)
    
    return(summary_stats)
    
  }, error = function(e) {
    warning(paste("Error processing threshold", threshold, ":", e$message))
    return(NULL)
  })
}

# Process all thresholds
thresholds <- c(0, 10, 30, 50, 70, 90)
all_summaries <- lapply(thresholds, process_threshold_summary)

# Remove NULL results from failed processes
all_summaries <- all_summaries[!sapply(all_summaries, is.null)]

# Combine all summaries and create final summary
final_summary <- bind_rows(all_summaries) %>%
  arrange(threshold, CURRENT, tchnlgy_y)

# Create a wide format summary by technology
tech_summary <- final_summary %>%
  pivot_wider(
    id_cols = c(CURRENT, THREATE, threshold),
    names_from = tchnlgy_y,
    values_from = c(total_area_km2, n_polygons),
    values_fill = 0
  )

# Save summaries
write.csv(final_summary,
          file.path(output_dir, "combined_summary_by_technology.csv"),
          row.names = FALSE)

write.csv(tech_summary,
          file.path(output_dir, "combined_summary_wide_format.csv"),
          row.names = FALSE)

# Create summary statistics by technology
tech_stats <- final_summary %>%
  group_by(tchnlgy_y, threshold) %>%
  summarise(
    total_area_km2 = sum(total_area_km2),
    n_species = n_distinct(CURRENT),
    n_threatened_species = sum(THREATE == "Y", na.rm = TRUE),
    .groups = 'drop'
  )

write.csv(tech_stats,
          file.path(output_dir, "technology_summary_stats.csv"),
          row.names = FALSE)

cat("\nProcessing complete! Summary files saved to:", output_dir, "\n")
cat("\nCreated files:\n",
    "1. combined_summary_by_technology.csv - Long format with all details\n",
    "2. combined_summary_wide_format.csv - Wide format with technologies as columns\n",
    "3. technology_summary_stats.csv - Overall statistics by technology\n")