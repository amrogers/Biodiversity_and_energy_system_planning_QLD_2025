
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, terr, stringr)
library(sf)
library(terra)
library(dplyr)
library(stringr)

# Set input and output file paths
gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
output_gdb <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"


# Function to process layers in a geodatabase
process_gdb_layers <- function(gdb_path) {
  # Check if we can read the geodatabase
  tryCatch({
    # List all layers in the geodatabase
    layers <- st_layers(gdb_path)$name
    
    # Print available drivers
    cat("Available drivers:\n")
    print(st_drivers())
    
    # Filter layers that:
    # - start with "off"
    # - contain "_30_" anywhere in the name
    # - end with "_cpa"
    target_layers <- layers[grep("^off.*_30_.*_cpa$", layers)]
    
    # Print matched layers
    cat("\nMatched layers:\n")
    print(target_layers)
    
    if(length(target_layers) == 0) {
      stop("No matching layers found")
    }
    
    # Process each matching layer
    for (layer_name in target_layers) {
      # Read the layer
      current_layer <- st_read(gdb_path, layer = layer_name)
      
      # Create new layer name by replacing "_30_" with "_10_"
      new_layer_name <- str_replace(layer_name, "_30_", "_10_")
      
      # Try to write using OpenFileGDB driver
      tryCatch({
        st_write(obj = current_layer,
                 dsn = gdb_path,
                 layer = new_layer_name,
                 driver = "OpenFileGDB",
                 append = FALSE)
      }, error = function(e) {
        # If OpenFileGDB fails, try ESRI Shapefile as fallback
        warning("OpenFileGDB driver failed, attempting to write as Shapefile")
        new_shapefile_path <- file.path(dirname(gdb_path), 
                                        paste0(new_layer_name, ".shp"))
        st_write(obj = current_layer,
                 dsn = new_shapefile_path,
                 append = FALSE)
      })
      
      cat(sprintf("Processed layer: %s -> %s\n", layer_name, new_layer_name))
    }
  }, error = function(e) {
    stop(paste("Error processing geodatabase:", e$message))
  })
}

# Usage example (replace with your actual path)
process_gdb_layers(gdb_path)
