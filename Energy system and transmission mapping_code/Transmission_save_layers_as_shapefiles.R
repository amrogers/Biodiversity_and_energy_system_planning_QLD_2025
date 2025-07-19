# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, data.table, progress, parallel, doParallel)

# Function to process a single layer
process_layer <- function(layer_name, input_gpkg, output_dir) {
  tryCatch({
    # Read the layer from GeoPackage
    layer_data <- st_read(input_gpkg, layer = layer_name, quiet = TRUE)
    
    # Create output shapefile name (remove any 'main.' prefix if it exists)
    shapefile_name <- gsub("^main\\.", "", layer_name)
    output_path <- file.path(output_dir, paste0(shapefile_name, ".shp"))
    
    # Save to shapefile
    st_write(layer_data, 
             dsn = output_path,
             delete_dsn = TRUE,  # Overwrite existing files
             quiet = TRUE)
    
    return(list(status = "success", name = shapefile_name))
    
  }, error = function(e) {
    return(list(status = "error", name = layer_name, message = e$message))
  })
}

# Main script
main <- function() {
  # Define these variables INSIDE the function
  input_gpkg <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission/split_transmission_lines2.gpkg"
  output_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission/TX_domestic_seperate_layers"
  
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # List all layers in the GeoPackage
  layers <- st_layers(input_gpkg)$name
  cat("Found", length(layers), "layers in GeoPackage\n")
  
  # Set up parallel processing
  num_cores <- max(1, parallel::detectCores() - 1)  # Leave one core free
  cat("Using", num_cores, "cores for parallel processing\n")
  
  # Create cluster
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  # Export the process_layer function and required packages to each worker
  clusterExport(cl, c("process_layer"))
  
  # Load required packages on each cluster
  clusterEvalQ(cl, {
    library(sf)
    library(dplyr)
  })
  
  # Process layers in parallel using local variables
  cat("Processing layers in parallel...\n")
  results <- parLapply(cl, layers, function(layer) {
    process_layer(layer, input_gpkg, output_dir)
  })
  
  # Stop cluster
  stopCluster(cl)
  
  # Process results
  successful <- sum(sapply(results, function(x) x$status == "success"))
  failed <- sum(sapply(results, function(x) x$status == "error"))
  
  cat("\nProcessing complete:\n")
  cat("Successfully processed:", successful, "layers\n")
  cat("Failed to process:", failed, "layers\n")
  
  # Print errors if any
  errors <- Filter(function(x) x$status == "error", results)
  if (length(errors) > 0) {
    cat("\nErrors encountered:\n")
    for (error in errors) {
      cat(sprintf("Layer %s: %s\n", error$name, error$message))
    }
  }
  
  # Print summary of processed files
  shapefile_list <- list.files(output_dir, pattern = "\\.shp$")
  cat("\nCreated", length(shapefile_list), "shapefiles:\n")
  print(shapefile_list)
}

# Run the main function
main()Run the main function
main()