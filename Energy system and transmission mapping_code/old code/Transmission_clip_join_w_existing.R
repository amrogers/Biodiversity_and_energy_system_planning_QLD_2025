rm(list = ls())


library(sf)
library(purrr)
library(stringr)
library(future)
library(furrr)

# Paths
shapefile_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission/TX_domestic_seperate_layers"
existing_tx_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/Electricity_Transmission_Lines.shp"
existing_tx_buff_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/Electricity_Transmission_Lines_1km_buff.shp"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission/tx_model_exisiting_overlap_tables"

# Ensure the output folder exists
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# List of shapefile names
threshold_layers <- list.files(shapefile_folder, pattern = "\\.shp$", full.names = TRUE)

# Read and transform the existing layers
existing_tx <- st_read(existing_tx_path, quiet = TRUE)
existing_tx_buff <- st_read(existing_tx_buff_path, quiet = TRUE)

# Use the CRS of the first model layer to align all layers
reference_layer <- st_read(threshold_layers[1], quiet = TRUE)
reference_crs <- st_crs(reference_layer)

existing_tx <- st_transform(existing_tx, reference_crs)
existing_tx_buff <- st_transform(existing_tx_buff, reference_crs)

# Function to process a single shapefile
process_layer <- function(layer_path) {
  layer_name <- str_remove(basename(layer_path), "\\.shp$")
  message("Processing layer: ", layer_name)
  
  # Extract the threshold value from the layer name
  threshold <- str_extract(layer_name, "t\\d+")
  
  # Read the model layer
  model_tx <- st_read(layer_path, quiet = TRUE)
  
  # Validate and process the model layer
  model_tx <- st_make_valid(model_tx)
  if (nrow(model_tx) == 0) {
    message("Skipping empty layer: ", layer_name)
    return(NULL)
  }
  
  # Clip model_tx with existing_tx_buff
  model_tx_clip <- st_intersection(model_tx, existing_tx_buff)
  model_tx_clip <- st_make_valid(model_tx_clip)
  
  # Create 1 km buffer around model_tx_clip
  model_tx_buff <- st_buffer(model_tx_clip, dist = 1000)
  model_tx_buff <- st_make_valid(model_tx_buff)
  
  # Clip existing_tx with model_tx_buff
  existing_tx_clip <- st_intersection(existing_tx, model_tx_buff)
  existing_tx_clip <- st_make_valid(existing_tx_clip)
  
  if (nrow(existing_tx_clip) == 0) {
    message("No intersection found for layer: ", layer_name)
    return(NULL)
  }
  
  # Perform spatial join between model_tx_clip and existing_tx_clip
  existing_join <- st_join(existing_tx_clip, model_tx_clip, join = st_intersects)
  
  # Add the threshold value as a new column
  existing_join$threshold <- threshold
  
  # Create CSV file name
  csv_name <- paste0("T", str_remove(threshold, "t"), "_existing_join.csv")
  output_path <- file.path(output_folder, csv_name)
  
  # Write the result to a CSV file
  st_drop_geometry(existing_join) %>%
    write.csv(output_path, row.names = FALSE)
  
  message("Processed and saved: ", csv_name)
}

# Parallel processing setup
plan(multisession, workers = parallel::detectCores() - 1)

# Run the process in parallel
future_map(threshold_layers, process_layer)

# Shut down parallel workers
plan(sequential)
