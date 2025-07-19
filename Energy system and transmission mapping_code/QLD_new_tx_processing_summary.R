# Load required libraries
library(sf)
library(dplyr)
library(stringr)

# Define input and output paths
model_tx_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/QLD_model_tx"
existing_tx_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/Electricity_Transmission_Lines_1km_buff.shp"
output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/QLD_threshold_tx_new"
summaries_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/QLD_threshold_tx_new_summaries"
summary_output_path <- file.path(summaries_path, "QLD_threshold_tx_new_summary.csv")

# Create output directories if they don't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
dir.create(summaries_path, recursive = TRUE, showWarnings = FALSE)

# Define the projected CRS (GDA2020 MGA Zone 55)
projected_crs <- 7855

# Read existing transmission lines buffer and project it
existing_tx <- st_read(existing_tx_path) %>%
  st_transform(crs = projected_crs)
cat("Read and projected existing transmission lines buffer\n")

# Get list of all model transmission files
model_files <- list.files(model_tx_path, 
                          pattern = "transmission_y2050_t\\d+\\.shp$", 
                          full.names = TRUE)
cat("Found", length(model_files), "model files to process\n")

# Create empty list to store summaries
all_summaries <- list()

# Function to process each threshold layer
process_threshold_layer <- function(file_path) {
  # Extract threshold number from filename
  threshold <- str_extract(basename(file_path), "t\\d+") %>%
    str_replace("t", "") %>%
    as.numeric()
  
  cat("\nProcessing threshold", threshold, "\n")
  
  # Read the model layer and project it immediately
  model_layer <- st_read(file_path) %>%
    st_transform(crs = projected_crs)
  cat("Read and projected model layer:", basename(file_path), "\n")
  
  # Erase overlapping areas (both layers are now in the same projected CRS)
  clipped_layer <- st_difference(model_layer, existing_tx)
  cat("Completed spatial difference operation\n")
  
  # Save processed layer
  output_file <- file.path(output_path, paste0("threshold_", threshold, "_tx_new.shp"))
  
  # Force delete existing file if it exists
  if (file.exists(output_file)) {
    file.remove(output_file)
    file.remove(gsub("\\.shp$", "\\.dbf", output_file))
    file.remove(gsub("\\.shp$", "\\.prj", output_file))
    file.remove(gsub("\\.shp$", "\\.shx", output_file))
    cat("Removed existing files\n")
  }
  
  # Save the layer with explicit overwrite
  st_write(clipped_layer, output_file, driver = "ESRI Shapefile", delete_layer = TRUE, quiet = FALSE)
  
  # Verify the file was created
  if (file.exists(output_file)) {
    cat("Successfully saved:", basename(output_file), "\n")
  } else {
    stop("Failed to save file:", basename(output_file))
  }
  
  # Create summary using projected geometry for accurate lengths
  layer_summary <- clipped_layer %>%
    filter(!is.na(kv) & kv != 0) %>%
    mutate(length_km = as.numeric(st_length(geometry) / 1000)) %>%
    group_by(kv) %>%
    summarize(total_length_km = sum(length_km), .groups = "drop") %>%
    mutate(layer_name = paste0("threshold_", threshold)) %>%
    select(layer_name, capacity = kv, length_km = total_length_km) %>%
    st_drop_geometry()
  
  # Save individual summary to summaries folder
  summary_file <- file.path(summaries_path, paste0("threshold_", threshold, "_summary.csv"))
  write.csv(layer_summary, summary_file, row.names = FALSE)
  cat("Saved summary for threshold", threshold, "\n")
  
  return(layer_summary)
}

# Process all threshold layers and store summaries
all_summaries <- lapply(model_files, process_threshold_layer)

# Combine all summaries into a single dataframe
combined_summary <- do.call(rbind, all_summaries)

# Double check that there's no geometry column
combined_summary <- as.data.frame(combined_summary)

# Write combined summary to CSV in summaries folder
write.csv(combined_summary, summary_output_path, row.names = FALSE)

# Print completion message
cat("\nProcessing complete.\n")
cat("Shapefiles saved to:", output_path, "\n")
cat("Summaries saved to:", summaries_path, "\n")

# List the files in both output directories to confirm
cat("\nFiles in shapefile output directory:\n")
list.files(output_path, pattern = "\\.shp$")
cat("\nFiles in summaries directory:\n")
list.files(summaries_path, pattern = "\\.csv$")summarize_clipped_layer <- function(layer_path) {
  layer_name <- tools::file_path_sans_ext(basename(layer_path))
  message(paste("Summarizing clipped layer:", layer_name))
  
  # Load the clipped transmission layer
  layer <- st_read(layer_path) %>%
    st_make_valid()
  
  # Remove invalid geometries if any
  if (any(!st_is_valid(layer))) {
    message("Invalid geometries detected and fixed.")
    layer <- layer[st_is_valid(layer), ]
  }
  
  # Summarize lengths by capacity
  summary <- layer %>%
    filter(!is.na(kv) & kv != 0) %>%
    mutate(length_km = as.numeric(st_length(geometry) / 1000)) %>%
    group_by(kv) %>%
    summarize(total_length_km = sum(length_km), .groups = "drop") %>%
    mutate(layer_name = layer_name) %>%
    select(layer_name, capacity = kv, length_km = total_length_km)
  
  # Drop geometry and save summary to the new summary folder
  summary_clean <- st_drop_geometry(summary)
  write.csv(summary_clean, file.path(clipped_summary_folder, paste0(layer_name, "_summary.csv")), row.names = FALSE)
  
  return(summary_clean)
}

# Process each clipped layer
clipped_summaries <- map_dfr(clipped_layers, summarize_clipped_layer)

# Combine all clipped summaries into one data frame
combined_clipped_summaries <- clipped_summaries %>%
  bind_rows()

# Save the combined summary table for the clipped layers
combined_clipped_summary_output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/combined_clipped_tx_summary.csv"
write.csv(combined_clipped_summaries, combined_clipped_summary_output_path, row.names = FALSE)
