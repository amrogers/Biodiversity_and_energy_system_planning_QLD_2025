# Load required libraries
library(sf)
library(dplyr)
library(stringr)

# Define input and output paths
model_tx_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission/QLD_model_tx"
existing_tx_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/Electricity_Transmission_Lines_1km_buff.shp"
output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission/QLD_threshold_tx_new"
summary_output_path <- file.path(output_path, "QLD_threshold_tx_new_summary.csv")

# Create output directory if it doesn't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Read existing transmission lines buffer
existing_tx <- st_read(existing_tx_path)
cat("Read existing transmission lines buffer\n")

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
  
  # Read the model layer
  model_layer <- st_read(file_path)
  cat("Read model layer:", basename(file_path), "\n")
  
  # Match CRS with existing transmission lines
  if (st_crs(model_layer) != st_crs(existing_tx)) {
    model_layer <- st_transform(model_layer, st_crs(existing_tx))
    cat("Transformed CRS to match existing transmission lines\n")
  }
  
  # Erase overlapping areas
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
  
  # Create summary - explicitly drop geometry
  layer_summary <- clipped_layer %>%
    filter(!is.na(kv) & kv != 0) %>%
    mutate(length_km = as.numeric(st_length(geometry) / 1000)) %>%
    group_by(kv) %>%
    summarize(total_length_km = sum(length_km), .groups = "drop") %>%
    mutate(layer_name = paste0("threshold_", threshold)) %>%
    select(layer_name, capacity = kv, length_km = total_length_km) %>%
    st_drop_geometry()  # Explicitly drop geometry column
  
  # Save individual summary
  summary_file <- file.path(output_path, paste0("threshold_", threshold, "_summary.csv"))
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

# Write combined summary to CSV
write.csv(combined_summary, summary_output_path, row.names = FALSE)

# Print completion message
cat("\nProcessing complete.\n")
cat("Results saved to:", output_path, "\n")
cat("Summary saved to:", summary_output_path, "\n")

# List the files in the output directory to confirm
cat("\nFiles in output directory:\n")
list.files(output_path, pattern = "\\.(shp|csv)$")