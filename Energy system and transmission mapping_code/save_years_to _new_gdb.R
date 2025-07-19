# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr)

# Set up parallel processing
future::plan(multisession, workers = parallel::detectCores() - 1)

# Set paths
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
output_base_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/yearly_outputs"

# Create output folder if it doesn't exist
dir.create(output_base_folder, showWarnings = FALSE)

# Define years and thresholds
years <- c(2030, 2040, 2060)
thresholds <- c(50, 70, 90)

# Get the list of layers in the GDB
gdb_layers <- st_layers(input_gdb_path)$name

# Function to process and save layers
process_and_save_layers <- function(year, threshold, input_gdb_path, output_folder) {
  # Filter layers based on criteria
  filtered_layers <- gdb_layers %>%
    grep(paste0("_", threshold, "_"), ., value = TRUE) %>%
    grep(paste0(year), ., value = TRUE) %>%
    grep("buff", ., invert = TRUE, value = TRUE) %>%
    grep("_spur", ., invert = TRUE, value = TRUE) %>%
    grep("_sub", ., invert = TRUE, value = TRUE) %>%
    grep("_export", ., invert = TRUE, value = TRUE) %>%
    grep("^off|^pv|^wind|^interTX", ., value = TRUE)
  
  # Create output GeoPackage path
  output_gpkg <- file.path(output_folder, paste0("layers_", year, ".gpkg"))
  
  # Read and save each filtered layer
  for(layer_name in filtered_layers) {
    # Read layer
    layer <- st_read(input_gdb_path, layer = layer_name)
    
    # Save to GeoPackage
    st_write(layer, dsn = output_gpkg, layer = layer_name, 
             driver = "GPKG", append = FALSE)
    
    cat(sprintf("Saved layer %s to %s\n", layer_name, output_gpkg))
  }
}

# Process each year
for(year in years) {
  # Create year folder
  year_folder <- file.path(output_base_folder, paste0("year_", year))
  dir.create(year_folder, showWarnings = FALSE)
  
  # Process all thresholds for this year
  for(threshold in thresholds) {
    process_and_save_layers(year, threshold, input_gdb_path, year_folder)
  }
  
  cat(sprintf("Completed processing for year %d\n", year))
}