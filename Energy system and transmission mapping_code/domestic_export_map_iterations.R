# =============================================================================
# Energy Infrastructure Spatial Analysis and Mapping - Reproducible Version
# =============================================================================
# Instructions:
# 1. Open the .Rproj file for this project.
# 2. Ensure the Figshare data is unzipped into the /data folder.
# =============================================================================
# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr, scales, here)

# Set up parallel processing
future::plan(multisession, workers = max(1, parallel::detectCores() - 1))

# =============================================================================
# Setup and Path Configuration
# =============================================================================

# 1. Define Standardized Paths using here()
# These work regardless of who downloads the code
data_root      <- here("data")
scenarios_dir  <- file.path(data_root, "Energy_system_model_outputs", "Energy_system_analysis_scenarios")
output_root    <- here("figures", "energy_maps")

# Ensure output directory exists
if (!dir.exists(output_root)) dir.create(output_root, recursive = TRUE)

# 2. Automated GDB Check and Unzip Logic
gdb_names <- c(tx1 = "QLD_v202412_eplus_tx1.gdb", tx2 = "QLD_v202412_eplus_tx2.gdb")

for (tx in names(gdb_names)) {
  gdb_path  <- file.path(scenarios_dir, gdb_names[tx])
  zip_path  <- paste0(gdb_path, ".zip")
  
  if (!dir.exists(gdb_path)) {
    if (file.exists(zip_path)) {
      message(">>> Extracting missing GDB: ", gdb_names[tx], ". This may take a moment...")
      unzip(zip_path, exdir = dirname(gdb_path))
    } else {
      stop("Error: Could not find GDB or ZIP at: ", gdb_path)
    }
  }
}

# Define analysis parameters
thresholds <- c(0, 10, 30, 50, 70, 90)
years <- c(2030, 2040, 2050)
transmission_scenarios <- c("tx1", "tx2")

# =============================================================================
# Processing Logic with "Skip-if-Exists"
# =============================================================================

for (tx_scenario in transmission_scenarios) {
  cat(sprintf("\n=== Scenario: %s ===\n", tx_scenario))
  
  input_gdb_path <- file.path(scenarios_dir, gdb_names[tx_scenario])
  gdb_layers     <- st_layers(input_gdb_path)$name
  
  for (threshold in thresholds) {
    # Define the output shapefile path
    shp_output <- file.path(output_root, paste0("shapefiles_", tx_scenario), 
                            sprintf("combined_renewables_2050_threshold_%d.shp", threshold))
    
    # Check if we can skip this processing step
    if (file.exists(shp_output)) {
      cat(sprintf("   - Skipping threshold %d (Shapefile already exists)\n", threshold))
      next
    }
    
    # [Insert your layer processing/process_layer/st_write logic here]
    # ... (Keeping your original 'furrr' and 'st_write' logic)
  }
}

# =============================================================================
# Mapping with Relative Paths
# =============================================================================

# Update the create_plot function to use your new standardized paths
create_plot <- function(tx_scenario, year, threshold, is_domestic = TRUE) {
  
  # Load base map
  qld_boundary <- ozmaps::ozmap_states %>% filter(NAME == "Queensland")
  
  # Construct Path to existing shapefile
  shp_folder <- file.path(output_root, paste0("shapefiles_", tx_scenario))
  shp_file   <- file.path(shp_folder, sprintf("combined_renewables_2050_threshold_%d.shp", threshold))
  
  if (!file.exists(shp_file)) return(NULL)
  
  # Check for PNG before rendering to save time
  map_type <- ifelse(is_domestic, "domestic", "export")
  png_name <- sprintf("%s_layer_map_%d_%d.png", map_type, threshold, year)
  png_path <- file.path(output_root, paste0(map_type, "_maps_", tx_scenario), png_name)
  
  if (file.exists(png_path)) {
    cat("   - Map already exists:", png_name, "\n")
    return(NULL) # Skip rendering
  }
  
  # [Insert your ggplot logic here]
}