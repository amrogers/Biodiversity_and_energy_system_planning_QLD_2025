# Load required packages (ensure these are installed)
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, purrr)

# Set paths (use the same paths as in your original script)
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/map_outputs"

# Define years and thresholds
years <- c(2050) #2030, 2040, 2050
thresholds <- c(0, 10, 30, 50, 70, 90)

# Get the list of layers in the GDB
gdb_layers <- st_layers(input_gdb_path)$name

# Function to calculate area for wind, solar, and off layers
calculate_area_summary <- function(year, threshold) {
  # Filter layers based on year and threshold
  filtered_layers <- gdb_layers %>%
    grep(paste0("_", threshold, "_"), ., value = TRUE) %>%
    grep(paste0(year), ., value = TRUE) %>%
    grep("buff", ., invert = TRUE, value = TRUE) %>%
    grep("_spur", ., invert = TRUE, value = TRUE) %>%
    grep("_sub", ., invert = TRUE, value = TRUE) %>%
    grep("_export", ., invert = TRUE, value = TRUE) %>%
    grep("^pv|^wind|^off", ., value = TRUE)
  
  # Initialize results dataframe
  area_results <- data.frame(
    domestic_export = character(),
    year = integer(),
    threshold = numeric(),
    solar_wind_off = character(),
    total_footprint_area = numeric()
  )
  
  # Process each domestic status (TRUE/FALSE)
  for (is_domestic in c(TRUE, FALSE)) {
    # Read and process all filtered layers
    layer_areas <- lapply(filtered_layers, function(layer_name) {
      # Read the layer
      layer <- st_read(input_gdb_path, layer = layer_name)
      
      # Filter by domestic status if column exists
      if ("domestic" %in% colnames(layer)) {
        layer <- layer[layer$domestic == as.integer(is_domestic), ]
      }
      
      # Check if layer is empty after filtering
      if (nrow(layer) == 0) return(NULL)
      
      # Convert to hectares (assuming areakm exists, otherwise calculate)
      if ("areakm" %in% colnames(layer)) {
        total_area <- sum(layer$areakm, na.rm = TRUE) * 100  # convert km² to hectares
      } else {
        # Calculate area if not pre-calculated
        total_area <- sum(st_area(layer) / 10000, na.rm = TRUE)  # convert m² to hectares
      }
      
      # Determine layer type
      solar_wind_off <- ifelse(grepl("^pv", layer_name), "solar",
                               ifelse(grepl("^wind", layer_name), "wind", "off"))
      
      return(data.frame(
        domestic_export = ifelse(is_domestic, "domestic", "export"),
        year = year,
        threshold = threshold,
        solar_wind_off = solar_wind_off,
        total_footprint_area = total_area
      ))
    })
    
    # Combine results, filtering out NULL entries
    layer_areas <- do.call(rbind, Filter(Negate(is.null), layer_areas))
    
    # Bind to overall results
    area_results <- rbind(area_results, layer_areas)
  }
  
  return(area_results)
}

# Calculate areas for all combinations
# Initialize an empty dataframe to store all results
all_area_results <- data.frame()

# Loop through years and thresholds to calculate areas
for (year in years) {
  for (threshold in thresholds) {
    # Calculate areas for this year and threshold
    current_results <- calculate_area_summary(year, threshold)
    
    # Append to overall results
    all_area_results <- rbind(all_area_results, current_results)
  }
}
all_area_results<-all_area_results[all_area_results$total_footprint_area != 0, ]

# Save results to a CSV file
output_csv_path <- paste0(output_folder, "/area_summary_", paste(years, collapse="_"), ".csv")
write.csv(all_area_results, output_csv_path, row.names = FALSE)

# Print the results and save location
print(all_area_results)
cat("Area summary saved to:", output_csv_path, "\n")