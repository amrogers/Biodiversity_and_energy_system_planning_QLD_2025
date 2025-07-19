#rm(list = ls())
# Load required libraries
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(sf, dplyr, raster, ozmaps, ggplot2)

#function to create map
create_lcoe_map <- function(gdb_path, csv_folder, output_path, technology, threshold, tolerance = 1000) {
  # Debug: Print input parameters
  cat("Technology:", technology, "\n")
  cat("Threshold:", threshold, "\n")
  
  # Create layer and file patterns based on technology and threshold
  layer_pattern <- sprintf("%s.*_%d.*_B8_.*", tolower(technology), threshold)
  csv_pattern <- sprintf("%s.*_%d.*\\.csv$", tolower(technology), threshold)
  
  # Find matching layer and CSV
  layers <- st_layers(gdb_path)
  
  # Debug: Print all available layers
  cat("Available layers:\n")
  print(layers$name)
  
  matching_layers <- grep(layer_pattern, layers$name, value = TRUE, ignore.case = TRUE)
  
  # Get matching CSV file
  matching_files <- list.files(
    csv_folder, 
    pattern = csv_pattern, 
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Debug: Print matching layers and files
  cat("Matching layers:\n")
  print(matching_layers)
  cat("Matching CSV files:\n")
  print(matching_files)
  
  # Check if files exist
  if (length(matching_layers) == 0 || length(matching_files) == 0) {
    stop(sprintf("No matching files found for %s at threshold %d", 
                 technology, threshold))
  }
  
  # Read data and simplify polygons
  polygon_layer <- st_read(gdb_path, layer = matching_layers[1]) %>%
    st_simplify(dTolerance = tolerance, preserveTopology = TRUE)
  csv_data <- read.csv(matching_files[1])
  
  # Merge data
  merged_data <- polygon_layer %>%
    left_join(csv_data, by = "OIDcom")
  
  # Get Queensland boundary (in its default projection)
  qld_boundary <- ozmap_states %>%
    filter(NAME == "Queensland")
  
  # Transform merged data to match Queensland boundary's CRS
  merged_data_transformed <- merged_data %>%
    st_transform(st_crs(qld_boundary))
  
  # Calculate standard deviation stretch
  mean_val <- mean(merged_data_transformed$LCOE_mwh_2060, na.rm = TRUE)
  sd_val <- sd(merged_data_transformed$LCOE_mwh_2060, na.rm = TRUE)
  stretch_min <- mean_val - 2 * sd_val
  stretch_max <- mean_val + 2 * sd_val
  
  # Create custom color ramp
  custom_colors <- colorRampPalette(c(
    "#FFFFD4", "#FED98E", "#FE9929", 
    "#D95F0E", "#993404", "#662506"
  ))(100)
  
  # Create the plot
  p <- ggplot() +
    geom_sf(data = merged_data_transformed, 
            aes(fill = LCOE_mwh_2060), 
            color = NA) +
    geom_sf(data = qld_boundary, 
            fill = NA, 
            color = "black", 
            size = 0.5) +
    scale_fill_gradientn(
      colors = custom_colors,
      limits = c(stretch_min, stretch_max),
      na.value = "transparent",
      name = "LCOE ($/MWh)"
    ) +
    ggtitle(sprintf("LCOE Map - %s Threshold: %d", toupper(technology), threshold)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      axis.title = element_blank()
    )
  
  return(p)
}


# Example usage with debugging
gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
csv_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/model_outputs"
output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/levelized_cost_maps"

# Try with lowercase 'pv'
map <- create_lcoe_map(gdb_path, csv_folder, output_path, technology = "pv", threshold = 0, tolerance = 5000)
print(map)
ower
map <- create_lcoe_map(gdb_path, csv_folder, output_path, tolerance = 500)

# Faster but less detailed
map <- create_lcoe_map(gdb_path, csv_folder, output_path, tolerance = 2000)

# Save as PDF
ggsave(
  file.path(output_path, "LCOE_map_pv_threshold_0.pdf"),
  plot = map,
  width = 10,
  height = 12,
  device = cairo_pdf
)

# Save as PNG
ggsave(
  file.path(output_path, "LCOE_map_pv_threshold_0.png"),
  plot = map,
  width = 10,
  height = 12,
  dpi = 300
)outputs/QLD_v202410_onshore_tx1_02/model_outputs"
output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/levelized_cost_maps"

#set technology and threshold
tech_type <- "wind"  # can be "pv", "wind", or "off"
threshold <- 0     # can be 0, 10, 30, 50, or 70

# Create a single map (example for PV with threshold 10)
map <- create_single_map(
  gdb_path = gdb_path,
  csv_folder = csv_folder,
  tech_type,  # can be "pv", "wind", or "off"
  threshold     # can be 0, 10, 30, 50, or 70
)

# Display the map
print(map)


#set output file name
output_file_path <- paste0(output_path, "/", "LCOE", "_",
                           tech_type, "_", threshold, ".png")
#output_file_path <- paste0(output_folder, "/", "test2", ".png")
# Save the plot
ggsave(map, filename = output_file_path, width = 10, height = 8, 
       units = "in")


#----------------------------------------#run in parallel---------------------
# Example usage:
# Define the combinations of tech_type and threshold
tech_types <- c("pv") #"pv", "wind", "off"
thresholds <- c(0)  # 0,  10, 30, 50, 70

# Create a list of all combinations
combinations <- expand.grid(tech_type = tech_types, threshold = thresholds)

# Detect the number of cores
num_cores <- detectCores() - 1  # Use all cores except one

# Create a cluster
cl <- makeCluster(num_cores)

# Export required objects to the cluster
clusterExport(cl, c("gdb_path", "csv_folder", "output_path", "create_single_map", 
                    "ozmap_states", "combinations"))  # Export 'combinations'

# Export required objects and packages to the cluster
clusterExport(cl, c("gdb_path", "csv_folder", "output_path", "create_single_map", 
                    "ozmap_states", "combinations")) 
clusterEvalQ(cl, {
  library(sf)      # Load the 'sf' package
  library(dplyr)   # Load the 'dplyr' package 
  library(raster)  # Load the 'raster' package
}) 

# Use parLapply to run the function in parallel
parLapply(cl, 1:nrow(combinations), function(i) {
  create_single_map(
    gdb_path = gdb_path,
    csv_folder = csv_folder,
    tech_type = combinations$tech_type[i],
    threshold = combinations$threshold[i],
    output_path = output_path
  )
})

# Stop the cluster
stopCluster(cl)

