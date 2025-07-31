# Load required libraries
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(sf, dplyr, raster, ozmaps, ggplot2)

# Main function to create LCOE map with technology-specific color gradients
create_lcoe_map <- function(gdb_path, csv_folder, output_path, technology, threshold, tolerance = 1000) {
  # Define technology-specific color gradients
  color_gradients <- list(
    "pv" = colorRampPalette(c(
      "#FFFFD4", "#FED98E", "#FE9929", 
      "#D95F0E", "#993404", "#662506"
    ))(100),
    "wind" = colorRampPalette(c(
      "#F7FBFF", "#DEEBF7", "#C6DBEF", 
      "#9ECAE1", "#6BAED6", "#4292C6", 
      "#2171B5", "#084594"
    ))(100),
    "off" = colorRampPalette(c(
      "#F7FBFF", "#DEEBF7", "#C6DBEF", 
      "#9ECAE1", "#6BAED6", "#4292C6", 
      "#2171B5", "#084594"
    ))(100)
  )
  
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
    warning(sprintf("No matching files found for %s at threshold %d", 
                    technology, threshold))
    return(NULL)
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
  
  # Get color gradient for the specific technology
  custom_colors <- color_gradients[[tolower(technology)]]
  
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
  
  # Save the plot
  output_filename <- file.path(
    output_path, 
    sprintf("LCOE_map_%s_threshold_%d.jpg", tolower(technology), threshold)
  )
  ggsave(
    filename = output_filename, 
    plot = p, 
    width = 10, 
    height = 12, 
    dpi = 300
  )
  
  return(p)
}

# Set paths
gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
csv_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/model_outputs"
output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/levelized_cost_maps"

# Create output directory if it doesn't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Define technologies and thresholds
technologies <- c("pv", "wind", "off")
thresholds <- c(0, 10, 30, 50, 70)

# Iterate through technologies and thresholds
for (tech in technologies) {
  for (thresh in thresholds) {
    tryCatch({
      # Generate and save map
      message(sprintf("Processing %s at threshold %d", tech, thresh))
      map <- create_lcoe_map(
        gdb_path = gdb_path, 
        csv_folder = csv_folder, 
        output_path = output_path, 
        technology = tech, 
        threshold = thresh, 
        tolerance = 1000
      )
      
      # Check if map was successfully created
      if (!is.null(map)) {
        message(sprintf("Map created for %s at threshold %d", tech, thresh))
      }
    }, 
    error = function(e) {
      message(sprintf("Error processing %s at threshold %d: %s", tech, thresh, e$message))
    })
  }
}

# Print completion message
message("LCOE map generation complete.")n <- mean_val - 2 * sd_val
  stretch_max <- mean_val + 2 * sd_val
  
  # Create plot
  p <- ggplot() +
    # Add raster layer
    geom_raster(data = raster_df, 
                aes(x = x, y = y, fill = value)) +
    # Add Queensland boundary
    geom_sf(data = qld_boundary, 
            fill = NA, 
            color = "black", 
            size = 0.5) +
    # Set custom color scale with standard deviation stretch
    scale_fill_gradientn(
      colors = custom_colors,
      limits = c(stretch_min, stretch_max),
      na.value = "transparent",
      name = "LCOE ($/MWh)"
    ) +
    # Add title and theme
    ggtitle(title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      axis.title = element_blank()
    ) +
    coord_sf()
  
  return(p)
}

# Function to save plots
save_maps_with_boundary <- function(results, base_path = "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02") {
  # Construct full output path
  output_dir <- file.path(base_path, "output_maps")
  
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save each plot
  for(name in names(results)) {
    plot <- plot_lcoe_map_with_boundary(
      results[[name]], 
      title = paste("LCOE Map -", name)
    )
    
    # Save as PNG
    output_file <- file.path(output_dir, paste0(name, "_map.png"))
    ggsave(
      output_file,
      plot = plot,
      width = 10,
      height = 12,
      dpi = 300
    )
    cat("Saved map:", output_file, "\n")
    
    # Also save as PDF for vector graphics
    output_file_pdf <- file.path(output_dir, paste0(name, "_map.pdf"))
    ggsave(
      output_file_pdf,
      plot = plot,
      width = 10,
      height = 12,
      device = cairo_pdf
    )
    cat("Saved map (PDF):", output_file_pdf, "\n")
  }
  
  cat("\nCompleted saving all maps.\n")
  cat("Output directory:", normalizePath(output_dir), "\n")
}

# # Run with parallel processing (automatically uses optimal number of cores)
results <- main(gdb_path, csv_folder)

 
# # Or specify number of cores manually
# results <- main(gdb_path, csv_folder, n_cores = 4)
# 
# # Plot results
if(length(results) > 0) {
  plot_lcoe_map(results[[1]], "LCOE Map - Queensland")
}

# Save all results to files
save_raster_maps(results)r_maps(results)