#rm(list = ls())

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr)

# Set up parallel processing
future::plan(multisession, workers = parallel::detectCores() - 1)

# Set paths
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/map_outputs"

# Set year and threshold
year <- 2060
threshold <- 0 

# Get the list of layers in the GDB
gdb_layers <- st_layers(input_gdb_path)$name

##------------------Functions-------------------------------
# Function to assign color and geometry type to each layer
assign_colors_and_type <- function(layer, layer_name) {
  color <- ifelse(grepl("pv", layer_name), "orange",
                  ifelse(grepl("wind", layer_name), "lightblue", "blue"))
  
  # Determine if the layer is a line or polygon
  geom_type <- st_geometry_type(layer, by_geometry = FALSE)
  is_line <- geom_type %in% c("LINESTRING", "MULTILINESTRING")
  
  layer_data_with_colors[[layer_name]] <<- list(layer = layer, color = color, is_line = is_line)
}

# Function to read layers and select attributes
process_layer <- function(layer_name) {
  layer <- st_read(input_gdb_path, layer = layer_name)
  
  available_cols <- colnames(layer)
  
  selected_cols <- c("geometry")
  
  if (grepl("wind", layer_name) || grepl("pv", layer_name)) {
    if ("areakm" %in% available_cols) selected_cols <- c(selected_cols, "areakm")
    if ("domestic" %in% available_cols) selected_cols <- c(selected_cols, "domestic")
  } else if (grepl("off", layer_name)) {
    selected_cols <- c("geometry")
  }
  
  layer <- layer %>% select(any_of(selected_cols))
  
  return(layer)
}

# Filter layers based on name criteria using regex
filtered_layers <- gdb_layers %>%
  grep(paste0("_", threshold, "_"), ., value = TRUE) %>%  # Updated this line
  grep(paste0(year), ., value = TRUE) %>%
  grep("buff", ., invert = TRUE, value = TRUE) %>%
  grep("_spur", ., invert = TRUE, value = TRUE) %>%
  grep("_sub", ., invert = TRUE, value = TRUE) %>%
  grep("_export", ., invert = TRUE, value = TRUE) %>%
  grep("^off|^pv|^wind|^interTX", ., value = TRUE)

# Get the Queensland basemap using ozmaps
oz_basemap <- ozmaps::ozmap_states %>% filter(NAME == "Queensland")

# Transform basemap to match the layer CRS
oz_basemap <- st_transform(oz_basemap, st_crs(4326))

# Mask each layer to only display within Queensland
mask_layer_to_queensland <- function(layer, layer_name) {
  layer <- st_transform(layer, st_crs(oz_basemap))
  if (grepl("interTX", layer_name)) {
    return(st_intersection(layer, oz_basemap))
  }
  return(layer)
}

# Read and process all filtered layers in parallel
layer_data_list <- furrr::future_map(filtered_layers, function(layer_name) {
  layer <- process_layer(layer_name)
  masked_layer <- mask_layer_to_queensland(layer, layer_name)
  return(masked_layer)
})

# Initialize an empty list to store layer data with colors
layer_data_with_colors <- list()

# Assign colors and geometry types to each processed layer
purrr::walk2(layer_data_list, filtered_layers, assign_colors_and_type)

# Function to create a plot with specific domestic status
create_plot <- function(is_domestic) {
  base_plot <- ggplot() +
    geom_sf(data = oz_basemap, fill = "white", color = "black", linewidth = .5) +
    theme_void()
  
  # Loop through each layer and add to the base plot
  for (layer_name in names(layer_data_with_colors)) {
    layer_data <- layer_data_with_colors[[layer_name]]
    
    if (!is.null(layer_data$layer)) {
      # Always add line layers
      if (layer_data$is_line) {
        if (grepl("^interTX", layer_name)) {
          # Simplify the layer
          layer_data$layer <- st_simplify(layer_data$layer, 
                                          preserveTopology = TRUE, 
                                          dTolerance = 0.01)
          line_width <- 2  # Significantly increased line width
        } else {
          line_width <- 5  # Default line width
        }
        base_plot <- base_plot +
          geom_sf(data = layer_data$layer,
                  color = "grey50",
                  size = line_width,
                  show.legend = FALSE)
      } 
      
      # Add polygon layers with domestic filter
      else if ("domestic" %in% colnames(layer_data$layer)) {
        # Filter layers based on domestic status
        filtered_layer <- layer_data$layer[layer_data$layer$domestic == as.integer(is_domestic), ]
        
        if (nrow(filtered_layer) > 0) {
          base_plot <- base_plot +
            geom_sf(data = filtered_layer,
                    fill = scales::alpha(layer_data$color, 1),
                    color = scales::alpha(layer_data$color, 1),
                    size = 0.005,  # Border thickness
                    show.legend = FALSE)
        }
      } 
      
      # Add other polygon layers without domestic column
      else {
        base_plot <- base_plot +
          geom_sf(data = layer_data$layer,
                  fill = layer_data$color,  # Set the fill color
                  color = layer_data$color,  # Set the outline color to match the fill
                  size = 0.05,  # Border thickness
                  show.legend = FALSE)
      }
    }  
  }
  
  return(base_plot)
}

# Create and save domestic plot
domestic_plot <- create_plot(is_domestic = TRUE)
domestic_plot

##save maps
output_file_path_domestic <- paste0(output_folder, "/","domestic_export_maps", "/",
                                    "domestic_layer_map_", 
                                    threshold, "_", year, ".png")
ggsave(domestic_plot, filename = output_file_path_domestic, width = 10, height = 8, 
       units = "in")
cat("Domestic Map saved to:", output_file_path_domestic, "\n")

# Create and save export plot
export_plot <- create_plot(is_domestic = FALSE)
export_plot

#save map
output_file_path_export <- paste0(output_folder, "/","domestic_export_maps", "/", "export_layer_map_", 
                                  threshold, "_", year, ".png")
ggsave(export_plot, filename = output_file_path_export, width = 10, height = 8, 
       units = "in")
cat("Export Map saved to:", output_file_path_export, "\n")threshold, year, "domestic")
domestic_map <- create_queensland_map(domestic_layers, "domestic")

# Export map
export_layers <- filter_layers(gdb_layers, threshold, year, "export")
export_map <- create_queensland_map(export_layers, "export")

# Save domestic map
domestic_output_path <- paste0(output_folder, "/", "domestic_layer_map_", 
                               threshold, "_", year, ".png")
ggsave(domestic_map, filename = domestic_output_path, width = 10, height = 8, 
       units = "in")
cat("Domestic map saved to:", domestic_output_path, "\n")

# Save export map
export_output_path <- paste0(output_folder, "/", "export_layer_map_", 
                             threshold, "_", year, ".png")
ggsave(export_map, filename = export_output_path, width = 10, height = 8, 
       units = "in")
cat("Export map saved to:", export_output_path, "\n")