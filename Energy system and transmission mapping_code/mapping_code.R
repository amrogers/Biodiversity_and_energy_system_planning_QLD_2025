
#rm(list = ls())

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr)

# Set up parallel processing
future::plan(multisession, workers = parallel::detectCores() - 1)

# Set paths
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus.gdb"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs"

# Set year and threshold
year <- 2060
threshold <-0 

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


# # Filter layers based on name criteria using regex
filtered_layers <- gdb_layers %>%
  grep(paste0("_", threshold, "_"), ., value = TRUE) %>%  # Updated this line
  grep(paste0(year), ., value = TRUE) %>%
  grep("buff", ., invert = TRUE, value = TRUE) %>%
  grep("_spur", ., invert = TRUE, value = TRUE) %>%
  grep("_sub", ., invert = TRUE, value = TRUE) %>%
  grep("_export", ., invert = TRUE, value = TRUE) %>%
  grep("^off|^pv|^wind|^interTX", ., value = TRUE)

#check line layers
# # # Filter layers based on name criteria using regex
# filtered_layers <- gdb_layers %>%
#   grep(paste0("_", threshold, "_"), ., value = TRUE) %>%  # Updated this line
#   grep(paste0(year), ., value = TRUE) %>%
#   grep("buff", ., invert = TRUE, value = TRUE) %>%
#   grep("_spur", ., invert = TRUE, value = TRUE) %>%
#   grep("_sub", ., invert = TRUE, value = TRUE) %>%
#   grep("_export", ., invert = TRUE, value = TRUE) %>%
#   grep("_sink", ., value = TRUE)
# 

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

# Plot all layers on the same map
base_plot <- ggplot() +
  geom_sf(data = oz_basemap, fill = "white", color = "black", linewidth = .5) +
  theme_void()


# Loop through each layer and add to the base plot
for (layer_name in names(layer_data_with_colors)) {
  layer_data <- layer_data_with_colors[[layer_name]]
  if (!is.null(layer_data$layer)) {
    if (layer_data$is_line) {
      # For line layers: adjust line width based on layer name and simplify if needed
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
                show.legend = FALSE)  # Remove legend for lines
    } else {
      # For polygon layers: fill color matches outline, with alpha adjustment for 'domestic'
      if ("domestic" %in% colnames(layer_data$layer)) {
        base_plot <- base_plot +
          geom_sf(data = layer_data$layer,
                  fill = scales::alpha(layer_data$color, 
                                       ifelse(layer_data$layer$domestic == 1, 1, 0.15)),  # Set fill transparency based on 'domestic'
                  color = scales::alpha(layer_data$color, 
                                        ifelse(layer_data$layer$domestic == 1, 1, 0.15)),  # Set outline transparency to match fill
                  size = 0.005,  # Border thickness
                  show.legend = FALSE)  # Remove legend for polygons
      } else {
        base_plot <- base_plot +
          geom_sf(data = layer_data$layer,
                  fill = layer_data$color,  # Set the fill color
                  color = layer_data$color,  # Set the outline color to match the fill
                  size = 0.05,  # Border thickness
                  show.legend = FALSE)  # Remove legend if no 'domestic' column
      }
    }
  }  
}
base_plot

#set output file name
output_file_path <- paste0(output_folder, "/", "combined_layer_map_", 
                           threshold, "_", year, ".png")
#output_file_path <- paste0(output_folder, "/", "test2", ".png")
# Save the plot
ggsave(base_plot, filename = output_file_path, width = 10, height = 8, 
       units = "in")

# Print a message confirming the export
cat("Map saved to:", output_file_path, "\n")




le_path, "\n")

