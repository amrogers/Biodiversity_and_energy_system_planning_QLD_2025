#rm(list = ls())

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr)

# Set up parallel processing
future::plan(multisession, workers = parallel::detectCores() - 1)

# Set paths
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/map_outputs"

# Define thresholds
years <- c(2060) #2030, 2040, 2050, 2060
thresholds <- c(0, 10, 30, 50, 70, 90) # 0, 10, 30, 50, 70, 90

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
  }
  
  layer <- layer %>% select(any_of(selected_cols))
  
  return(layer)
}

# Function to create and save map
create_map <- function(layer_data_with_colors, oz_basemap, output_file_path, title) {
  base_plot <- ggplot() +
    geom_sf(data = oz_basemap, fill = "white", color = "black", linewidth = 0.5) +
    theme_void() +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  for (layer_name in names(layer_data_with_colors)) {
    layer_data <- layer_data_with_colors[[layer_name]]
    if (!is.null(layer_data$layer)) {
      if (layer_data$is_line) {
        line_width <- 5  # Default line width
        base_plot <- base_plot +
          geom_sf(data = layer_data$layer,
                  color = "grey50",
                  size = line_width,
                  show.legend = FALSE)
      } else {
        if ("domestic" %in% colnames(layer_data$layer)) {
          base_plot <- base_plot +
            geom_sf(data = layer_data$layer,
                    fill = scales::alpha(layer_data$color, 
                                         ifelse(layer_data$layer$domestic == 1, 1, 0.15)),
                    color = scales::alpha(layer_data$color, 
                                          ifelse(layer_data$layer$domestic == 1, 1, 0.15)),
                    size = 0.005,
                    show.legend = FALSE)
        } else {
          base_plot <- base_plot +
            geom_sf(data = layer_data$layer,
                    fill = layer_data$color,
                    color = layer_data$color,
                    size = 0.15,
                    show.legend = FALSE)
        }
      }
    }
  }
  
  ggsave(base_plot, filename = output_file_path, width = 10, height = 8, units = "in")
  cat("Map saved to:", output_file_path, "\n")
}

##----------------set up basemap-------------------------
# Get the Queensland basemap using ozmaps
oz_basemap <- ozmaps::ozmap_states %>% filter(NAME == "Queensland")

# Transform basemap to match the layer CRS
oz_basemap <- st_transform(oz_basemap, st_crs(4326))

##-------------------------------------------------------
# ------------------Iterate over thresholds-----------------------
for (threshold in thresholds) {
  # Process wind layers
  wind_layers <- gdb_layers %>%
    grep(paste0("^wind.*_", threshold, "_.*B8"), ., value = TRUE)
  
  # Process PV layers
  pv_layers <- gdb_layers %>%
    grep(paste0("^pv.*_", threshold, "_.*B8"), ., value = TRUE)
  
  # Print the selected layers for debugging
  cat("Wind layers selected:", paste(wind_layers, collapse=", "), "\n")
  cat("PV layers selected:", paste(pv_layers, collapse=", "), "\n")
  
  # Function to process and mask layers
  mask_layer_to_queensland <- function(layer, layer_name) {
    layer <- st_transform(layer, st_crs(oz_basemap))
    return(layer)
  }
  
  # Process wind layers
  if (length(wind_layers) > 0) {
    layer_data_with_colors <- list()
    wind_data_list <- furrr::future_map(wind_layers, function(layer_name) {
      layer <- process_layer(layer_name)
      masked_layer <- mask_layer_to_queensland(layer, layer_name)
      return(masked_layer)
    })
    purrr::walk2(wind_data_list, wind_layers, assign_colors_and_type)
    wind_output_path <- paste0(output_folder, "/wind_map_B8_", threshold, ".png")
    create_map(layer_data_with_colors, oz_basemap, wind_output_path, 
               paste("Wind Sites - Threshold:", threshold))
  }
  
  # Process PV layers
  if (length(pv_layers) > 0) {
    layer_data_with_colors <- list()
    pv_data_list <- furrr::future_map(pv_layers, function(layer_name) {
      layer <- process_layer(layer_name)
      masked_layer <- mask_layer_to_queensland(layer, layer_name)
      return(masked_layer)
    })
    purrr::walk2(pv_data_list, pv_layers, assign_colors_and_type)
    pv_output_path <- paste0(output_folder, "/pv_map_B8_", threshold, ".png")
    create_map(layer_data_with_colors, oz_basemap, pv_output_path, 
               paste("PV Sites - Threshold:", threshold))
  }
}


##----------------svae lyers to new .gdb
#rm(list = ls())


# Set up parallel processing
future::plan(multisession, workers = parallel::detectCores() - 1)

# Set paths
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb"
output_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/Selected_B8_Layers.gdb"

# Define thresholds
thresholds <- c(0, 10, 30, 50, 70, 90) # 0, 10, 30, 50, 70, 90

# Get the list of layers in the GDB
gdb_layers <- st_layers(input_gdb_path)$name

# Function to process layer
process_layer <- function(layer_name) {
  layer <- st_read(input_gdb_path, layer = layer_name)
  return(layer)
}

# Create a new geodatabase
if (file.exists(output_gdb_path)) {
  cat("Output geodatabase already exists. Proceeding with adding layers...\n")
} else {
  sf::st_write(sf::st_sf(geometry = sf::st_sfc()), 
               dsn = file.path(dirname(output_gdb_path), "output.mdb"), 
               layer = "temp",
               driver = "PGeo")
  sf::st_delete(output_gdb_path, "temp")
  cat("Created new geodatabase:", output_gdb_path, "\n")
}

# Process each threshold
for (threshold in thresholds) {
  # Get wind and PV layers
  wind_layers <- gdb_layers %>%
    grep(paste0("^wind.*_", threshold, "_.*B8"), ., value = TRUE)
  
  pv_layers <- gdb_layers %>%
    grep(paste0("^pv.*_", threshold, "_.*B8"), ., value = TRUE)
  
  # Combine all layers
  all_layers <- c(wind_layers, pv_layers)
  
  # Print the selected layers
  cat("\nProcessing threshold:", threshold, "\n")
  cat("Selected layers:", paste(all_layers, collapse=", "), "\n")
  
  # Process and save each layer
  for (layer_name in all_layers) {
    cat("Processing layer:", layer_name, "\n")
    
    # Read the layer
    layer <- process_layer(layer_name)
    
    # Create a new layer name (you can modify this if you want different naming)
    new_layer_name <- layer_name
    
    # Write to new geodatabase
    tryCatch({
      st_write(layer, 
               dsn = output_gdb_path, 
               layer = new_layer_name,
               driver = "FileGDB",
               append = FALSE)
      cat("Successfully saved layer:", new_layer_name, "\n")
    }, error = function(e) {
      cat("Error saving layer:", new_layer_name, "\n")
      cat("Error message:", e$message, "\n")
    })
  }
}

cat("\nProcess complete. All layers have been saved to:", output_gdb_path, "\n")rror message:", e$message, "\n")
    })
  }
}

cat("\nProcess complete. All layers have been saved to:", output_gdb_path, "\n")