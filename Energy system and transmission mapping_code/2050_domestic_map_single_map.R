# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, ggplot2, ozmaps, RColorBrewer)

# Make sure ggspatial is installed and loaded for scale bar and north arrow
if (!require(ggspatial)) {
  install.packages("ggspatial")
  library(ggspatial)
}

# Set paths - update these to match your directory structure
shapefiles_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/domestic_tx1_shapefiles"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs"
output_file <- paste0(output_folder, "/comprehensive_renewables_map.png")

# Path to the existing transmission line shapefile
existing_tx_shapefile <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/QLD_existing_tx.shp"

# Path to the interTX shapefile (direct reference)
interTX_shapefile <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/domestic_tx1_shapefiles/interTX_2050_threshold_0.shp"

# ========== DEFINE THRESHOLD PARAMETERS ==========
# Define all available thresholds
all_thresholds <- c(0, 10, 30, 50, 70, 90)

# SELECT WHICH THRESHOLDS TO INCLUDE
# *** MODIFY THIS LINE TO SELECT DESIRED THRESHOLDS ***
selected_indices <- c(1, 3, 4, 5)  # Example: selecting 0, 30, 50, 70
thresholds <- all_thresholds[selected_indices]

# *** SELECT MAP TYPE ***
# Set to TRUE for two separate maps (one for wind, one for solar)
# Set to FALSE for a single combined map (original behavior)
separate_tech_maps <- TRUE

# *** CUSTOM LABEL SELECTION ***
# Define your custom labels for each threshold in DISPLAY ORDER
ordered_labels <- c(
  "BAU",       # For threshold 0
  "top 30%",   # For threshold 30
  "top 50%",   # For threshold 50
  "top 70%"    # For threshold 70
)

# *** CUSTOM TECHNOLOGY LABELS ***
# Define custom names for technologies (optional)
tech_labels <- c(
  "Wind" = "Wind Energy",
  "Solar PV" = "Solar Power"
)

# Set year
year <- 2050

# *** CUSTOM COLOR SELECTION ***
# Define your custom colors for each technology and threshold IN DISPLAY ORDER

# Wind technology colors - one for each threshold
wind_colors <- c(
  "darkgrey",  # For threshold 0/BAU 
  "#2171B5",   # For threshold 30
  "#6BAED6",   # For threshold 50
  "#C6DBEF"    # For threshold 70
)

# Solar PV technology colors - one for each threshold
solar_colors <- c(
  "darkred",     # For threshold 0/BAU
  "darkorange",  # For threshold 30
  "goldenrod",   # For threshold 50
  "gold"         # For threshold 70
)

# Get Queensland basemap using ozmaps
oz_basemap <- ozmaps::ozmap_states %>% filter(NAME == "Queensland")
oz_basemap <- st_transform(oz_basemap, st_crs(4326))

# Function to read existing transmission line shapefile with projection check
read_existing_tx <- function() {
  if(file.exists(existing_tx_shapefile)) {
    existing_tx <- st_read(existing_tx_shapefile)
    
    # Transform to match basemap CRS
    if (st_crs(existing_tx) != st_crs(oz_basemap)) {
      cat("Transforming existing transmission lines to match basemap projection\n")
      existing_tx <- st_transform(existing_tx, st_crs(oz_basemap))
    }
    
    # Intersect with Queensland boundary to ensure it's within state boundaries
    existing_tx <- st_intersection(existing_tx, oz_basemap)
    
    # Simplify the layer if it's complex
    existing_tx <- st_simplify(existing_tx, preserveTopology = TRUE, dTolerance = 0.01)
    
    return(existing_tx)
  } else {
    cat("Existing transmission line shapefile not found at:", existing_tx_shapefile, "\n")
    return(NULL)
  }
}

# Function to read interTX shapefile with projection check
read_interTX <- function() {
  if(file.exists(interTX_shapefile)) {
    cat("Reading interTX shapefile\n")
    interTX_layer <- st_read(interTX_shapefile)
    
    # Transform to match basemap CRS if needed
    if (st_crs(interTX_layer) != st_crs(oz_basemap)) {
      cat("Transforming interTX layer to match basemap projection\n")
      interTX_layer <- st_transform(interTX_layer, st_crs(oz_basemap))
    }
    
    # Intersect with Queensland boundary to ensure it's within state boundaries
    interTX_layer <- st_intersection(interTX_layer, oz_basemap)
    
    # Simplify the layer if it's complex
    interTX_layer <- st_simplify(interTX_layer, preserveTopology = TRUE, dTolerance = 0.01)
    
    return(interTX_layer)
  } else {
    cat("InterTX shapefile not found at:", interTX_shapefile, "\n")
    return(NULL)
  }
}

# Read existing transmission line shapefile
existing_tx <- read_existing_tx()

# Read interTX shapefile
interTX <- read_interTX()

# Function to read shapefile if it exists
safe_read_shapefile <- function(file_path) {
  if (file.exists(file_path)) {
    return(st_read(file_path))
  } else {
    message("File not found: ", file_path)
    return(NULL)
  }
}

# Initialize empty list to store all layers
all_layers <- list()

# Read shapefiles for each threshold
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  
  # Construct file path
  shapefile_path <- file.path(shapefiles_path, 
                              paste0("combined_renewables_", year, "_threshold_", threshold, ".shp"))
  
  # Read the shapefile
  layer <- safe_read_shapefile(shapefile_path)
  
  if (!is.null(layer)) {
    # Check projection and transform if needed
    if (st_crs(layer) != st_crs(oz_basemap)) {
      cat("Transforming threshold", threshold, "layer to match basemap projection\n")
      layer <- st_transform(layer, st_crs(oz_basemap))
    }
    
    # Add threshold as an attribute
    layer$threshold <- threshold
    all_layers[[i]] <- layer
    cat("Successfully read", shapefile_path, "\n")
  } else {
    cat("Could not read", shapefile_path, "\n")
  }
}

# Combine all layers
combined_layers <- bind_rows(all_layers)

# Make sure the data exists before proceeding
if (is.null(combined_layers) || nrow(combined_layers) == 0) {
  stop("No data could be read from the shapefiles")
}

# Convert threshold to factor for correct ordering
combined_layers$threshold <- factor(combined_layers$threshold, levels = thresholds)

# Standardize technology column
if (!"technology" %in% colnames(combined_layers)) {
  stop("The 'technology' column was not found in the shapefiles")
} else {
  combined_layers$technology <- tolower(combined_layers$technology)
  cat("Technology types found:", paste(unique(combined_layers$technology), collapse=", "), "\n")
  
  # Map technologies to standardized names - combining offshore and onshore wind
  combined_layers$tech_group <- case_when(
    combined_layers$technology %in% c("wind", "onshore", "offshore") ~ "Wind",
    combined_layers$technology %in% c("solar_pv", "solar", "pv") ~ "Solar PV",
    TRUE ~ combined_layers$technology
  )
}

# Create an interaction factor for technology and threshold
combined_layers$tech_threshold <- interaction(combined_layers$tech_group, combined_layers$threshold)

# ========== CREATE COLOR MAPPING ==========
# Initialize color mapping
color_mapping <- c()

# Add Wind colors to mapping
for (i in seq_along(thresholds)) {
  interaction_value <- paste("Wind", thresholds[i], sep = ".")
  color_mapping[interaction_value] <- wind_colors[i]
}

# Add Solar colors to mapping
for (i in seq_along(thresholds)) {
  interaction_value <- paste("Solar PV", thresholds[i], sep = ".")
  color_mapping[interaction_value] <- solar_colors[i]
}

# Add colors for any other technologies that might be present
unique_techs <- unique(combined_layers$tech_group)
other_techs <- unique_techs[!unique_techs %in% c("Wind", "Solar PV")]

if (length(other_techs) > 0) {
  # Default green palette for any other technologies
  other_colors <- brewer.pal(length(thresholds), "Greens")
  
  for (tech in other_techs) {
    for (i in seq_along(thresholds)) {
      interaction_value <- paste(tech, thresholds[i], sep = ".")
      color_mapping[interaction_value] <- other_colors[i]
    }
  }
}

# Function to create map with common styling
create_map_base <- function(data, title_text, subtitle_text, interTX_data = NULL, existing_tx_data = NULL) {
  p <- ggplot() +
    # Add Queensland basemap
    geom_sf(data = oz_basemap, fill = "white", color = "gray10", linewidth = 0.5) +
    geom_sf(data = data, 
            aes(fill = tech_threshold), 
            color = NA,
            alpha = 0.7)
  
  # Add existing transmission line if it exists (dark grey)
  if (!is.null(existing_tx_data) && nrow(existing_tx_data) > 0) {
    p <- p + geom_sf(data = existing_tx_data, 
                     color = "grey40",  # Dark grey 
                     linewidth = 0.5,
                     linetype = "solid")
  }
  
  # Add modeled interTX layer if it exists (lighter grey)
  if (!is.null(interTX_data) && nrow(interTX_data) > 0) {
    p <- p + geom_sf(data = interTX_data, 
                     color = "#808080",  # Light grey
                     linewidth = 0.5,
                     linetype = "dotted")
  }
  
  p <- p +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      # Remove the background grid lines (longitude/latitude)
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Remove axis labels (lat/long details)
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      # Move legend to the right side
      legend.position = "right",
      # Adjust legend appearance for vertical layout
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      # Adjust legend spacing
      legend.key.height = unit(0.8, "lines"),
      legend.key.width = unit(1.2, "lines"),
      # Ensure there's enough room for the legend
      plot.margin = margin(10, 25, 10, 10)
    )
  
  return(p)
}

# Function to extract legend from a ggplot object
get_legend <- function(a_ggplot){
  # Use cowplot package to extract the legend
  if (!require(cowplot)) {
    install.packages("cowplot")
    library(cowplot)
  }
  
  legend <- cowplot::get_legend(
    a_ggplot + 
      theme(
        legend.position = "right",
        legend.box.margin = margin(0, 0, 0, 0)
      )
  )
  
  return(legend)
}

# Function to create a legend directly without using a map
create_standalone_legend <- function(tech_group, tech_name, thresholds, labels, colors) {
  # Create dummy data for the legend
  dummy_data <- data.frame(
    tech_group = rep(tech_group, length(thresholds)),
    threshold = thresholds,
    stringsAsFactors = FALSE
  )
  
  # Create interaction variable
  dummy_data$tech_threshold <- interaction(dummy_data$tech_group, dummy_data$threshold)
  
  # Create breaks and labels
  legend_breaks <- c()
  legend_labels <- c()
  for (i in seq_along(thresholds)) {
    legend_breaks <- c(legend_breaks, paste(tech_group, thresholds[i], sep = "."))
    legend_labels <- c(legend_labels, labels[i])
  }
  
  # Create color mapping for this technology
  color_mapping_tech <- c()
  for (i in seq_along(thresholds)) {
    interaction_value <- paste(tech_group, thresholds[i], sep = ".")
    color_mapping_tech[interaction_value] <- colors[i]
  }
  
  # Create a plot just for the legend
  legend_plot <- ggplot(dummy_data, aes(x = 1, y = 1, fill = tech_threshold)) +
    geom_tile() +
    scale_fill_manual(
      name = tech_name, 
      values = color_mapping_tech,
      breaks = legend_breaks,
      labels = legend_labels,
      guide = guide_legend(
        title.position = "top",
        ncol = 1,
        byrow = TRUE
      )
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.8, "lines"),
      legend.key.width = unit(1.2, "lines")
    )
  
  # Extract just the legend
  return(get_legend(legend_plot))
}

# Selected thresholds string for filenames
selected_thresholds_str <- paste(ordered_labels, collapse = "_")

if (separate_tech_maps) {
  # Make sure patchwork is installed for arranging plots
  if (!require(patchwork)) {
    install.packages("patchwork")
    library(patchwork)
  }
  
  # Create a map for Wind
  wind_data <- combined_layers %>% filter(tech_group == "Wind")
  
  # Create wind map without legend
  wind_map <- create_map_base(
    wind_data,
    NULL, 
    NULL,
    interTX,
    existing_tx
  ) +
    scale_fill_manual(
      values = color_mapping,
      guide = "none"
    )
  
  # Create standalone wind legend
  wind_legend <- create_standalone_legend(
    tech_group = "Wind",
    tech_name = "Wind Energy",
    thresholds = thresholds,
    labels = ordered_labels,
    colors = wind_colors
  )
  
  # Create a map for Solar PV
  solar_data <- combined_layers %>% filter(tech_group == "Solar PV")
  
  # Create solar map without legend
  solar_map <- create_map_base(
    solar_data,
    NULL,
    NULL,
    interTX,
    existing_tx
  ) +
    scale_fill_manual(
      values = color_mapping,
      guide = "none"
    )
  
  # Create standalone solar legend
  solar_legend <- create_standalone_legend(
    tech_group = "Solar PV",
    tech_name = "Solar Power",
    thresholds = thresholds,
    labels = ordered_labels,
    colors = solar_colors
  )
  
  # Combine maps into a single figure
  combined_plots <- wind_map + solar_map + 
    plot_layout(ncol = 2, widths = c(1, 1)) & 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Display the combined maps
  print(combined_plots)
  
} else {
  # Create combined map (original behavior)
  # Create combined legend for all technologies
  
  # Create dummy data for both technologies
  wind_dummy_data <- data.frame(
    tech_group = rep("Wind", length(thresholds)),
    threshold = thresholds,
    stringsAsFactors = FALSE
  )
  
  solar_dummy_data <- data.frame(
    tech_group = rep("Solar PV", length(thresholds)),
    threshold = thresholds,
    stringsAsFactors = FALSE
  )
  
  combined_dummy_data <- rbind(wind_dummy_data, solar_dummy_data)
  combined_dummy_data$tech_threshold <- interaction(combined_dummy_data$tech_group, combined_dummy_data$threshold)
  
  # Create legend breaks and labels
  legend_breaks <- c()
  legend_labels <- c()
  
  # Create breaks and labels for all technologies and thresholds
  for (tech in unique(combined_dummy_data$tech_group)) {
    tech_name <- ifelse(tech %in% names(tech_labels), tech_labels[tech], tech)
    for (i in seq_along(thresholds)) {
      legend_breaks <- c(legend_breaks, paste(tech, thresholds[i], sep = "."))
      legend_labels <- c(legend_labels, paste(tech_name, "-", ordered_labels[i]))
    }
  }
  
  # Create combined map with legend and both transmission line layers
  combined_map <- create_map_base(
    combined_layers,
    NULL,
    NULL,
    interTX,
    existing_tx
  ) +
    scale_fill_manual(
      values = color_mapping,
      guide = "none"
    )
  
  # Create a plot for the combined legend
  combined_legend_plot <- ggplot(combined_dummy_data, aes(x = 1, y = 1, fill = tech_threshold)) +
    geom_tile() +
    scale_fill_manual(
      name = "Renewable Energy", 
      values = color_mapping,
      breaks = legend_breaks,
      labels = legend_labels,
      guide = guide_legend(
        title.position = "top",
        ncol = 1,
        byrow = TRUE
      )
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.8, "lines"),
      legend.key.width = unit(1.2, "lines")
    )
  
  # Extract just the combined legend
  combined_legend <- get_legend(combined_legend_plot)
  
  # Display the map and legend
  print(combined_map)
  print(combined_legend)
}

# Display the legends (if in separate_tech_maps mode)
if (separate_tech_maps) {
  print(wind_legend)
  print(solar_legend)
}

# Save the combined figure (if in separate_tech_maps mode)
if (separate_tech_maps) {
  combined_output_file <- paste0(output_folder, "/renewables_combined_maps", ".png")
  ggsave(combined_output_file, combined_plots, width = 16, height = 10, units = "in", dpi = 600)
  cat("Combined dual map saved to:", combined_output_file, "\n")
  
  # Save individual legends
  wind_legend_file <- paste0(output_folder, "/wind_legend_ordered.png")
  ggsave(wind_legend_file, wind_legend, width = 3, height = 5, units = "in", dpi = 600)
  cat("Wind legend saved to:", wind_legend_file, "\n")
  
  solar_legend_file <- paste0(output_folder, "/solar_legend_ordered.png")
  ggsave(solar_legend_file, solar_legend, width = 3, height = 5, units = "in", dpi = 600)
  cat("Solar legend saved to:", solar_legend_file, "\n")
}