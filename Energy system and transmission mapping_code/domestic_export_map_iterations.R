#rm(list = ls())

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr)

# Set up parallel processing
future::plan(multisession, workers = parallel::detectCores() - 1)

# Set paths
input_gdb_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx1.gdb"
output_folder <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/domestic_shapefiles"

# Create output directory if it doesn't exist
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Define thresholds
thresholds <- c(0, 10, 30, 50, 70, 90)

# Get the list of layers in the GDB
gdb_layers <- st_layers(input_gdb_path)$name

# Function to read and process layer with technology type
process_layer <- function(layer_name) {
  layer <- st_read(input_gdb_path, layer = layer_name)
  
  # Determine technology type
  tech_type <- case_when(
    grepl("^pv", layer_name) ~ "solar_pv",
    grepl("^wind", layer_name) ~ "wind",
    grepl("^off", layer_name) ~ "offshore",
    TRUE ~ "other"
  )
  
  # Select necessary columns and add technology type
  available_cols <- colnames(layer)
  selected_cols <- c("geometry")
  
  if ("areakm" %in% available_cols) selected_cols <- c(selected_cols, "areakm")
  if ("domestic" %in% available_cols) selected_cols <- c(selected_cols, "domestic")
  
  layer <- layer %>% 
    select(any_of(selected_cols)) %>%
    mutate(
      technology = tech_type,
      layer_name = layer_name  # Keep original layer name for reference
    )
  
  return(layer)
}

# Process each threshold
for (threshold in thresholds) {
  cat(sprintf("\nProcessing threshold: %d\n", threshold))
  
  # Get layers for each technology type with specific pattern matching
  # Pattern: technology_vtx1_threshold_..._2050_cpa
  wind_pattern <- sprintf("^wind.*_%d_.*_2050_cpa$", threshold)
  pv_pattern <- sprintf("^pv.*_%d_.*_2050_cpa$", threshold)
  offshore_pattern <- sprintf("^off.*_%d_.*_2050_cpa$", threshold)
  
  wind_layers <- gdb_layers[grep(wind_pattern, gdb_layers)]
  pv_layers <- gdb_layers[grep(pv_pattern, gdb_layers)]
  offshore_layers <- gdb_layers[grep(offshore_pattern, gdb_layers)]
  
  # Print found layers for debugging
  cat("Found layers:\n")
  cat("Wind:", paste(wind_layers, collapse=", "), "\n")
  cat("PV:", paste(pv_layers, collapse=", "), "\n")
  cat("Offshore:", paste(offshore_layers, collapse=", "), "\n")
  
  # Combine all layer names
  all_layers <- c(wind_layers, pv_layers, offshore_layers)
  
  if (length(all_layers) > 0) {
    # Process all layers and combine them
    combined_layers <- furrr::future_map(all_layers, function(layer_name) {
      cat(sprintf("Processing layer: %s\n", layer_name))
      process_layer(layer_name)
    }) %>%
      bind_rows()
    
    # Create output filename
    output_filename <- file.path(output_folder, sprintf("combined_renewables_2050_threshold_%d.shp", threshold))
    
    # Save combined layer as shapefile
    st_write(combined_layers, 
             output_filename, 
             driver = "ESRI Shapefile",
             append = FALSE)
    
    cat(sprintf("Saved combined layer for threshold %d to: %s\n", threshold, output_filename))
  } else {
    cat(sprintf("No layers found for threshold %d\n", threshold))
  }
}

cat("\nProcess complete. All combined shapefiles have been saved to:", output_folder, "\n")

##----------------domestic CPA summary --------------------------------
# Load required libraries
library(sf)
library(dplyr)

# Set the working directory to your shapefile folder
setwd("Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/domestic_tx1_shapefiles")

# Get list of shapefile paths
shp_files <- list.files(pattern = "combined_renewables_.*\\.shp$")

# Function to extract threshold from filename
get_threshold <- function(filename) {
  as.numeric(gsub(".*threshold_([0-9]+)\\.shp$", "\\1", filename))
}

# Initialize empty list to store data
all_data <- list()

# Read and process each shapefile
for(file in shp_files) {
  # Read shapefile
  shp <- st_read(file, quiet = TRUE)
  
  # Extract threshold
  threshold <- get_threshold(file)
  
  # Calculate summary statistics
  summary_stats <- shp %>%
    group_by(technology) %>%
    summarise(total_area = sum(areakm, na.rm = TRUE)) %>%
    mutate(threshold = threshold)
  
  # Store in list
  all_data[[length(all_data) + 1]] <- st_drop_geometry(summary_stats)
}

# Combine all summaries
final_summary <- bind_rows(all_data)

# Reorder columns
final_summary <- final_summary %>%
  select(threshold, technology, total_area)

# Write the results to CSV
write.csv(final_summary, 
          "renewable_infrastructure_summary.csv", 
          row.names = FALSE)

# Display the results
print(final_summary)ine_width <- 0.05  # Much thinner line width for interTX and spur lines
        } else {
          line_width <- 0.05  # Default line width for other line layers
        }
        base_plot <- base_plot +
          geom_sf(data = layer_data$layer,
                  color = "grey30",
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
                  size = 0.005,  # Border thickness
                  show.legend = FALSE)
      }
    }  
  }
  
  return(base_plot)
}

# Generate plots for all combinations of years and thresholds
for (year in years) {
  for (threshold in thresholds) {
    # Create and save domestic plot
    domestic_plot <- create_plot(year, threshold, is_domestic = TRUE)
    output_file_path_domestic <- paste0(output_folder, "/", "domestic_maps_",tx_x, "/",
                                        "domestic_layer_map_", 
                                        threshold, "_", year, ".png")
    ggsave(domestic_plot, filename = output_file_path_domestic, width = 15, height = 15, 
           units = "in")
    cat("Domestic Map saved to:", output_file_path_domestic, "\n")
    
    # Create and save export plot
    export_plot <- create_plot(year, threshold, is_domestic = FALSE)
    output_file_path_export <- paste0(output_folder, "/",
                                      "export_maps_",tx_x, "/",
                                      "export_layer_map_",
                                      threshold, "_", year, ".png")
    ggsave(export_plot, filename = output_file_path_export, width = 15, height = 15, 
           units = "in")
    cat("Export Map saved to:", output_file_path_export, "\n")
  }
}
}png")
    ggsave(export_plot, filename = output_file_path_export, width = 10, height = 8, 
           units = "in")
    cat("Export Map saved to:", output_file_path_export, "\n")
  }
}
