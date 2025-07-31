# Load required libraries
library(sf)
library(dplyr)
library(stringr)
library(parallel)
library(doParallel)

# Define input and output paths
model_tx_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/QLD_model_tx"
existing_tx_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/Electricity_Transmission_Lines_1km_buff.shp"
output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/QLD_threshold_tx_new"
summaries_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/QLD_threshold_tx_new_summaries"
summary_output_path <- file.path(summaries_path, "QLD_threshold_tx_new_summary.csv")

# Create output directories if they don't exist
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
dir.create(summaries_path, recursive = TRUE, showWarnings = FALSE)

# Read existing transmission lines buffer - do this once outside the parallel process
existing_tx <- st_read(existing_tx_path)
cat("Read existing transmission lines buffer\n")

# Get list of all model transmission files
model_files <- list.files(model_tx_path, 
                          pattern = "transmission_y2050_t\\d+\\.shp$", 
                          full.names = TRUE)
cat("Found", length(model_files), "model files to process\n")

# Set up parallel processing
num_cores <- detectCores() - 1  # Leave one core free for system processes
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Export necessary objects and functions to the cluster
clusterExport(cl, c("existing_tx", "output_path", "summaries_path"))

# Modified process_threshold_layer function for parallel execution
process_threshold_layer <- function(file_path) {
  require(sf)
  require(dplyr)
  require(stringr)
  
  # Extract threshold number from filename
  threshold <- str_extract(basename(file_path), "t\\d+") %>%
    str_replace("t", "") %>%
    as.numeric()
  
  # Read the model layer
  model_layer <- st_read(file_path, quiet = TRUE)
  
  # Match CRS with existing transmission lines
  if (st_crs(model_layer) != st_crs(existing_tx)) {
    model_layer <- st_transform(model_layer, st_crs(existing_tx))
  }
  
  # Erase overlapping areas
  clipped_layer <- st_difference(model_layer, existing_tx)
  
  # Save processed layer
  output_file <- file.path(output_path, paste0("threshold_", threshold, "_tx_new.shp"))
  
  # Force delete existing file if it exists (using tryCatch to handle potential file locks)
  tryCatch({
    if (file.exists(output_file)) {
      file.remove(output_file)
      file.remove(gsub("\\.shp$", "\\.dbf", output_file))
      file.remove(gsub("\\.shp$", "\\.prj", output_file))
      file.remove(gsub("\\.shp$", "\\.shx", output_file))
    }
  }, error = function(e) {
    warning("Could not remove existing files for threshold ", threshold)
  })
  
  # Save the layer with explicit overwrite
  st_write(clipped_layer, output_file, driver = "ESRI Shapefile", 
           delete_layer = TRUE, quiet = TRUE)
  
  # Create summary
  layer_summary <- clipped_layer %>%
    filter(!is.na(kv) & kv != 0) %>%
    mutate(length_km = as.numeric(st_length(geometry) / 1000)) %>%
    group_by(kv) %>%
    summarize(total_length_km = sum(length_km), .groups = "drop") %>%
    mutate(layer_name = paste0("threshold_", threshold)) %>%
    select(layer_name, capacity = kv, length_km = total_length_km) %>%
    st_drop_geometry()
  
  # Save individual summary
  summary_file <- file.path(summaries_path, 
                            paste0("threshold_", threshold, "_summary.csv"))
  write.csv(layer_summary, summary_file, row.names = FALSE)
  
  return(layer_summary)
}

# Process files in parallel
all_summaries <- parLapply(cl, model_files, process_threshold_layer)

# Stop the cluster
stopCluster(cl)

# Combine all summaries into a single dataframe
combined_summary <- do.call(rbind, all_summaries)

# Write combined summary to CSV
write.csv(combined_summary, summary_output_path, row.names = FALSE)

# Print completion message
cat("\nProcessing complete.\n")
cat("Shapefiles saved to:", output_path, "\n")
cat("Summaries saved to:", summaries_path, "\n")

# List the files in both output directories to confirm
cat("\nFiles in shapefile output directory:\n")
list.files(output_path, pattern = "\\.shp$")
cat("\nFiles in summaries directory:\n")
list.files(summaries_path, pattern = "\\.csv$")     year = attrs$year,
          threshold = attrs$threshold,
          source_layer = layer_name
        )
      
      processed_layers[[length(processed_layers) + 1]] <- layer_data
      cat("Successfully processed layer\n")
      
    }, error = function(e) {
      warning(sprintf("Error processing layer %s: %s", layer_name, e$message))
    })
  }
  
  # Check if any layers were processed
  if (length(processed_layers) == 0) {
    stop("No matching layers were found or processed successfully")
  }
  
  # Combine all processed layers
  merged_layers <- do.call(rbind, processed_layers)
  
  # Create output directory if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  
  # Save the complete merged dataset
  output_file <- file.path(output_folder, "merged_transmission_lines.gpkg")
  st_write(merged_layers, output_file, delete_layer = TRUE)
  
  # Split and save separate layers for each year/threshold combination
  output_gpkg <- file.path(output_folder, "split_transmission_lines2.gpkg")
  for (yr in years) {
    for (thresh in thresholds) {
      # Filter data for this year/threshold combination
      subset_data <- merged_layers %>%
        filter(year == yr, threshold == thresh)
      
      if (nrow(subset_data) > 0) {
        # Create layer name
        layer_name <- sprintf("transmission_y%d_t%d", yr, thresh)
        
        # Save to GPKG with additional options to prevent schema prefix
        st_write(subset_data, 
                 dsn = output_gpkg, 
                 layer = layer_name,
                 layer_options = c("OVERWRITE=YES", "SCHEMA="),  # Add these options
                 delete_layer = TRUE)
        
        cat(sprintf("Saved layer: %s\n", layer_name))
      }
    }
  }  
  return(merged_layers)
}

# Execute the processing
merged_data <- process_gdb_layers(
  input_gdb_path = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx1.gdb",
  output_folder = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission",
  years = c(2050),
  thresholds = c(0, 10, 30, 50, 70, 90)
)

# Print summary of processed data including kv attribute
summary_data <- merged_data %>%
  st_drop_geometry() %>%
  group_by(technology, line_type, year, threshold, kv) %>%
  summarise(count = n(), .groups = 'drop')

print(summary_data)

------------second run--------------------------------
#-------------------------------------------------------------

# Function to list layers matching pattern
list_matching_layers <- function(input_gdb_path, years, thresholds) {
  # Get all layers from GDB
  layers <- sf::st_layers(input_gdb_path)$name
  
  # Create patterns for matching
  tech_patterns <- c("off", "pv", "wind", "interTX")
  line_types <- c("bulk", "spur", "sink", "export")
  
  # Filter layers based on criteria
  matching_layers <- layers[sapply(layers, function(layer) {
    # Check if layer contains year
    has_year <- any(sapply(years, function(y) grepl(paste0("_", y, "_"), layer)))
    
    # Check if layer contains threshold
    has_threshold <- any(sapply(thresholds, function(t) grepl(paste0("_", t, "_"), layer)))
    
    # Check if layer contains technology
    has_tech <- any(sapply(tech_patterns, function(t) grepl(paste0("^", t), layer)))
    
    # Check if layer contains line type
    has_line_type <- any(sapply(line_types, function(l) grepl(paste0("_", l, "$"), layer)))
    
    return(has_year && has_threshold && has_tech && has_line_type)
  })]
  
  # Create a data frame with layer information
  layer_info <- data.frame(
    layer_name = matching_layers,
    technology = sapply(matching_layers, function(l) {
      tech <- tech_patterns[sapply(tech_patterns, function(t) grepl(paste0("^", t), l))]
      return(tech[1])
    }),
    line_type = sapply(matching_layers, function(l) {
      line <- line_types[sapply(line_types, function(t) grepl(paste0("_", t, "$"), l))]
      return(line[1])
    }),
    threshold = sapply(matching_layers, function(l) {
      thresh_pattern <- "_\\d+_"
      thresh_match <- regmatches(l, regexpr(thresh_pattern, l))
      gsub("_", "", thresh_match)
    })
  )
  
  return(layer_info)
}

# Function to print summary of identified layers
print_layer_summary <- function(layer_info) {
  cat("\nIdentified Layers Summary:\n")
  cat("=======================\n")
  cat("Total layers found:", nrow(layer_info), "\n\n")
  
  # Group by technology and threshold
  tech_summary <- table(layer_info$technology, layer_info$threshold)
  cat("Layers by Technology and Threshold:\n")
  print(tech_summary)
  
  cat("\nDetailed Layer List:\n")
  for(i in 1:nrow(layer_info)) {
    cat(sprintf("%d. %s\n", i, layer_info$layer_name[i]))
  }
}

# Function to save layers to GPKG
save_to_gpkg <- function(input_gdb_path, layer_info, output_folder) {
  # Create output folder if it doesn't exist
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Process each layer in parallel
  future.seed <- TRUE  # Set seed for reproducibility
  
  future_map(1:nrow(layer_info), function(i) {
    layer <- layer_info$layer_name[i]
    tech <- layer_info$technology[i]
    threshold <- layer_info$threshold[i]
    
    # Read layer from GDB
    sf_layer <- sf::st_read(input_gdb_path, layer, quiet = TRUE)
    
    # Create output filename
    output_file <- file.path(output_folder, 
                             paste0(tech, "_threshold_", threshold, ".gpkg"))
    
    # Save to GPKG with error handling
    tryCatch({
      sf::st_write(sf_layer, output_file, layer, append = TRUE, quiet = TRUE)
    }, error = function(e) {
      warning(sprintf("Error writing layer %s: %s", layer, e$message))
    })
  }, .options = furrr::furrr_options(seed = TRUE))
}

# Function to add attributes to layers
add_attributes <- function(output_folder, layer_info) {
  future_map(1:nrow(layer_info), function(i) {
    layer <- layer_info$layer_name[i]
    tech <- layer_info$technology[i]
    line_type <- layer_info$line_type[i]
    threshold <- layer_info$threshold[i]
    
    # Construct GPKG path
    gpkg_path <- file.path(output_folder, 
                           paste0(tech, "_threshold_", threshold, ".gpkg"))
    
    tryCatch({
      # Check if file exists
      if (!file.exists(gpkg_path)) {
        warning(sprintf("File not found: %s", gpkg_path))
        return(NULL)
      }
      
      # Read layer
      sf_layer <- sf::st_read(gpkg_path, layer, quiet = TRUE)
      
      # Add attributes
      sf_layer$technology <- tech
      sf_layer$line_type <- line_type
      
      # Save back to GPKG, overwriting existing layer
      sf::st_write(sf_layer, gpkg_path, layer, 
                   delete_layer = TRUE, 
                   quiet = TRUE)
    }, error = function(e) {
      warning(sprintf("Error processing layer %s in %s: %s", 
                      layer, gpkg_path, e$message))
    })
  }, .options = furrr::furrr_options(seed = TRUE))
}

# Function to merge layers by threshold
merge_layers <- function(output_folder, layer_info) {
  # Get unique thresholds
  thresholds <- unique(layer_info$threshold)
  
  # Process each threshold
  future_map(thresholds, function(thresh) {
    # Get all layers for this threshold
    thresh_layers <- layer_info[layer_info$threshold == thresh, ]
    
    # Read and combine all layers
    combined_layer <- future_map(1:nrow(thresh_layers), function(i) {
      layer <- thresh_layers$layer_name[i]
      tech <- thresh_layers$technology[i]
      
      gpkg_path <- file.path(output_folder, 
                             paste0(tech, "_threshold_", thresh, ".gpkg"))
      
      sf::st_read(gpkg_path, layer)
    }, .options = furrr::furrr_options(seed = TRUE)) %>% 
      bind_rows()
    
    # Save merged layer
    output_file <- file.path(output_folder, 
                             paste0("merged_threshold_", thresh, ".gpkg"))
    sf::st_write(combined_layer, output_file)
  }, .options = furrr::furrr_options(seed = TRUE))
}
# Main execution function
process_gis_layers <- function(input_gdb_path, output_folder, years, thresholds) {
  # Step 1: List matching layers
  layer_info <- list_matching_layers(input_gdb_path, years, thresholds)
  
  # Step 2: Save to GPKG
  save_to_gpkg(input_gdb_path, layer_info, output_folder)
  
  # Step 3: Add attributes
  add_attributes(output_folder, layer_info)
  
  # Step 4: Merge layers
  merge_layers(output_folder, layer_info)
  
  return(layer_info)
}

##---------------------individual run steps-----------------------------------
#set up processing info
layer_info <- list_matching_layers(
  input_gdb_path = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx2.gdb",
  years = c(2050),
  thresholds = c(0, 10, 30, 50, 70, 90)
)

#If the results look good, save to GPKG:
save_to_gpkg(
  input_gdb_path = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx2.gdb",
  layer_info = layer_info,
  output_folder = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_transmission"
)

#Add attributes to the layers:
add_attributes(
  output_folder = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_transmission",
  layer_info = layer_info
)

#Finally, merge the layers by threshold:
merge_layers(
  output_folder = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_transmission",
  layer_info = layer_info
)

#Or, if you want to run everything at once:
results <- process_gis_layers(
  input_gdb_path = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/QLD_v202412_eplus_tx2.gdb",
  output_folder = "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_transmission",
  years = c(2050),
  thresholds = c(0, 10, 30, 50, 70, 90)
)