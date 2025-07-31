library(sf)
library(dplyr)
library(future)
library(future.apply)
library(tools)
library(stringr)

# Set up parallel processing
plan(multisession)

# Define input and output paths
base_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx1_domestic_transmission"
input_dir <- file.path(base_path, "QLD_model_tx")
centerline_output_dir <- file.path(base_path, "buffered_centerlines")
final_output_dir <- file.path(base_path, "tx1_simplified_buffered")

# Create output directories if they don't exist
dir.create(centerline_output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(final_output_dir, showWarnings = FALSE, recursive = TRUE)

# Function to calculate easement radius based on max_kv
calculate_easement <- function(max_kv) {
  # Default easement value if no valid KV is found
  if (is.na(max_kv) || !is.finite(max_kv)) {
    message("No valid KV value found, using default easement value of 15m")
    return(15)
  }
  
  case_when(
    max_kv > 500 ~ 35,
    max_kv >= 275 & max_kv <= 330 ~ 30,
    max_kv >= 60 & max_kv <= 220 ~ 20,
    max_kv > 0 & max_kv < 60 ~ 15,
    TRUE ~ 15  # Default value if none of the above conditions are met
  )
}

# Function to safely create buffer
safe_buffer <- function(geometry, distance) {
  tryCatch({
    if (!all(st_is_valid(geometry))) {
      geometry <- st_make_valid(geometry)
    }
    
    if (!is.finite(distance)) {
      message("Non-finite buffer distance detected, using default value of 15m")
      distance <- 15
    }
    
    buffered <- st_buffer(geometry, distance)
    
    if (!all(st_is_valid(buffered))) {
      buffered <- st_make_valid(buffered)
    }
    
    return(buffered)
  }, error = function(e) {
    message(paste("Buffer operation failed:", e$message))
    # Return a minimal valid buffer as fallback
    return(st_buffer(geometry, 15))
  })
}

# Main processing function
process_transmission_layer <- function(threshold) {
  tryCatch({
    # Read input shapefile
    input_file <- file.path(input_dir, sprintf("transmission_y2050_t%d.shp", threshold))
    message(sprintf("Processing threshold %d: Reading file %s", threshold, input_file))
    
    tx_lines <- st_read(input_file, quiet = TRUE)
    
    # Print column names to help diagnose voltage field
    message("Available columns in the shapefile:")
    print(names(tx_lines))
    
    # Check for voltage field - try common names
    voltage_field <- NULL
    possible_voltage_fields <- c("kv", "voltage", "VOLTAGE", "KV", "voltage_kv", "Voltage")
    for (field in possible_voltage_fields) {
      if (field %in% names(tx_lines)) {
        voltage_field <- field
        break
      }
    }
    
    if (is.null(voltage_field)) {
      message("No voltage field found. Using default values.")
      tx_lines$kv <- 15  # Add default voltage field
    } else {
      message(sprintf("Using voltage field: %s", voltage_field))
      # Rename the found field to 'kv' for consistency
      tx_lines$kv <- tx_lines[[voltage_field]]
    }
    
    # Convert voltage field to numeric if it's not already
    tx_lines$kv <- as.numeric(as.character(tx_lines$kv))
    
    # Replace any NA or non-finite values with default
    tx_lines$kv[is.na(tx_lines$kv) | !is.finite(tx_lines$kv)] <- 15
    
    # Print summary of voltage values
    message("Voltage values summary:")
    print(summary(tx_lines$kv))
    
    # Ensure projection is GDA2020 MGA Zone 55
    tx_lines <- st_transform(tx_lines, 28355)
    
    # Validate input geometries
    if (!all(st_is_valid(tx_lines))) {
      message("Fixing invalid input geometries...")
      tx_lines <- st_make_valid(tx_lines)
    }
    
    # 1. Add 100m buffer
    message("Creating initial 100m buffer...")
    buffered <- safe_buffer(tx_lines, 100)
    
    # 2. Dissolve overlapping buffers
    message("Dissolving overlapping buffers...")
    dissolved <- st_union(buffered)
    
    # 3. Find centerlines
    message("Creating centerlines...")
    centerlines <- st_combine(st_geometry(tx_lines)) %>%
      st_line_merge()
    
    if (!all(st_is_valid(centerlines))) {
      centerlines <- st_make_valid(centerlines)
    }
    
    # Save raw centerlines to new directory
    message("Saving raw centerlines...")
    centerline_sf <- st_sf(geometry = centerlines)
    st_write(centerline_sf,
             file.path(raw_centerline_dir, sprintf("centerline_t%d.shp", threshold)),
             delete_layer = TRUE)
    
    # 4. Create 500m buffer around centerlines
    message("Creating 500m buffer around centerlines...")
    centerline_buffer <- safe_buffer(centerlines, 500)
    
    # 5. Save buffered centerline
    message("Saving buffered centerline...")
    centerline_output <- st_sf(geometry = centerline_buffer)
    st_write(centerline_output,
             file.path(centerline_output_dir, sprintf("buffered_centerline_t%d.shp", threshold)),
             delete_layer = TRUE)
    
    # 6 & 7. Find max KV and count of transmission lines within buffer
    message("Calculating max KV and transmission line count...")
    centerline_data <- st_intersection(tx_lines, centerline_buffer) %>%
      group_by() %>%
      summarize(
        max_kv = max(kv, na.rm = TRUE),
        tx_count = n()
      )
    
    # Print summary of calculated values
    message("Centerline data summary:")
    print(summary(centerline_data))
    
    # 8. Calculate easement radius
    message("Calculating easement radius...")
    easement_radius <- calculate_easement(centerline_data$max_kv)
    message(sprintf("Calculated easement radius: %f", easement_radius))
    
    # 9. Buffer based on easement radius
    message("Creating final buffer...")
    final_buffer <- safe_buffer(centerlines, easement_radius)
    
    # 10. Save final buffered layer
    message("Saving final buffered layer...")
    st_write(final_buffer,
             file.path(final_output_dir, sprintf("tx_simplified_buffered_t%d.shp", threshold)),
             delete_layer = TRUE)
    
    # 11. Calculate area in square kilometers
    message("Calculating final area...")
    area_sq_km <- st_area(final_buffer) %>%
      units::set_units("km^2") %>%
      as.numeric()
    
    message(sprintf("Processing complete for threshold %d", threshold))
    return(data.frame(threshold = threshold, area_sq_km = area_sq_km))
    
  }, error = function(e) {
    message(sprintf("Error processing threshold %d: %s", threshold, e$message))
    return(NULL)
  })
}

# Get list of thresholds
thresholds <- c(0, 10, 30, 50, 70, 90)

# Process all thresholds in parallel
message("Starting parallel processing...")
results <- future_lapply(thresholds, process_transmission_layer)

# Combine results and save area table
message("Combining results and saving area table...")
valid_results <- results[!sapply(results, is.null)]
if (length(valid_results) > 0) {
  area_table <- do.call(rbind, valid_results) %>%
    as.data.frame()
  
  # Save area results
  write.csv(area_table,
            file.path(final_output_dir, "threshold_areas.csv"),
            row.names = FALSE)
  
  message("Area table saved successfully!")
} else {
  message("No valid results to save!")
}

message("Processing complete!")