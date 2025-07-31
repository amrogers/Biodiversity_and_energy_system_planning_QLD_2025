rm(list = ls())
library(sf)
library(dplyr)
library(parallel)
library(purrr)
library(readr)

# Function to process one threshold
process_threshold <- function(data) {
  # Extract parameters
  threshold <- data$threshold
  base_path <- data$base_path
  existing_tx_path <- data$existing_tx_path
  output_path <- data$output_path
  
  # Define input file paths
  tx_filename <- file.path(base_path, paste0("transmission_y2050_t", threshold, ".shp"))
  
  # Load data
  existing_tx <- st_read(existing_tx_path, quiet = TRUE)
  model_tx <- st_read(tx_filename, quiet = TRUE)
  
  # Ensure CRS match
  if (st_crs(existing_tx) != st_crs(model_tx)) {
    model_tx <- st_transform(model_tx, st_crs(existing_tx))
  }
  
  # Process each polygon in existing_tx
  results <- existing_tx %>%
    rowwise() %>%
    mutate(
      max_QLD_model_tx = {
        intersecting <- model_tx[st_intersects(geometry, model_tx)[[1]], ]
        if (nrow(intersecting) > 0) max(intersecting$kv, na.rm = TRUE) else NA_real_
      },
      line_count = {
        intersecting <- model_tx[st_intersects(geometry, model_tx)[[1]], ]
        nrow(intersecting)
      }
    ) %>%
    ungroup()
  
  # Save results as shapefile
  shapefile_output <- file.path(output_path, paste0("existing_model_tx_join_", threshold, ".shp"))
  st_write(results, shapefile_output, append = FALSE, quiet = TRUE)
  
  # Remove geometry and save as CSV
  results_csv <- results %>% st_drop_geometry()
  csv_output <- file.path(output_path, paste0("existing_model_tx_join_", threshold, ".csv"))
  write.csv(results_csv, csv_output, row.names = FALSE)
  
  cat(sprintf("Completed processing threshold %d\n", threshold))
  return(results)
}

# Main execution function
run_parallel_analysis <- function() {
  # Define paths and thresholds
  base_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/QLD_model_tx"
  existing_tx_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/QLD_existing_tx_simplified_buff_no_overlap2.shp"
  output_path <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/exisitng_model_tx_max_join2"
  
  thresholds <- model_thresholds
  
  # Create parameter sets
  param_list <- lapply(thresholds, function(t) {
    list(
      threshold = t,
      base_path = base_path,
      existing_tx_path = existing_tx_path,
      output_path = output_path
    )
  })
  
  # Setup parallel processing
  num_cores <- min(detectCores() - 1, 8)  # Limit to 8 cores
  cat(sprintf("Running analysis on %d cores\n", num_cores))
  cl <- makeCluster(num_cores)
  
  # Export necessary functions and libraries to the cluster
  clusterEvalQ(cl, {
    library(sf)
    library(dplyr)
  })
  clusterExport(cl, c("process_threshold"))
  
  # Run the analysis in parallel
  start_time <- Sys.time()
  results_list <- parLapply(cl, param_list, process_threshold)
  end_time <- Sys.time()
  
  # Stop the cluster
  stopCluster(cl)
  
  # Print execution time
  time_taken <- end_time - start_time
  cat(sprintf("\nAnalysis completed in %.2f minutes\n", as.numeric(time_taken, units = "mins")))
  
  return(results_list)
}


# set thresholds

model_thresholds <- c(0, 10, 30, 50, 70, 90) #0, 10, 30, 50, 70, 90

# Add these checks before run_parallel_analysis()
stopifnot(dir.exists(base_path))
stopifnot(file.exists(existing_tx_path))
stopifnot(dir.exists(output_path))
# Run the analysis
results <- run_parallel_analysis()



#-----------------------------summarize results--------------------------------


library(tidyverse)

# Set the working directory
base_dir <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/tx2_domestic_transmission/exisitng_model_tx_max_join2"
setwd(base_dir)

# Define the threshold values
thresholds <- c(0, 10, 30, 50, 70, 90)

# Function to process a single CSV file
process_file <- function(threshold) {
  # Construct file name
  filename <- paste0("existing_model_tx_join_", threshold, ".csv")
  
  tryCatch({
    # Check if the file exists
    if (!file.exists(filename)) {
      warning(sprintf("File not found: %s", filename))
      return(NULL)
    }
    
    # Read the CSV file
    data <- read_csv(filename, show_col_types = FALSE) %>%
      # Filter out null and 0 values in max_QLD_model_tx
      filter(!is.na(max_QLD_model_tx), max_QLD_model_tx != 0, length_km>5) %>%
      # Group by capacity and calculate the total length
      group_by(mx_nrb_) %>%
      summarise(
        total_length_km = sum(length_km, na.rm = TRUE)
      ) %>%
      # Add threshold column from the input value
      mutate(
        threshold = threshold
      )
    
    return(data)
  }, error = function(e) {
    warning(sprintf("Error processing file for threshold %d: %s", threshold, e$message))
    return(NULL)
  })
}

# Process all files and combine results
combined_summary <- map_dfr(thresholds, process_file)

# Check if combined_summary has valid data
if (nrow(combined_summary) == 0) {
  stop("No valid data to process. Check the input files or processing logic.")
}


# Save the combined summary
write_csv(combined_summary, "tx_join_summary.csv")

# Create a preview with nice formatting
preview <- combined_summary %>%
  arrange(threshold, desc(total_length_km))

# Print preview of results
print(preview, n = )
processing function with correct column names
process_file <- function(threshold) {
  filename <- paste0("existing_model_tx_join_", threshold, ".csv")
  
  tryCatch({
    data <- read_csv(filename, show_col_types = FALSE) %>%
      filter(!is.na(max_QLD_model_tx), max_QLD_model_tx != 0) %>%
      group_by(cpcty_k) %>%  # Changed from capacity_k to cpcty_k
      summarise(
        total_length_km = sum(lngth_k, na.rm = TRUE),  # Using lngth_k
        n_segments = n()
      ) %>%
      mutate(
        threshold = threshold,
        avg_length_km = total_length_km / n_segments
      )
    
    return(data)
  }, error = function(e) {
    warning(sprintf("Error processing file for threshold %d: %s", threshold, e$message))
    return(NULL)
  })
}

# Process all files and combine results
combined_summary <- map_dfr(thresholds, process_file)

# Add additional summary statistics
combined_summary <- combined_summary %>%
  arrange(threshold, desc(total_length_km)) %>%
  group_by(threshold) %>%
  mutate(
    pct_of_total = total_length_km / sum(total_length_km) * 100
  )

# Save and print results
write_csv(combined_summary, "tx_join_summary.csv")

# Create a preview
preview <- combined_summary %>%
  select(threshold, cpcty_k, total_length_km, n_segments, avg_length_km, pct_of_total) %>%
  arrange(threshold, desc(total_length_km))

print(preview, n = )