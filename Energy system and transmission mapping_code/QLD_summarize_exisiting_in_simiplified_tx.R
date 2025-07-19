#summarize exisiting transmission details into simplified tx for appropriate upgrade metrics 
library(sf)
library(future.apply)
library(dplyr)

# [Previous script remains the same, add this line before writing output]

# Calculate line length in kilometers
simplified_tx_with_results <- simplified_tx_with_results %>%
  mutate(line_length_km = as.numeric(st_length(.) / 1000))

# Write shapefile
st_write(simplified_tx_with_results, 
         "C:\\Users\\andrewrogers\\OneDrive - The University of Melbourne\\Boundless_data\\QLD\\Electricity_Transmission_Lines\\QLD_existing_tx_simplified_with_results.shp",
         delete_dsn = TRUE)

# Write CSV of attribute table
write.csv(st_drop_geometry(simplified_tx_with_results), 
          "C:\\Users\\andrewrogers\\OneDrive - The University of Melbourne\\Boundless_data\\QLD\\Electricity_Transmission_Lines\\QLD_existing_tx_simplified_results.csv", 
          row.names = FALSE)

# Print summary
print(summary(simplified_tx_with_results))


# Write CSV of attribute table
write.csv(st_drop_geometry(simplified_tx_with_results), 
          "C:\\Users\\andrewrogers\\OneDrive - The University of Melbourne\\Boundless_data\\QLD\\Electricity_Transmission_Lines\\QLD_existing_tx_simplified_results.csv", 
          row.names = FALSE)

# Print summary
print(summary(simplified_tx_with_results))y lines
  nearby_lines <- existing_tx[nearby_lines_mask, ]
  
  # Count nearby lines
  nearby_line_count <- sum(nearby_lines_mask)
  
  # Get max capacity of nearby lines
  max_capacity <- if(nearby_line_count > 0) {
    max(nearby_lines$cpcty_k, na.rm = TRUE)
  } else {
    NA_real_
  }
  
  # Return results
  return(data.frame(
    nearby_line_count = nearby_line_count,
    max_nearby_capacity = max_capacity
  ))
}

# Apply the function in parallel
results <- future_lapply(
  1:nrow(simplified_tx), 
  process_simplified_line, 
  future.packages = c("sf", "dplyr")
)

# Convert results to a dataframe
results_df <- do.call(rbind, results)

# Add results to simplified_tx
simplified_tx_with_results <- simplified_tx %>%
  mutate(
    nearby_line_count = results_df$nearby_line_count,
    max_nearby_capacity = results_df$max_nearby_capacity
  )

# Optional: Write out the updated shapefile
st_write(simplified_tx_with_results, 
         "C:\\Users\\andrewrogers\\OneDrive - The University of Melbourne\\Boundless_data\\QLD\\Electricity_Transmission_Lines\\QLD_existing_tx_simplified_with_results.shp",
         delete_dsn = TRUE)

# Print summary
print(summary(simplified_tx_with_results))


# Write CSV of attribute table
write.csv(st_drop_geometry(simplified_tx_with_results), 
          "C:\\Users\\andrewrogers\\OneDrive - The University of Melbourne\\Boundless_data\\QLD\\Electricity_Transmission_Lines\\QLD_existing_tx_simplified_results.csv", 
          row.names = FALSE)

# Print summary
print(summary(simplified_tx_with_results))