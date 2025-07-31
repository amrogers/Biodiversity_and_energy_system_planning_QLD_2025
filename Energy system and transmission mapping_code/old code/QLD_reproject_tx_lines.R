# Load required libraries
library(sf)

# File paths existing lines
input_file <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/QLD_existing_tx.shp"
output_file <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/Electricity_Transmission_Lines/QLD_existing_tx_projected.shp"

# Read the shapefile
lines <- st_read(input_file)

# Define an appropriate projected coordinate system (e.g., GDA2020 MGA Zone 55)
projected_crs <- 7855  # EPSG code for GDA2020 MGA Zone 55

# Transform to the projected CRS
lines_projected <- st_transform(lines, crs = projected_crs)

# Calculate the length of each line in kilometers
lines_projected$length_km_projected <- st_length(lines_projected) / 1000  # Convert from meters to kilometers

# Save the updated shapefile
st_write(lines_projected, output_file)

# Print a confirmation
cat("Projected shapefile saved to:", output_file, "\n")
"\n")
