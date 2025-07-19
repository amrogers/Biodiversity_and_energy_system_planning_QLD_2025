# Load required libraries
library(sf)
library(dplyr)
library(readr)

# Set the directory containing the shapefiles
shapefile_dir <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/QLD_100m_SNES_500spp/shapefiles"
output_dir <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/QLD_100m_SNES_500spp"

# Create an empty dataframe to store the results
species_data <- data.frame(
  ShapefileID = integer(),
  SpeciesName = character(),
  CommonName = character(),
  ThreatStatus = character(),
  stringsAsFactors = FALSE
)

# Get list of all shapefiles
shapefiles <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE)

# Loop through each shapefile
for (shapefile in shapefiles) {
  # Extract the shapefile ID (file number)
  file_name <- basename(shapefile)
  file_id <- as.integer(tools::file_path_sans_ext(file_name))
  
  # Read the shapefile
  tryCatch({
    shape_data <- st_read(shapefile, quiet = TRUE)
    
    # Extract the required attributes - take first row since attributes should be the same for all features
    # Use the first row, but handle empty shapefiles
    if (nrow(shape_data) > 0) {
      species_name <- shape_data$CURRENT_NA[1]
      common_name <- shape_data$COMMON_NA[1]
      threat_status <- shape_data$THREATENED[1]
      
      # Add to the dataframe
      species_data <- rbind(species_data, data.frame(
        ShapefileID = file_id,
        SpeciesName = species_name,
        CommonName = common_name,
        ThreatStatus = threat_status,
        stringsAsFactors = FALSE
      ))
      
      # Print progress every 50 files
      if (file_id %% 50 == 0) {
        cat("Processed file", file_id, "\n")
      }
    } else {
      warning(paste("Shapefile", file_name, "is empty. Skipping."))
    }
  }, error = function(e) {
    warning(paste("Error processing", file_name, ":", e$message))
  })
}

# Sort the dataframe by ShapefileID
species_data <- species_data %>% arrange(ShapefileID)

# Save the results to a CSV file
output_file <- file.path(output_dir, "species_attributes.csv")
write_csv(species_data, output_file)

cat("Process complete. Results saved to:", output_file, "\n")
cat("Total species processed:", nrow(species_data), "\n")