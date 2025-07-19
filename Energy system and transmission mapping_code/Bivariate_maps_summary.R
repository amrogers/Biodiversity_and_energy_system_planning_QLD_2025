# Load required packages
library(sf)
library(dplyr)

# Set input and output paths
output_dir <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/Bivariate_maps/bivariate_maps/bivariate_summaries"

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Function to create categories based on resource type
create_resource_categories <- function(data, resource_type) {
  data %>%
    mutate(
      area_km2 = as.numeric(st_area(.))/1000000,  # Convert m² to km²
      # Create zonation categories
      zonation_cat = case_when(
        Zonation_B >= 90 ~ 6,
        Zonation_B >= 70 ~ 5,
        Zonation_B >= 50 ~ 4,
        Zonation_B >= 30 ~ 3,
        Zonation_B >= 10 ~ 2,
        TRUE ~ 1
      ),
      # Create resource-specific categories
      resource_cat = if(resource_type == "wind") {
        case_when(
          Wind_CF >= 30 ~ 2,
          TRUE ~ 1
        )
      } else {
        case_when(
          Solar_CF >= 15 ~ 2,
          TRUE ~ 1
        )
      }
    )
}

# Function to create summary with category ranges
create_summary <- function(data, area_name, resource_type) {
  categorized_data <- create_resource_categories(data, resource_type)
  
  categorized_data %>%
    # Remove geometry and group by categories
    st_drop_geometry() %>%
    group_by(zonation_cat, resource_cat) %>%
    summarize(
      total_area_km2 = sum(area_km2),
      .groups = "drop"
    ) %>%
    # Calculate percentage
    mutate(
      percentage = total_area_km2 / sum(total_area_km2) * 100,
      area_name = area_name,
      resource_type = resource_type,
      # Add category descriptions
      zonation_range = case_when(
        zonation_cat == 6 ~ "90-100",
        zonation_cat == 5 ~ "70-89.9",
        zonation_cat == 4 ~ "50-69.9",
        zonation_cat == 3 ~ "30-49.9",
        zonation_cat == 2 ~ "10-29.9",
        zonation_cat == 1 ~ "0-9.9"
      ),
      resource_range = case_when(
        resource_type == "wind" & resource_cat == 2 ~ "30-40",
        resource_type == "wind" & resource_cat == 1 ~ "<29.9",
        resource_type == "solar" & resource_cat == 2 ~ "15-24.9",
        resource_type == "solar" & resource_cat == 1 ~ "0-14.9",
        TRUE ~ NA_character_
      )
    ) %>%
    # Round numbers
    mutate(
      total_area_km2 = round(total_area_km2, 2),
      percentage = round(percentage, 2)
    ) %>%
    # Reorder columns
    select(area_name, resource_type,
           zonation_cat, zonation_range,
           resource_cat, resource_range,
           total_area_km2, percentage)
}

# Function to standardize field names
standardize_fields <- function(data, resource_type) {
  # Get the actual zonation field name from the data
  zonation_field <- names(data)[grep("^[Zz]onation", names(data))]
  
  # Rename fields based on resource type
  if (resource_type == "wind") {
    data %>%
      rename_with(~"Zonation_B", all_of(zonation_field)) %>%
      rename_with(~"Wind_CF", matches("^Wind"))
  } else {
    data %>%
      rename_with(~"Zonation_B", all_of(zonation_field)) %>%
      rename_with(~"Solar_CF", matches("^Solar"))
  }
}

# Function to process a single resource type
process_resource <- function(resource_type) {
  # Set file paths based on resource type
  base_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/Bivariate_maps/bivariate_maps"
  
  file_paths <- list(
    full = file.path(base_path, "QLD", sprintf("QLD_%s_x_CF2.shp", resource_type)),
    rez = file.path(base_path, "QLD_REZ_clip", sprintf("QLD_%s_x_CF2_REZ.shp", resource_type)),
    non_rez = file.path(base_path, "QLD_outside_REZ", sprintf("QLD_%s_x_CF2_outside_REZ.shp", resource_type))
  )
  
  # Read and process data with standardized field names
  rez_areas <- st_read(file_paths$rez) %>% 
    st_transform(28355) %>%
    standardize_fields(resource_type)
  
  non_rez_areas <- st_read(file_paths$non_rez) %>% 
    st_transform(28355) %>%
    standardize_fields(resource_type)
  
  # Create summaries
  rez_summary <- create_summary(rez_areas, "Within REZ", resource_type)
  non_rez_summary <- create_summary(non_rez_areas, "Outside REZ", resource_type)
  
  # Combine summaries
  combined_summary <- bind_rows(rez_summary, non_rez_summary)
  
  # Save resource-specific summary
  write.csv(combined_summary, 
            file.path(output_dir, sprintf("%s_zonation_summary.csv", resource_type)), 
            row.names = FALSE)
  
  return(combined_summary)
}

# Process both wind and solar data
process_resources <- function(resources = c("wind", "solar")) {
  all_summaries <- lapply(resources, process_resource) %>%
    bind_rows()
  
  # Save combined summary for all resources
  write.csv(all_summaries, 
            file.path(output_dir, "combined_resource_zonation_summary.csv"), 
            row.names = FALSE)
  
  # Print summaries for each resource
  for(resource in resources) {
    resource_data <- all_summaries %>% filter(resource_type == resource)
    
    print(sprintf("\nSummary of %s and Zonation Categories:", toupper(resource)))
    print(paste(rep("=", 50), collapse=""))
    print(resource_data %>% 
            arrange(area_name, desc(zonation_cat), desc(resource_cat)))
    
    # Print total areas for this resource
    print(sprintf("\nTotal Areas for %s (km²):", toupper(resource)))
    print(paste(rep("=", 30), collapse=""))
    resource_data %>%
      group_by(area_name) %>%
      summarize(total_area_km2 = sum(total_area_km2)) %>%
      print()
  }
}

# Choose which analysis to run:

# For both wind and solar:
process_resources()

# For wind only:
# process_resources("wind")

# For solar only:
process_resources("solar")

# Uncomment the line you want to use and comment out the others)
  ),
  non_rez_summary %>% summarize(
    area_name = "Outside REZ",
    total_area_km2 = sum(total_area_km2)
  )
) %>% print()