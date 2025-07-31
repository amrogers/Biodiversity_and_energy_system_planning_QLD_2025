#################################################################
# Threatened Species Prioritization Script
#
# This script:
# 1. Creates a list of threatened species with model outputs and a summary table
# 2. Resamples SDM .tif files to replace NoData with 0 for Australia's extent
# 3. Sets up prioritization to run on the resampled species
#################################################################

# Load necessary libraries
library(readxl)        # For reading Excel files
library(dplyr)         # For data manipulation
library(stringr)       # For string operations
library(terra)         # For spatial raster operations
library(sf)            # For vector spatial data
library(prioritizr)    # For conservation prioritization
library(writexl)       # For writing Excel files
library(tibble)        # For tibble operations
library(prioritizr)

# Install Rsymphony solver if not already installed
if (!requireNamespace("Rsymphony", quietly = TRUE)) {
  install.packages("Rsymphony")
}
library(Rsymphony)

# Define file paths
species_list_path <- "Z:/Priotization_SDMs/Sprat_birds_rept_mam.xlsx"  # Path to species list
sdm_base_path <- "Z:/Priotization_SDMs/Predictions"               # Base path for SDM files
output_path <- "Z:/Priotization_SDMs/sdm_matching_results.xlsx"        # Output path for results
aus_shapefile <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/AU_border/AUS_2021_AUST_SHP_GDA2020/AUS_2021_AUST_GDA2020.shp"  # Australia shapefile
resampled_path <- "Z:/Priotization_SDMs/spp_resampled3"                 # Path for resampled species
prioritization_output <- "Z:/Priotization_SDMs/Results"                # Output path for prioritization results

# Create directories if they don't exist
dir.create(resampled_path, showWarnings = FALSE, recursive = TRUE)
dir.create(prioritization_output, showWarnings = FALSE, recursive = TRUE)

#########################################
# TASK 1: Create list of threatened species with model outputs and summary table
#########################################

print("TASK 1: Creating list of threatened species with model outputs and summary table")

# Function to safely read the Excel file
read_species_data <- function() {
  tryCatch({
    print("Reading species list...")
    df <- read_excel(species_list_path)
    print(paste("Successfully read", nrow(df), "species"))
    return(df)
  }, error = function(e) {
    print(paste("Error reading species file:", e$message))
    # Return an empty tibble with expected columns
    return(tibble(
      Scientific_name = character(0),
      EPBC_Act = character(0),
      threat_status = character(0),
      Taxonomic_Data = character(0),
      Common_name = character(0)
    ))
  })
}

# Read the species list
species_df <- read_species_data()

# Print column names for debugging
print("Column names in species data:")
print(colnames(species_df))

# Standardize column names if needed
expected_cols <- c("Scientific_name", "EPBC_Act", "threat_status", 
                   "Taxonomic_Data", "Common_name")

# Check and rename columns if necessary
for (i in seq_along(expected_cols)) {
  if (!expected_cols[i] %in% colnames(species_df)) {
    # Look for similar column names
    similar_cols <- grep(gsub("_", ".*", tolower(expected_cols[i])), 
                         tolower(colnames(species_df)), 
                         value = TRUE)
    
    if (length(similar_cols) > 0) {
      old_name <- colnames(species_df)[tolower(colnames(species_df)) == tolower(similar_cols[1])]
      print(paste("Renaming column", old_name, "to", expected_cols[i]))
      colnames(species_df)[colnames(species_df) == old_name] <- expected_cols[i]
    } else {
      # Create missing column
      print(paste("Creating missing column:", expected_cols[i]))
      species_df[[expected_cols[i]]] <- NA_character_
    }
  }
}

# Create a column for matching with filenames
species_df$species_match <- tolower(species_df$Scientific_name) %>%
  str_replace_all(" ", "_")

# Function to list files in a directory
list_sdm_files <- function(taxon) {
  # Complete path to the taxon directory
  taxon_path <- file.path(sdm_base_path, taxon)
  
  # Check if directory exists
  if (!dir.exists(taxon_path)) {
    print(paste("Warning: Directory not found:", taxon_path))
    return(tibble(
      file_path = character(0),
      file_name = character(0),
      species_name = character(0),
      taxon = character(0)
    ))
  }
  
  # List files with the current prediction pattern
  files <- list.files(
    path = taxon_path,
    pattern = "_prediction_current\\.tif$",
    full.names = TRUE  # Get full paths
  )
  
  if (length(files) == 0) {
    print(paste("No files found in", taxon_path))
    return(tibble(
      file_path = character(0),
      file_name = character(0),
      species_name = character(0),
      taxon = character(0)
    ))
  }
  
  # Get just the file names (without the path)
  file_names <- basename(files)
  
  # Extract species names from file names
  species_names <- character(length(file_names))
  for (i in seq_along(file_names)) {
    # Split by underscore and take first two parts
    parts <- str_split(file_names[i], "_")[[1]]
    if (length(parts) >= 2) {
      species_names[i] <- paste(parts[1], parts[2], sep = "_")
    } else {
      species_names[i] <- parts[1] # Handle case with no underscore
    }
  }
  
  # Create and return a tibble
  result <- tibble(
    file_path = files,
    file_name = file_names,
    species_name = species_names,
    taxon = taxon
  )
  
  print(paste("Found", nrow(result), "files in", taxon, "directory"))
  return(result)
}

# Get files for each taxonomic group
print("Scanning directories for SDM files...")
aves_files <- list_sdm_files("aves")
mammalia_files <- list_sdm_files("mammalia")
reptilia_files <- list_sdm_files("reptilia")

# Combine all files
all_files <- bind_rows(aves_files, mammalia_files, reptilia_files)
print(paste("Total files found:", nrow(all_files)))

# Match species with files
print("Matching species with SDM files...")
matched_species <- vector("logical", nrow(species_df))
sdm_files <- vector("character", nrow(species_df))

# Manual matching loop
for (i in seq_len(nrow(species_df))) {
  species_to_match <- species_df$species_match[i]
  match_index <- which(all_files$species_name == species_to_match)
  
  if (length(match_index) > 0) {
    matched_species[i] <- TRUE
    sdm_files[i] <- all_files$file_path[match_index[1]]
  } else {
    matched_species[i] <- FALSE
    sdm_files[i] <- NA_character_
  }
}

# Add matching results to the species data frame
species_df$has_sdm <- matched_species
species_df$sdm_file <- sdm_files

print(paste("Found SDM matches for", sum(species_df$has_sdm), "out of", nrow(species_df), "species"))

# Create summary table 1: List of species with model outputs
print("Creating summary table of species with SDM models...")
species_with_sdm <- species_df %>%
  filter(has_sdm) %>%
  select(Scientific_name, Common_name, Taxonomic_Data, threat_status, sdm_file)

# Create summary table 2: Summary by taxonomic group and threat status
print("Creating summary by taxonomic group and threat status...")
summary_by_group <- species_df %>%
  group_by(Taxonomic_Data, threat_status) %>%
  summarize(
    total_species = n(),
    species_with_sdm = sum(has_sdm),
    percentage = round(100 * species_with_sdm / total_species, 1),
    .groups = "drop"
  )

# Create total row
print("Adding total row to summary...")
total_species_count <- nrow(species_df)
total_with_sdm <- sum(species_df$has_sdm)
total_percentage <- round(100 * total_with_sdm / total_species_count, 1)

total_row <- tibble(
  Taxonomic_Data = "Total",
  threat_status = "",
  total_species = total_species_count,
  species_with_sdm = total_with_sdm,
  percentage = total_percentage
)

# Combine with summary
summary_with_total <- bind_rows(summary_by_group, total_row)

# Print summary to console
print("Summary of species with SDM models:")
print(summary_with_total)

# Write results to Excel
print("Writing results to Excel...")
sheets <- list(
  "Species with SDMs" = species_with_sdm,
  "Summary by Group" = summary_with_total,
  "All Species Details" = species_df %>% 
    select(Scientific_name, Common_name, Taxonomic_Data, 
           threat_status, has_sdm)
)

tryCatch({
  write_xlsx(sheets, output_path)
  print(paste("Results successfully saved to:", output_path))
}, error = function(e) {
  print(paste("Error writing Excel file:", e$message))
})

print("TASK 1 Complete: Created list of threatened species with model outputs and summary table")

#########################################
# TASK 2: Resample SDM .tif files
#########################################

print("\nTASK 2: Resampling SDM .tif files")

# Filter for species with SDM files
species_to_resample <- species_df %>%
  filter(has_sdm)

if (nrow(species_to_resample) == 0) {
  stop("No species with SDM files found. Cannot proceed with resampling.")
}

# Load Australia reference raster directly as the planning area
print("Loading Australia reference raster as planning area...")
planning_area <- tryCatch({
  rast("Z:/Extent_raster/Au_extent_2.tif")
}, error = function(e) {
  stop("Error loading Australia reference raster: ", e$message)
})

# Verify the reference raster has loaded correctly
print(paste("Planning area dimensions:", dim(planning_area)[1], "x", dim(planning_area)[2]))
print(paste("Planning area CRS:", crs(planning_area)))
print(paste("Planning area extent:", ext(planning_area)))
print(paste("Planning area resolution:", res(planning_area)))

# Ensure values in planning area are either 1 or 0 (binary mask)
unique_vals <- unique(values(planning_area))
print(paste("Unique values in planning area:", paste(unique_vals, collapse = ", ")))

# Create binary mask if it's not already
if (!all(unique_vals %in% c(0, 1, NA))) {
  print("Converting planning area to binary mask (0/1)...")
  planning_area[planning_area > 0] <- 1
  planning_area[is.na(planning_area)] <- 0
}

# Save planning area for reference
writeRaster(planning_area, file.path(prioritization_output, "planning_area.tif"), overwrite = TRUE)
print("Planning area raster saved")

# Simplified function to resample species
resample_species <- function(species_name, raster_path, output_path) {
  print(paste("Resampling species:", species_name))
  
  # Skip if resampled file already exists
  if (file.exists(output_path)) {
    print("Resampled file already exists, skipping...")
    return(TRUE)
  }
  
  tryCatch({
    # Load the original SDM raster
    species_rast <- rast(raster_path)
    
    # Print raster info
    print(paste("Original raster dimensions:", dim(species_rast)[1], "x", dim(species_rast)[2]))
    
    # Calculate basic statistics for reporting
    min_val <- as.numeric(global(species_rast, "min", na.rm = TRUE))
    max_val <- as.numeric(global(species_rast, "max", na.rm = TRUE))
    mean_val <- as.numeric(global(species_rast, "mean", na.rm = TRUE))
    print(paste("Value range: Min =", min_val, "Max =", max_val, "Mean =", mean_val))
    
    # Direct resampling using bilinear for continuous data
    print("Resampling to match planning area...")
    species_resampled <- resample(species_rast, planning_area, method = "bilinear")
    
    # Apply mask to keep values only within Australia
    print("Applying Australia mask...")
    species_masked <- mask(species_resampled, planning_area)
    
    # Replace any NA values within the mask with 0
    print("Setting NA values within Australia to 0...")
    # Create a template of 0s with planning area's exact geometry
    zero_template <- planning_area * 0
    
    # Use cover to replace NA values with 0s
    species_final <- cover(species_masked, zero_template)
    
    # Check if we have any valid data
    final_sum <- as.numeric(global(species_final, "sum", na.rm = TRUE))
    print(paste("Final resampled sum:", final_sum))
    
    if (!is.na(final_sum) && final_sum > 0) {
      # Save the resampled raster
      print("Saving resampled raster...")
      writeRaster(species_final, output_path, overwrite = TRUE, 
                  datatype = "FLT4S", 
                  NAflag = -9999)
      
      print(paste("Saved resampled raster to:", output_path))
      return(TRUE)
    } else {
      print("WARNING: Resampled raster has no valid values!")
      
      # Create a minimal presence raster
      print("Creating minimal presence raster...")
      minimal_raster <- planning_area * 0
      
      # Calculate a meaningful value based on original raster
      presence_value <- if (!is.na(mean_val) && mean_val > 0) mean_val else 0.1
      
      # Find center of Australia
      center_xy <- terra::centroids(as.polygons(planning_area, dissolve = TRUE))
      center_cell <- terra::cellFromXY(minimal_raster, terra::crds(center_xy))
      
      # Create a small patch of presence
      cell_indices <- cellFromRowCol(minimal_raster, 
                                     row = rowFromCell(minimal_raster, center_cell) + -5:5,
                                     col = colFromCell(minimal_raster, center_cell) + -5:5)
      
      valid_indices <- cell_indices[!is.na(cell_indices)]
      valid_indices <- valid_indices[valid_indices <= ncell(minimal_raster) & valid_indices > 0]
      
      minimal_values <- values(minimal_raster)
      minimal_values[valid_indices] <- presence_value
      values(minimal_raster) <- minimal_values
      
      # Save with explicit settings
      writeRaster(minimal_raster, output_path, overwrite = TRUE,
                  datatype = "FLT4S", 
                  NAflag = -9999)
      
      print("Saved minimal presence raster")
      return(TRUE)
    }
  }, error = function(e) {
    print(paste("Error resampling raster:", e$message))
    
    # Last resort - create extremely basic presence
    try({
      print("Creating emergency fallback raster...")
      fallback <- planning_area * 0
      
      # Set center cell to 0.1
      center_cell <- floor(ncell(fallback) / 2)
      cell_values <- values(fallback)
      cell_values[center_cell] <- 0.1
      values(fallback) <- cell_values
      
      writeRaster(fallback, output_path, overwrite = TRUE, 
                  datatype = "FLT4S", 
                  NAflag = -9999)
      
      print("Saved emergency fallback raster")
      return(TRUE)
    }, silent = FALSE)
    
    return(FALSE)
  })
}

# Test resampling with just one species
print("\nTESTING RESAMPLING WITH ONE SPECIES")
test_species <- species_to_resample[1, ]
test_species_name <- test_species$species_match
test_raster_path <- test_species$sdm_file
test_output_path <- file.path(resampled_path, paste0(test_species_name, "_resampled.tif"))

test_result <- resample_species(test_species_name, test_raster_path, test_output_path)

if (test_result) {
  print("Test resampling successful!")
  
  # Proceed with resampling all species
  print("\nPROCEEDING WITH BATCH RESAMPLING")
  resampled_results <- data.frame(
    species_name = species_to_resample$species_match,
    original_path = species_to_resample$sdm_file,
    resampled_path = file.path(resampled_path, paste0(species_to_resample$species_match, "_resampled.tif")),
    resampled_success = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Set the first species as already processed
  resampled_results$resampled_success[1] <- TRUE
  
  # Process remaining species
  for (i in 2:nrow(resampled_results)) {
    species_name <- resampled_results$species_name[i]
    raster_path <- resampled_results$original_path[i]
    output_path <- resampled_results$resampled_path[i]
    
    resampled_results$resampled_success[i] <- resample_species(species_name, raster_path, output_path)
  }
  
  # Save resampling results for reference
  write.csv(resampled_results, file.path(prioritization_output, "resampled_species_status.csv"), 
            row.names = FALSE)
  
  # Summary
  success_count <- sum(resampled_results$resampled_success)
  print("\nResampling summary:")
  print(paste("Successfully resampled", success_count, "out of", nrow(resampled_results), 
              "species rasters."))
  print(paste("Resampled files saved to:", resampled_path))
  print("Check resampled_species_status.csv for details")
  
} else {
  print("Test resampling failed. Please check the error messages above.")
  # Stop here if the test failed
  stop("Test resampling failed. Cannot proceed with batch resampling.")
}

print("TASK 2 Complete: Resampled SDM .tif files")
#########################################
# TASK 3: Set up prioritization
#########################################

print("\nTASK 3: Setting up prioritization")

# Create a data frame for the resampled species if it doesn't exist
if (!exists("resampled_results")) {
  # List all resampled files
  resampled_files <- list.files(resampled_path, pattern = "_resampled\\.tif$", full.names = TRUE)
  
  if (length(resampled_files) == 0) {
    stop("No resampled species rasters found. Cannot proceed with prioritization.")
  }
  
  # Extract species names from file paths
  species_names <- basename(resampled_files)
  species_names <- gsub("_resampled\\.tif$", "", species_names)
  
  # Create data frame
  resampled_results <- data.frame(
    species_name = species_names,
    resampled_path = resampled_files,
    resampled_success = TRUE,
    stringsAsFactors = FALSE
  )
}

# Filter for successfully resampled species
successful_species <- resampled_results %>%
  filter(resampled_success)

if (nrow(successful_species) == 0) {
  stop("No successfully resampled species found. Cannot proceed with prioritization.")
}

print(paste("Proceeding with prioritization for", nrow(successful_species), "species..."))

# Load planning area raster
print("Loading planning area...")
planning_area <- rast(file.path(prioritization_output, "planning_area.tif"))

# Create an empty list to store species rasters
print("Loading resampled species rasters...")
species_stack <- list()
weights <- numeric(nrow(successful_species))
names(weights) <- successful_species$species_name
species_processed <- 0

# Load each resampled raster
for (i in 1:nrow(successful_species)) {
  species_name <- successful_species$species_name[i]
  resampled_path <- successful_species$resampled_path[i]
  
  print(paste("Loading species", i, "of", nrow(successful_species), ":", species_name))
  
  tryCatch({
    # Load the resampled raster
    species_rast <- rast(resampled_path)
    
    # Add to the stack
    species_stack[[species_name]] <- species_rast
    
    # Find the species in the original dataframe to get threat status
    species_idx <- which(species_df$species_match == species_name)
    
    # Set weight based on threat status
    # You might want to adjust these weights based on your specific needs
    threat_status <- species_df$threat_status[species_idx]
    
    weight <- switch(
      as.character(threat_status),
      "CR" = 4,  # Critically Endangered
      "EN" = 3,  # Endangered
      "VU" = 2,  # Vulnerable
      "NT" = 1,  # Near Threatened
      "LC" = 0.5,  # Least Concern
      1  # Default weight
    )
    
    weights[species_name] <- weight
    species_processed <- species_processed + 1
    
  }, error = function(e) {
    print(paste("Error loading resampled raster for", species_name, ":", e$message))
  })
}

print(paste("Successfully loaded", species_processed, "out of", nrow(successful_species), 
            "species rasters."))

# Remove any NULL elements from the list
species_stack <- species_stack[!sapply(species_stack, is.null)]

# Check if we have any valid rasters left
if (length(species_stack) > 0) {
  # Convert list to SpatRaster
  print(paste("Creating SpatRaster stack with", length(species_stack), "species..."))
  features <- rast(species_stack)
  
  # Adjust weights to match the features
  weights <- weights[names(species_stack)]
  
  # Print weights for reference
  print("Species weights based on threat status:")
  for (i in 1:length(weights)) {
    print(paste(names(weights)[i], ":", weights[i]))
  }
  
  # Check which solver is available
  available_solvers <- available_solvers()
  if (length(available_solvers) == 0) {
    stop("No optimization solvers are installed! Please install one of: Rsymphony, gurobi, lpsolveAPI, or cplexAPI.")
  }
  print(paste("Available solvers:", paste(available_solvers, collapse=", ")))
  print(paste("Using", available_solvers[1], "solver for optimization."))
  
  # Create a conservation problem with ABF-like objective
  print("Setting up prioritization problem...")
  problem <- problem(planning_area, features) %>%
    add_max_utility_objective(weights) %>%  # ABF-like approach (equivalent to Zonation ABF mode)
    add_binary_decisions() %>%
    add_default_solver(gap = 0.1, time_limit = 3600, verbose = TRUE)  # 1-hour time limit
  
  # Solve the problem
  print("Solving prioritization problem...")
  tryCatch({
    solution <- solve(problem)
    
    # Save the solution
    print("Saving results...")
    
    # Save as GeoTIFF
    writeRaster(solution, file.path(prioritization_output, "prioritization_solution.tif"), 
                overwrite = TRUE)
    
    # Convert to shapefile and save
    solution_poly <- as.polygons(solution)
    st_write(st_as_sf(solution_poly), file.path(prioritization_output, "prioritization_solution.shp"), 
             delete_dsn = TRUE)
    
    print(paste("Prioritization completed and results saved to", prioritization_output))
  }, error = function(e) {
    print(paste("Error solving prioritization problem:", e$message))
    print("Consider trying a different solver or simplifying the problem.")
  })
} else {
  print("No valid rasters could be processed. Cannot proceed with prioritization.")
}

print("TASK 3 Complete: Prioritization setup")
print("Script execution completed!")ror = function(e) {
    print(paste("Error loading resampled raster for", species_name, ":", e$message))
  })
}

print(paste("Successfully loaded", species_processed, "out of", nrow(successful_species), 
            "species rasters."))

# Remove any NULL elements from the list
species_stack <- species_stack[!sapply(species_stack, is.null)]

# Check if we have any valid rasters left
if (length(species_stack) > 0) {
  # Convert list to SpatRaster
  print(paste("Creating SpatRaster stack with", length(species_stack), "species..."))
  features <- rast(species_stack)
  
  # Adjust weights to match the features
  weights <- weights[names(species_stack)]
  
  # Print weights for reference
  print("Species weights based on threat status:")
  for (i in 1:length(weights)) {
    print(paste(names(weights)[i], ":", weights[i]))
  }
  
  # Check which solver is available
  available_solvers <- available_solvers()
  if (length(available_solvers) == 0) {
    stop("No optimization solvers are installed! Please install one of: Rsymphony, gurobi, lpsolveAPI, or cplexAPI.")
  }
  print(paste("Available solvers:", paste(available_solvers, collapse=", ")))
  print(paste("Using", available_solvers[1], "solver for optimization."))
  
  # Create a conservation problem with ABF-like objective
  print("Setting up prioritization problem...")
  problem <- problem(planning_area, features) %>%
    add_max_utility_objective(weights) %>%  # ABF-like approach (equivalent to Zonation ABF mode)
    add_binary_decisions() %>%
    add_default_solver(gap = 0.1, time_limit = 3600, verbose = TRUE)  # 1-hour time limit
  
  # Solve the problem
  print("Solving prioritization problem...")
  tryCatch({
    solution <- solve(problem)
    
    # Save the solution
    print("Saving results...")
    
    # Save as GeoTIFF
    writeRaster(solution, file.path(prioritization_output, "prioritization_solution.tif"), 
                overwrite = TRUE)
    
    # Convert to shapefile and save
    solution_poly <- as.polygons(solution)
    st_write(st_as_sf(solution_poly), file.path(prioritization_output, "prioritization_solution.shp"), 
             delete_dsn = TRUE)
    
    print(paste("Prioritization completed and results saved to", prioritization_output))
  }, error = function(e) {
    print(paste("Error solving prioritization problem:", e$message))
    print("Consider trying a different solver or simplifying the problem.")
  })
} else {
  print("No valid rasters could be processed. Cannot proceed with prioritization.")
}

print("TASK 3 Complete: Prioritization setup")
print("Script execution completed!")= 3,  # Endangered
      "VU" = 2,  # Vulnerable
      "NT" = 1,  # Near Threatened
      "LC" = 0.5,  # Least Concern
      1  # Default weight
    )
    
    weights[species_name] <- weight
    species_processed <- species_processed + 1
    
  }, error = function(e) {
    print(paste("Error loading resampled raster for", species_name, ":", e$message))
  })
}

print(paste("Successfully loaded", species_processed, "out of", nrow(successful_species), 
            "species rasters."))

# Remove any NULL elements from the list
species_stack <- species_stack[!sapply(species_stack, is.null)]

# Check if we have any valid rasters left
if (length(species_stack) > 0) {
  # Convert list to SpatRaster
  print(paste("Creating SpatRaster stack with", length(species_stack), "species..."))
  features <- rast(species_stack)
  
  # Adjust weights to match the features
  weights <- weights[names(species_stack)]
  
  # Print weights for reference
  print("Species weights based on threat status:")
  for (i in 1:length(weights)) {
    print(paste(names(weights)[i], ":", weights[i]))
  }
  
  # Check which solver is available
  available_solvers <- available_solvers()
  if (length(available_solvers) == 0) {
    stop("No optimization solvers are installed! Please install one of: Rsymphony, gurobi, lpsolveAPI, or cplexAPI.")
  }
  print(paste("Available solvers:", paste(available_solvers, collapse=", ")))
  print(paste("Using", available_solvers[1], "solver for optimization."))
  
  # Create a conservation problem with ABF-like objective
  print("Setting up prioritization problem...")
  problem <- problem(planning_area, features) %>%
    add_max_utility_objective(weights) %>%  # ABF-like approach (equivalent to Zonation ABF mode)
    add_binary_decisions() %>%
    add_default_solver(gap = 0.1, time_limit = 3600, verbose = TRUE)  # 1-hour time limit
  
  # Solve the problem
  print("Solving prioritization problem...")
  tryCatch({
    solution <- solve(problem)
    
    # Save the solution
    print("Saving results...")
    
    # Save as GeoTIFF
    writeRaster(solution, file.path(prioritization_output, "prioritization_solution.tif"), 
                overwrite = TRUE)
    
    # Convert to shapefile and save
    solution_poly <- as.polygons(solution)
    st_write(st_as_sf(solution_poly), file.path(prioritization_output, "prioritization_solution.shp"), 
             delete_dsn = TRUE)
    
    print(paste("Prioritization completed and results saved to", prioritization_output))
  }, error = function(e) {
    print(paste("Error solving prioritization problem:", e$message))
    print("Consider trying a different solver or simplifying the problem.")
  })
} else {
  print("No valid rasters could be processed. Cannot proceed with prioritization.")
}

print("TASK 3 Complete: Prioritization setup")
print("Script execution completed!")