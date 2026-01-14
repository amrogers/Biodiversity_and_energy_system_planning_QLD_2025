# Script to select specific shapefiles, copy them to a new folder, and create a map
# with the species distributions (buffered) and CAPAD protected areas

# Load required libraries
library(sf)
library(ozmaps)
library(dplyr)
library(fs)
library(ggplot2)
library(glue) # For cleaner string formatting
library(ggrepel) # For better label placement

# --- Define Paths ---
source_folder <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/QLD_100m_SNES_500spp/shapefiles"
target_folder <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/zero_coverage"
capad_path <- "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/CAPAD_Terrestrial/CAPAD_QLD.shp"

# --- Create Target Folder ---
if (!dir.exists(target_folder)) {
  dir_create(target_folder)
  cat(glue("Created folder: {target_folder}\n"))
}

# --- List of Files to Select ---
file_ids <- c(3, 9, 52, 56, 145, 177, 194, 206, 208, 244, 263, 267, 269, 317, 332, 423, 459, 466, 471, 522, 524)
file_id_strings <- as.character(file_ids) # Ensure IDs are treated as strings

# --- Check Source Folder ---
if (!dir.exists(source_folder)) {
  stop(glue("Source folder doesn't exist: {source_folder}"))
}

# --- Copy Specified Shapefiles ---
cat("Files in source folder:\n")
source_files <- dir_ls(source_folder, glob = "*.shp")
print(source_files)

# --- Copy specified shapefiles to the target folder ---
cat("\nCopying files to target folder...\n")
copied_files <- character(0) # Keep track of copied files

for (id_str in file_id_strings) {
  files_to_copy <- dir_ls(source_folder, glob = glue("{id_str}.*"))
  
  if (length(files_to_copy) == 0) {
    cat(glue("Warning: No files found for ID {id_str}\n"))
    next
  }
  
  for (file in files_to_copy) {
    new_file <- file.path(target_folder, basename(file))
    file_copy(file, new_file, overwrite = TRUE)
    cat(glue("Copied: {basename(file)}\n"))
    if (endsWith(basename(file), ".shp")) {
      copied_files <- c(copied_files, new_file)
    }
  }
}

# --- Read Base Layers ---
qld <- ozmaps::ozmap_states %>%
  filter(NAME == "Queensland") %>%
  st_transform(3577) # GDA 1994 Australia Albers projection

capad <- st_read(capad_path) %>%
  st_transform(3577)

# --- Find Shapefiles to Process ---
cat("\nShapefiles in target folder after copying:\n")
shapefile_paths <- dir_ls(target_folder, glob = "*.shp")
print(shapefile_paths)

# --- If no files in target folder, read directly from source folder ---
if (length(shapefile_paths) == 0) {
  cat("\nNo shapefiles found in target folder. Trying to read directly from source folder.\n")
  
  shapefile_paths <- character(0)
  
  # Get shapefile paths for the IDs we want
  for (id_str in file_id_strings) {
    file_path <- file.path(source_folder, paste0(id_str, ".shp"))
    if (file.exists(file_path)) {
      shapefile_paths <- c(shapefile_paths, file_path)
      cat(glue("Found shapefile: {file_path}\n"))
    } else {
      cat(glue("Warning: Shapefile not found: {file_path}\n"))
    }
  }
  
  if (length(shapefile_paths) == 0) {
    stop("No shapefiles found in either target or source folder. Check file paths and IDs.")
  }
}

# --- Process Shapefiles: Buffer Creation ---
species_buffers <- list()
file_names_processed <- character(0) # To store processed file names

cat("\nProcessing these shapefiles:\n")
print(basename(shapefile_paths))

for (i in seq_along(shapefile_paths)) {
  file_path <- shapefile_paths[i]
  species_id <- tools::file_path_sans_ext(basename(file_path))
  
  cat(glue("Processing species ID: {species_id} from {basename(file_path)}\n"))
  
  tryCatch({
    # Read the shapefile
    shape <- st_read(file_path, quiet = TRUE)
    cat(glue("Shapefile {basename(file_path)} read successfully.\n"))
    
    # Transform to Albers projection if needed
    if (st_crs(shape) != 3577) {
      shape <- st_transform(shape, 3577)
      cat(glue("Shapefile {basename(file_path)} transformed to Albers projection.\n"))
    }
    
    # Convert polygons to centroids (points)
    points <- st_centroid(shape)
    cat(glue("Centroids calculated for {basename(file_path)}.\n"))
    
    # Add 10km buffer around points (10000 meters)
    buffer <- st_buffer(points, dist = 10000)
    cat(glue("Buffer created for {basename(file_path)}.\n"))
    
    # Create a new sf object with geometry and species_id
    buffer_sf <- st_sf(geometry = st_geometry(buffer), species_id = species_id)
    
    # Store in the list
    if (!is.null(buffer_sf) && (nrow(buffer_sf) > 0)) {
      species_buffers[[i]] <- buffer_sf
      file_names_processed <- c(file_names_processed, species_id) # Store filename
      cat(glue("Buffer_sf added to species_buffers for {basename(file_path)}.\n"))
    } else {
      cat(glue("Warning: Buffer_sf is empty or NULL for {basename(file_path)}. Skipping.\n"))
    }
    
  }, error = function(e) {
    cat(glue("Error processing {basename(file_path)}: {e$message}\n"))
  })
}

# --- Combine Buffers ---
# Remove any NULL elements from the list
species_buffers <- Filter(Negate(is.null), species_buffers)

if (length(species_buffers) == 0) {
  stop("No species buffers were successfully created. Check for errors during processing.")
}

all_buffers <- do.call(rbind, species_buffers)

cat(glue("\nCombined buffer has {nrow(all_buffers)} features\n"))
cat("Column names in the combined buffer dataset:\n")
print(names(all_buffers))

# --- Create Lookup Table ---
cat("\nCreating a lookup table for the map numbers...\n")
lookup_table <- data.frame(
  Map_Number = 1:length(file_names_processed),
  File_ID = file_names_processed,
  stringsAsFactors = FALSE
)

cat("\nLookup table for map numbers:\n")
print(lookup_table)


# --- Prepare for Mapping ---
# Add a sequential ID for labeling the map
all_buffers <- all_buffers %>%
  mutate(map_label = 1:n())

# Create label points (for connecting lines)
label_points <- st_centroid(all_buffers)

# Transform to WGS 84 (EPSG:4326) for the final map
all_buffers_wgs <- st_transform(all_buffers, 4326)
qld_wgs <- st_transform(qld, 4326)
capad_wgs <- st_transform(capad, 4326)
label_points_wgs <- st_transform(label_points, 4326) # Transform label points


# --- Create the Map ---
p <- ggplot() +
  # Add CAPAD layer with forest green color
  geom_sf(data = capad_wgs, fill = "forestgreen", color = NA, alpha = 0.5) +
  # Add species buffer areas with a blue color and some transparency
  geom_sf(data = all_buffers_wgs, fill = "steelblue", color = "darkblue", alpha = 0.7) +
  # Add Queensland border
  geom_sf(data = qld_wgs, fill = NA, color = "black", linewidth = 0.5) +
  # Add connecting lines
  geom_segment(
    data = st_as_sf(all_buffers_wgs), # use all_buffers_wgs
    aes(
      x = st_coordinates(st_centroid(geometry))[, "X"],
      y = st_coordinates(st_centroid(geometry))[, "Y"],
      xend = st_coordinates(label_points_wgs)[, "X"], # use label_points_wgs
      yend = st_coordinates(label_points_wgs)[, "Y"]
    ),
    color = "black",
    linewidth = 0.5
  ) +
  # Add labels using ggrepel
  ggrepel::geom_text_repel(
    data = as.data.frame(st_coordinates(label_points_wgs)), # Convert to dataframe
    aes(x = X, y = Y, label = all_buffers_wgs$map_label),
    size = 8, # Adjust this value to change the label size
    fontface = "bold",
    color = "black",
    box.padding = 0.5,       # Adjust padding as needed
    point.padding = 0.5,
    segment.color = "black",
    min.segment.length = 0,
    force = 0.1
  ) +
  # Remove title, axes, and labels
  labs(
    title = NULL,          # Remove title
    x = NULL, y = NULL
  ) +
  theme_void() +           # Use a void theme
  coord_sf(crs = 4326)

# Print the map
print(p)

# --- Save the map ---
ggsave(filename = file.path(target_folder, "species_distribution_map.png"), plot = p, width = 10, height = 12, dpi = 300)
cat(glue("Map saved as 'species_distribution_map.png' in the target folder: {target_folder}\n"))

# --- Save the Lookup Table ---
output_table_name <- file.path(target_folder, "map_number_lookup.csv")
write.csv(lookup_table, output_table_name, row.names = FALSE)
cat(glue("\nLookup table saved as 'map_number_lookup.csv' in the target folder: {target_folder}\n"))

