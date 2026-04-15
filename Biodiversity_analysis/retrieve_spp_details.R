# =============================================================================
# Species Attribute Extraction — SNES 500-species Shapefiles
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: Loops over all shapefiles in QLD_100m_SNES_500spp/shapefiles/,
#          extracts CURRENT_NA, COMMON_NA, and THREATENED attributes from the
#          first non-empty feature in each file, and saves a species attribute
#          lookup table (species_attributes.csv).
#
# Input:  BESP_data_qld_2025/QLD_100m_SNES_500spp/shapefiles/
#           {N}.shp  (one per species; N = integer species ID)
#
# Output: BESP_data_qld_2025/QLD_100m_SNES_500spp/species_attributes.csv
#           columns: ShapefileID, SpeciesName, CommonName, ThreatStatus
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, readr, here)

source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# --- USER CONTROL ---
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

shapefile_dir <- paths$snes_shapefiles
output_file   <- paths$snes_attributes

if (!dir.exists(shapefile_dir)) {
  stop(
    "Shapefile directory not found:\n  ", shapefile_dir, "\n\n",
    "Expected: BESP_data_qld_2025/QLD_100m_SNES_500spp/shapefiles/"
  )
}

if (file.exists(output_file) && !overwrite_mode) {
  cat(">>> Output already exists (set overwrite_mode = TRUE to regenerate):\n  ",
      output_file, "\n")
  stop("Stopping — output exists.")
}

# =============================================================================
# 2. Extract Attributes
# =============================================================================

shapefiles <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE)
cat(">>> Found", length(shapefiles), "shapefiles in", shapefile_dir, "\n")

species_data <- data.frame(
  ShapefileID  = integer(),
  SpeciesName  = character(),
  CommonName   = character(),
  ThreatStatus = character(),
  stringsAsFactors = FALSE
)

for (shapefile in shapefiles) {
  file_name <- basename(shapefile)
  file_id   <- as.integer(tools::file_path_sans_ext(file_name))

  tryCatch({
    shape_data <- st_read(shapefile, quiet = TRUE)

    if (nrow(shape_data) > 0) {
      species_data <- rbind(species_data, data.frame(
        ShapefileID  = file_id,
        SpeciesName  = shape_data$CURRENT_NA[1],
        CommonName   = shape_data$COMMON_NA[1],
        ThreatStatus = shape_data$THREATENED[1],
        stringsAsFactors = FALSE
      ))

      if (!is.na(file_id) && file_id %% 50 == 0) {
        cat("  Processed file", file_id, "\n")
      }
    } else {
      warning(paste("Shapefile", file_name, "is empty. Skipping."))
    }
  }, error = function(e) {
    warning(paste("Error processing", file_name, ":", e$message))
  })
}

# =============================================================================
# 3. Save Output
# =============================================================================

species_data <- species_data %>% arrange(ShapefileID)

write_csv(species_data, output_file)
cat("✓ Species attributes saved to:", output_file, "\n")
cat("  Total species processed:", nrow(species_data), "\n")
cat("\n=== SPECIES ATTRIBUTE EXTRACTION COMPLETE ===\n")
