################################################################################
# LCOE and Exclusion Zone Analysis
# 
# Purpose: Create maps and statistics of Levelized Cost of Electricity (LCOE)
#          overlaid with biodiversity exclusion zones
# 
# Author: [Your Name]
# Date: 2025-12-08
# 
# Inputs:
#   - GDB file with CPA polygons
#   - CSV files with LCOE values
#   - Shapefile with exclusion zones
# 
# Outputs:
#   - Maps showing LCOE with exclusion zone overlays
#   - Histograms of LCOE distribution by exclusion zone
#   - Summary statistics CSV files
################################################################################

# Load required libraries ------------------------------------------------------
if (!require(pacman)) install.packages("pacman")
library(pacman)
p_load(sf, dplyr, ggplot2, ozmaps, gridExtra)

# Global Constants -------------------------------------------------------------
TARGET_CRS <- 4283  # GDA94 (standard Australian CRS)
EXCLUSION_ZONES <- c(30, 50, 70)
TECHNOLOGIES <- c("wind", "pv")
LCOE_THRESHOLD <- 0

# Color palettes - Purple to Yellow gradient for exclusion zones
ZONE_LCOE_COLORS <- list(
  "30" = colorRampPalette(c("#4A148C", "#7B1FA2", "#9C27B0", "#BA68C8", "#E1BEE7"))(100),  # Purple gradient
  "50" = colorRampPalette(c("#0D47A1", "#1976D2", "#2196F3", "#64B5F6", "#BBDEFB"))(100),  # Blue gradient
  "70" = colorRampPalette(c("#F57F17", "#FBC02D", "#FFEB3B", "#FFF59D", "#FFF9C4"))(100)   # Yellow gradient
)

# Outline colors for exclusion zones
EXCLUSION_OUTLINE_COLORS <- c(
  "30" = "#4A148C",  # Dark purple
  "50" = "#0D47A1",  # Dark blue
  "70" = "#F57F17"   # Dark yellow/gold
)

# Helper Functions -------------------------------------------------------------

#' Validate file paths
#' @param paths Named list of file paths to validate
#' @return TRUE if all paths exist, stops execution otherwise
validate_paths <- function(paths) {
  for (name in names(paths)) {
    if (!file.exists(paths[[name]]) && !dir.exists(paths[[name]])) {
      stop(sprintf("Path does not exist - %s: %s", name, paths[[name]]))
    }
  }
  cat("✓ All paths validated successfully\n")
  return(TRUE)
}

#' Transform spatial data to target CRS
#' @param sf_object sf object to transform
#' @param target_crs Target CRS code
#' @param make_valid Whether to fix invalid geometries
#' @return Transformed sf object
transform_to_crs <- function(sf_object, target_crs = TARGET_CRS, make_valid = TRUE) {
  if (is.na(st_crs(sf_object))) {
    warning("Input has no CRS defined. Assuming it's already in target CRS.")
    sf_object <- st_set_crs(sf_object, target_crs)
  } else if (st_crs(sf_object) != st_crs(target_crs)) {
    sf_object <- st_transform(sf_object, target_crs)
  }
  
  if (make_valid) {
    sf_object <- st_make_valid(sf_object)
  }
  
  return(sf_object)
}

#' Find matching files based on technology and threshold
#' @param gdb_path Path to geodatabase
#' @param csv_folder Path to CSV folder
#' @param technology Technology type (wind/pv)
#' @param threshold Threshold value
#' @return List with layer name and csv path
find_matching_files <- function(gdb_path, csv_folder, technology, threshold) {
  layer_pattern <- sprintf("%s.*_%d.*_B8_.*", tolower(technology), threshold)
  csv_pattern <- sprintf("%s.*_%d.*\\.csv$", tolower(technology), threshold)
  
  layers <- st_layers(gdb_path)
  matching_layers <- grep(layer_pattern, layers$name, value = TRUE, ignore.case = TRUE)
  matching_files <- list.files(csv_folder, pattern = csv_pattern, 
                               full.names = TRUE, ignore.case = TRUE)
  
  if (length(matching_layers) == 0 || length(matching_files) == 0) {
    stop(sprintf("No matching files found for %s at threshold %d", 
                 technology, threshold))
  }
  
  return(list(
    layer = matching_layers[1],
    csv = matching_files[1]
  ))
}

# Data Loading Functions -------------------------------------------------------

#' Load and prepare CPA polygons with LCOE data
#' @param gdb_path Path to geodatabase
#' @param csv_path Path to CSV file
#' @param layer_name Name of layer in geodatabase
#' @param simplify_tolerance Tolerance for geometry simplification
#' @return sf object with merged data in target CRS
load_lcoe_data <- function(gdb_path, csv_path, layer_name, 
                           simplify_tolerance = 1000) {
  cat("  Loading polygon data from GDB...\n")
  polygons <- st_read(gdb_path, layer = layer_name, quiet = TRUE)
  
  cat("  Simplifying geometries...\n")
  polygons <- st_simplify(polygons, dTolerance = simplify_tolerance, 
                          preserveTopology = TRUE)
  
  cat("  Loading LCOE data from CSV...\n")
  lcoe_data <- read.csv(csv_path)
  
  cat("  Joining polygon and LCOE data...\n")
  merged_data <- polygons %>%
    left_join(lcoe_data, by = "OIDcom")
  
  # Verify LCOE column exists
  if (!"LCOE_mwh_2060" %in% names(merged_data)) {
    stop("LCOE_mwh_2060 column not found in merged data")
  }
  
  cat("  Transforming to target CRS (EPSG:", TARGET_CRS, ")...\n")
  merged_data <- transform_to_crs(merged_data, TARGET_CRS)
  
  cat(sprintf("  ✓ Loaded %d features\n", nrow(merged_data)))
  return(merged_data)
}

#' Load and prepare exclusion zones
#' @param exclusion_path Path to exclusion zones shapefile
#' @param zones Vector of zone values to include
#' @return sf object with exclusion zones in target CRS
load_exclusion_zones <- function(exclusion_path, zones = EXCLUSION_ZONES) {
  cat("  Loading exclusion zones...\n")
  exclusion_zones <- st_read(exclusion_path, quiet = TRUE)
  
  cat("  Filtering to zones:", paste(zones, collapse = ", "), "\n")
  exclusion_zones <- exclusion_zones %>%
    filter(gridcode %in% zones) %>%
    mutate(gridcode = as.character(gridcode))
  
  cat("  Transforming to target CRS (EPSG:", TARGET_CRS, ")...\n")
  exclusion_zones <- transform_to_crs(exclusion_zones, TARGET_CRS)
  
  cat(sprintf("  ✓ Loaded %d exclusion zones\n", nrow(exclusion_zones)))
  return(exclusion_zones)
}

#' Load Queensland boundary
#' @return sf object with Queensland boundary in target CRS
load_qld_boundary <- function() {
  cat("  Loading Queensland boundary...\n")
  qld_boundary <- ozmap_states %>%
    filter(NAME == "Queensland")
  
  cat("  Transforming to target CRS (EPSG:", TARGET_CRS, ")...\n")
  qld_boundary <- transform_to_crs(qld_boundary, TARGET_CRS)
  
  cat("  ✓ Loaded Queensland boundary\n")
  return(qld_boundary)
}

# Analysis Functions -----------------------------------------------------------

#' Assign CPAs to exclusion zones using centroid-based spatial join
#' @param lcoe_data sf object with LCOE data
#' @param exclusion_zones sf object with exclusion zones
#' @param zones Vector of zone values to analyze
#' @return sf object with zone assignment
assign_to_exclusion_zones <- function(lcoe_data, exclusion_zones, 
                                      zones = EXCLUSION_ZONES) {
  cat("  Assigning CPAs to exclusion zones using centroids...\n")
  
  # Repair geometries before processing
  cat("    Repairing CPA geometries...\n")
  lcoe_data <- st_make_valid(lcoe_data)
  
  cat("    Repairing exclusion zone geometries...\n")
  exclusion_zones <- st_make_valid(exclusion_zones)
  
  # Calculate centroids of CPAs with error handling
  cat("    Calculating CPA centroids...\n")
  cpa_centroids <- tryCatch({
    st_centroid(lcoe_data)
  }, error = function(e) {
    cat("    Error with st_centroid, trying point_on_surface...\n")
    st_point_on_surface(lcoe_data)
  }, warning = function(w) {
    cat("    Warning with st_centroid, trying point_on_surface...\n")
    st_point_on_surface(lcoe_data)
  })
  
  # Spatial join - find which exclusion zone each centroid falls in
  cat("    Performing spatial join...\n")
  joined <- st_join(cpa_centroids, exclusion_zones, join = st_within)
  
  # Add the zone assignment back to the original CPA data
  lcoe_data$assigned_zone <- joined$gridcode
  
  # Summary
  zone_counts <- table(lcoe_data$assigned_zone, useNA = "ifany")
  cat("\n  Zone assignment summary:\n")
  for (z in names(zone_counts)) {
    if (is.na(z)) {
      cat(sprintf("    Outside zones: %d CPAs\n", zone_counts[z]))
    } else {
      cat(sprintf("    Zone %s: %d CPAs\n", z, zone_counts[z]))
    }
  }
  
  cat("  ✓ Zone assignment complete\n")
  return(lcoe_data)
}

#' Calculate LCOE statistics within exclusion zones
#' @param lcoe_data sf object with LCOE data and assigned zones
#' @param zones Vector of zone values to analyze
#' @return List of LCOE values by zone
calculate_zone_statistics <- function(lcoe_data, zones = EXCLUSION_ZONES) {
  cat("  Calculating LCOE statistics by exclusion zone...\n")
  lcoe_by_zone <- list()
  
  for (zone in as.character(zones)) {
    zone_data <- lcoe_data %>% filter(assigned_zone == zone)
    
    if (nrow(zone_data) > 0 && "LCOE_mwh_2060" %in% names(zone_data)) {
      lcoe_values <- zone_data$LCOE_mwh_2060[!is.na(zone_data$LCOE_mwh_2060)]
      
      if (length(lcoe_values) > 0) {
        lcoe_by_zone[[zone]] <- lcoe_values
        
        cat(sprintf("    Zone %s: %d CPAs, LCOE range: $%.2f - $%.2f/MWh\n", 
                    zone, length(lcoe_values),
                    min(lcoe_values), max(lcoe_values)))
      }
    } else {
      cat(sprintf("    Zone %s: No CPAs assigned\n", zone))
    }
  }
  
  cat(sprintf("  ✓ Calculated statistics for %d zones\n", length(lcoe_by_zone)))
  return(lcoe_by_zone)
}

#' Create summary statistics data frame
#' @param lcoe_by_zone List of LCOE values by zone
#' @param technology Technology name
#' @return Data frame with summary statistics
create_statistics_df <- function(lcoe_by_zone, technology) {
  stats_list <- lapply(names(lcoe_by_zone), function(zone) {
    if (length(lcoe_by_zone[[zone]]) > 0) {
      data.frame(
        Technology = toupper(technology),
        Exclusion_Zone = zone,
        Count = length(lcoe_by_zone[[zone]]),
        Mean_LCOE = mean(lcoe_by_zone[[zone]], na.rm = TRUE),
        Median_LCOE = median(lcoe_by_zone[[zone]], na.rm = TRUE),
        SD_LCOE = sd(lcoe_by_zone[[zone]], na.rm = TRUE),
        Min_LCOE = min(lcoe_by_zone[[zone]], na.rm = TRUE),
        Max_LCOE = max(lcoe_by_zone[[zone]], na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  })
  
  stats_df <- do.call(rbind, stats_list)
  return(stats_df)
}

# Visualization Functions ------------------------------------------------------

#' Create LCOE map with CPAs colored by exclusion zone gradient
#' @param lcoe_data sf object with LCOE data and assigned zones
#' @param qld_boundary sf object with Queensland boundary
#' @param technology Technology name
#' @return ggplot object
create_lcoe_map <- function(lcoe_data, qld_boundary, technology) {
  cat("  Creating map...\n")
  
  # Prepare data for each zone with zone-specific coloring
  lcoe_data$fill_color <- NA_character_
  
  for (zone in as.character(EXCLUSION_ZONES)) {
    # Filter CPAs in this zone
    zone_mask <- !is.na(lcoe_data$assigned_zone) & lcoe_data$assigned_zone == zone
    
    if (sum(zone_mask) > 0) {
      # Get LCOE values for this zone
      zone_lcoe <- lcoe_data$LCOE_mwh_2060[zone_mask]
      
      # Normalize LCOE to 0-1 within this zone (for color mapping)
      min_lcoe <- min(zone_lcoe, na.rm = TRUE)
      max_lcoe <- max(zone_lcoe, na.rm = TRUE)
      
      if (max_lcoe > min_lcoe) {
        normalized_lcoe <- (zone_lcoe - min_lcoe) / (max_lcoe - min_lcoe)
      } else {
        normalized_lcoe <- rep(0.5, length(zone_lcoe))
      }
      
      # Get zone color palette
      zone_colors <- ZONE_LCOE_COLORS[[zone]]
      
      # Map normalized LCOE to color indices (1-100)
      # Lower LCOE = index closer to 1 (darker), Higher LCOE = index closer to 100 (lighter)
      color_indices <- pmax(1, pmin(100, round(normalized_lcoe * 99) + 1))
      
      # Assign colors
      lcoe_data$fill_color[zone_mask] <- zone_colors[color_indices]
      
      cat(sprintf("    Zone %s: Colored %d CPAs (LCOE: $%.2f - $%.2f/MWh)\n",
                  zone, sum(zone_mask), min_lcoe, max_lcoe))
    }
  }
  
  # Handle CPAs outside all zones (if any)
  outside_mask <- is.na(lcoe_data$assigned_zone)
  if (sum(outside_mask) > 0) {
    lcoe_data$fill_color[outside_mask] <- "gray80"
    cat(sprintf("    Outside zones: %d CPAs colored gray\n", sum(outside_mask)))
  }
  
  # Create the plot
  p <- ggplot() +
    # CPAs colored by zone and LCOE
    geom_sf(data = lcoe_data, aes(fill = I(fill_color)), color = NA) +
    # Queensland boundary
    geom_sf(data = qld_boundary, 
            fill = NA, 
            color = "black", 
            linewidth = 1.0) +
    # Formatting
    ggtitle(sprintf("LCOE by Exclusion Zone - %s", toupper(technology))) +
    labs(subtitle = "Purple = Zone 30 | Blue = Zone 50 | Yellow = Zone 70\nDarker shades = Lower LCOE within each zone") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, lineheight = 1.2)
    )
  
  cat("  ✓ Map created\n")
  return(p)
}

#' Create faceted histogram of LCOE by exclusion zone
#' @param lcoe_by_zone List of LCOE values by zone
#' @return ggplot object
create_histograms <- function(lcoe_by_zone) {
  cat("  Creating faceted histogram...\n")
  
  # Prepare data frame for faceted plot
  hist_data_list <- list()
  
  for (zone in names(lcoe_by_zone)) {
    if (length(lcoe_by_zone[[zone]]) > 0) {
      hist_data_list[[zone]] <- data.frame(
        LCOE = lcoe_by_zone[[zone]],
        Zone = paste0("Zone ", zone),
        Zone_num = zone,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(hist_data_list) == 0) {
    cat("  ! No histogram data available\n")
    return(NULL)
  }
  
  # Combine all data
  hist_data <- do.call(rbind, hist_data_list)
  
  # Ensure proper factor ordering
  hist_data$Zone <- factor(hist_data$Zone, 
                           levels = paste0("Zone ", as.character(EXCLUSION_ZONES)))
  
  # Calculate breaks for histogram to determine colors
  n_bins <- 30
  
  # Create plot with gradient coloring
  p <- ggplot(hist_data, aes(x = LCOE)) +
    theme_minimal()
  
  # Add histograms for each zone with gradient colors
  for (zone in as.character(EXCLUSION_ZONES)) {
    zone_label <- paste0("Zone ", zone)
    zone_data <- hist_data %>% filter(Zone == zone_label)
    
    if (nrow(zone_data) > 0) {
      # Get the color gradient for this zone
      zone_colors <- ZONE_LCOE_COLORS[[zone]]
      
      # Calculate histogram breaks
      lcoe_range <- range(zone_data$LCOE, na.rm = TRUE)
      breaks <- seq(lcoe_range[1], lcoe_range[2], length.out = n_bins + 1)
      
      # Assign colors based on LCOE value (lower LCOE = darker color)
      zone_data$bin <- cut(zone_data$LCOE, breaks = breaks, 
                           include.lowest = TRUE, labels = FALSE)
      
      # Normalize bin indices to 0-1 for color selection
      max_bin <- max(zone_data$bin, na.rm = TRUE)
      zone_data$color_index <- pmax(1, pmin(100, 
                                            round((zone_data$bin / max_bin) * 99) + 1))
      zone_data$bar_color <- zone_colors[zone_data$color_index]
      
      p <- p + 
        geom_histogram(data = zone_data, 
                       aes(fill = ..x..), 
                       bins = n_bins, 
                       color = "black", 
                       linewidth = 0.3)
    }
  }
  
  # Add gradient fills for each zone
  p <- p +
    facet_wrap(~ Zone, ncol = 1, scales = "free_y") +
    scale_fill_gradientn(
      colors = c(
        ZONE_LCOE_COLORS[["30"]][20],   # Darker purple
        ZONE_LCOE_COLORS[["30"]][80],   # Lighter purple
        ZONE_LCOE_COLORS[["50"]][20],   # Darker blue
        ZONE_LCOE_COLORS[["50"]][80],   # Lighter blue
        ZONE_LCOE_COLORS[["70"]][20],   # Darker yellow
        ZONE_LCOE_COLORS[["70"]][80]    # Lighter yellow
      ),
      guide = "none"  # Hide the gradient legend
    ) +
    labs(
      title = "LCOE Distribution by Exclusion Zone",
      subtitle = "Darker bars = lower LCOE, Lighter bars = higher LCOE",
      x = "LCOE ($/MWh)",
      y = "Frequency"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      strip.text = element_text(face = "bold", size = 12),
      strip.background = element_rect(fill = "gray90", color = "black"),
      panel.grid.minor = element_blank()
    )
  
  cat("  ✓ Created faceted histogram\n")
  return(p)
}

# Output Functions -------------------------------------------------------------

#' Save all outputs for a technology
#' @param map_plot ggplot map object
#' @param histogram_plot ggplot histogram object
#' @param stats_df Statistics data frame
#' @param output_path Output directory path
#' @param technology Technology name
save_outputs <- function(map_plot, histogram_plot, stats_df, 
                         output_path, technology) {
  cat("  Saving outputs...\n")
  
  tech_lower <- tolower(technology)
  
  # Save map
  map_filename <- file.path(output_path, 
                            sprintf("LCOE_exclusion_map_%s.jpg", tech_lower))
  ggsave(filename = map_filename, plot = map_plot, 
         width = 12, height = 14, dpi = 300)
  cat("    ✓ Saved map:", map_filename, "\n")
  
  # Save histograms
  if (!is.null(histogram_plot)) {
    hist_filename <- file.path(output_path, 
                               sprintf("LCOE_histograms_%s.jpg", tech_lower))
    
    # Check if it's a ggplot object or a grob
    if (inherits(histogram_plot, "ggplot")) {
      ggsave(filename = hist_filename, plot = histogram_plot, 
             width = 10, height = 12, dpi = 300)
    } else {
      # If it's a grob (from grid.arrange), save it differently
      ggsave(filename = hist_filename, plot = histogram_plot, 
             width = 10, height = 12, dpi = 300)
    }
    cat("    ✓ Saved histograms:", hist_filename, "\n")
  } else {
    cat("    ! No histogram to save\n")
  }
  
  # Save statistics
  if (!is.null(stats_df) && nrow(stats_df) > 0) {
    stats_filename <- file.path(output_path, 
                                sprintf("LCOE_statistics_%s.csv", tech_lower))
    write.csv(stats_df, stats_filename, row.names = FALSE)
    cat("    ✓ Saved statistics:", stats_filename, "\n")
  }
  
  cat("  ✓ All outputs saved\n")
}

# Main Analysis Function -------------------------------------------------------

#' Main analysis function for a single technology
#' @param gdb_path Path to geodatabase
#' @param csv_folder Path to CSV folder
#' @param exclusion_path Path to exclusion zones shapefile
#' @param output_path Output directory
#' @param technology Technology type
#' @param threshold LCOE threshold value
#' @param simplify_tolerance Geometry simplification tolerance
#' @return List with results
analyze_technology <- function(gdb_path, csv_folder, exclusion_path, 
                               output_path, technology, 
                               threshold = LCOE_THRESHOLD, 
                               simplify_tolerance = 1000) {
  
  cat("\n", strrep("=", 70), "\n")
  cat("PROCESSING:", toupper(technology), "\n")
  cat(strrep("=", 70), "\n")
  
  # Find matching files
  cat("\n1. Finding matching files...\n")
  files <- find_matching_files(gdb_path, csv_folder, technology, threshold)
  cat("  Layer:", files$layer, "\n")
  cat("  CSV:", basename(files$csv), "\n")
  
  # Load data
  cat("\n2. Loading spatial data...\n")
  lcoe_data <- load_lcoe_data(gdb_path, files$csv, files$layer, simplify_tolerance)
  exclusion_zones <- load_exclusion_zones(exclusion_path)
  qld_boundary <- load_qld_boundary()
  
  # Verify all data is in same CRS
  cat("\n3. Verifying coordinate systems...\n")
  cat("  LCOE data CRS:", st_crs(lcoe_data)$input, "\n")
  cat("  Exclusion zones CRS:", st_crs(exclusion_zones)$input, "\n")
  cat("  QLD boundary CRS:", st_crs(qld_boundary)$input, "\n")
  
  if (st_crs(lcoe_data) != st_crs(exclusion_zones) || 
      st_crs(lcoe_data) != st_crs(qld_boundary)) {
    stop("CRS mismatch detected after transformation!")
  }
  cat("  ✓ All layers in same CRS\n")
  
  # Assign CPAs to exclusion zones based on majority overlap
  cat("\n4. Assigning CPAs to exclusion zones...\n")
  lcoe_data <- assign_to_exclusion_zones(lcoe_data, exclusion_zones)
  
  # Calculate statistics
  cat("\n5. Analyzing LCOE within exclusion zones...\n")
  lcoe_by_zone <- calculate_zone_statistics(lcoe_data)
  stats_df <- create_statistics_df(lcoe_by_zone, technology)
  
  # Create visualizations
  cat("\n6. Creating visualizations...\n")
  map_plot <- create_lcoe_map(lcoe_data, qld_boundary, technology)
  histogram_plot <- create_histograms(lcoe_by_zone)
  
  # Save outputs
  cat("\n7. Saving outputs...\n")
  save_outputs(map_plot, histogram_plot, stats_df, output_path, technology)
  
  cat("\n✓ COMPLETED:", toupper(technology), "\n")
  cat(strrep("=", 70), "\n")
  
  return(list(
    map = map_plot,
    histograms = histogram_plot,
    statistics = stats_df,
    lcoe_data = lcoe_data,
    exclusion_zones = exclusion_zones
  ))
}

################################################################################
# MAIN EXECUTION
################################################################################

main <- function() {
  cat("\n")
  cat(strrep("#", 78), "\n")
  cat("# LCOE AND EXCLUSION ZONE ANALYSIS\n")
  cat(strrep("#", 78), "\n\n")
  
  # Configuration --------------------------------------------------------------
  cat("CONFIGURATION\n")
  cat(strrep("-", 78), "\n")
  
  config <- list(
    gdb_path = "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/QLD_v202410_onshore_tx1.gdb",
    csv_folder = "Z:/NetZero_scenarios_outputs/QLD_v202410_onshore_tx1_02/model_outputs",
    exclusion_path = "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Boundless_data/QLD/zonation_results/Zonation_exclusion_thresholds/Zonation_exclusion_thresholds.shp",
    output_path = "C:/Users/andrewrogers/OneDrive - The University of Melbourne/Project_documents/results/supplementary data_biodiversity and energy system planning_qld_2025/Energy_system_model_outputs"
  )
  
  cat("GDB Path:", config$gdb_path, "\n")
  cat("CSV Folder:", config$csv_folder, "\n")
  cat("Exclusion Path:", config$exclusion_path, "\n")
  cat("Output Path:", config$output_path, "\n")
  cat("Target CRS: EPSG:", TARGET_CRS, "\n")
  cat("Technologies:", paste(TECHNOLOGIES, collapse = ", "), "\n")
  cat("Exclusion Zones:", paste(EXCLUSION_ZONES, collapse = ", "), "\n\n")
  
  # Validate paths
  validate_paths(config)
  
  # Create output directory
  dir.create(config$output_path, showWarnings = FALSE, recursive = TRUE)
  cat("✓ Output directory ready\n\n")
  
  # Run analysis for each technology -----------------------------------------
  results <- list()
  
  for (tech in TECHNOLOGIES) {
    tryCatch({
      results[[tech]] <- analyze_technology(
        gdb_path = config$gdb_path,
        csv_folder = config$csv_folder,
        exclusion_path = config$exclusion_path,
        output_path = config$output_path,
        technology = tech
      )
    }, error = function(e) {
      cat("\n!!! ERROR processing", toupper(tech), "!!!\n")
      cat("Error message:", e$message, "\n")
      cat(strrep("=", 70), "\n")
    })
  }
  
  # Combined output ----------------------------------------------------------
  if (length(results) > 0) {
    cat("\n")
    cat(strrep("=", 78), "\n")
    cat("CREATING COMBINED OUTPUTS\n")
    cat(strrep("=", 78), "\n\n")
    
    # Combine statistics from all technologies
    all_stats <- do.call(rbind, lapply(results, function(x) {
      if (!is.null(x) && !is.null(x$statistics)) x$statistics else NULL
    }))
    
    if (!is.null(all_stats) && nrow(all_stats) > 0) {
      cat("Summary Statistics:\n")
      print(all_stats)
      cat("\n")
      
      # Save combined statistics
      combined_stats_file <- file.path(config$output_path, 
                                       "LCOE_statistics_combined.csv")
      write.csv(all_stats, combined_stats_file, row.names = FALSE)
      cat("✓ Combined statistics saved to:", combined_stats_file, "\n")
    }
  }
  
  # Final summary ------------------------------------------------------------
  cat("\n")
  cat(strrep("#", 78), "\n")
  cat("# ANALYSIS COMPLETE\n")
  cat(strrep("#", 78), "\n")
  cat("\nOutput directory:", config$output_path, "\n")
  cat("Successful analyses:", length(results), "of", length(TECHNOLOGIES), "\n\n")
  
  return(results)
}

# Run the analysis -------------------------------------------------------------
if (interactive()) {
  results <- main()
}