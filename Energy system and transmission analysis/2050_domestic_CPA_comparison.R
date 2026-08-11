# =============================================================================
# Supplementary Table 2: TX1 vs TX2 Spatial Comparison Analysis
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Recovered and adapted for the reproducible pipeline Aug 2026
# =============================================================================
# Purpose: Analyze renewable energy deployment scenarios across transmission
#          scenarios (TX1, TX2) and biodiversity avoidance thresholds:
#            Step 1 - Overall spatial overlap between TX1 and TX2 (Jaccard index)
#            Step 2 - Technology-specific overlap (wind, solar_pv, offshore)
#            Step 3 - Wind-solar co-occurrence within each scenario
#            Step 4 - Visualisations (PNG)
#            Step 5 - Plain-text summary report
#
# Provenance: this analysis was originally run standalone in Oct 2025, writing
#             to Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs/
#             transmission_scenario_area_comparison/ (see analysis_summary.txt
#             there for the original run). The copy of the script committed to
#             this repo in Jan 2026 was an incomplete placeholder stub with no
#             working logic. A complete working copy was recovered from
#             Z:/NetZero_scenarios_outputs/Code/2050_domestic_CPA_comparison_
#             revised.R in Aug 2026 and adapted below to use _paths.R /
#             results/ instead of hardcoded Z: paths, so it runs from the
#             Figshare deposit without requiring access to that drive.
#
# Input:  BESP_data_qld_2025/Energy_system_model_outputs/
#           Electricity_Transmission_Lines/Tx_outputs/
#           domestic_tx1_shapefiles/combined_renewables_2050_threshold_{N}.shp
#           domestic_tx2_shapefiles/combined_renewables_2050_threshold_{N}.shp
#         (paths$tx_outputs in _paths.R; N = 0, 10, 30, 50, 70, 90)
#
# Output: results/transmission_scenario_comparison/
#           overall_comparison_stats.csv       -- Supplementary Table 2
#           technology_comparison_stats.csv
#           wind_solar_cooccurrence.csv
#           wind_solar_cooccurrence_comparison.csv
#           scenario_comparison_plots.png, technology_comparison.png,
#           wind_solar_cooccurrence_plots.png
#           analysis_summary.txt
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  sf,          # Spatial data handling
  dplyr,       # Data manipulation
  tidyr,       # Data tidying
  ggplot2,     # Visualization
  knitr,       # Tables
  scales,      # Formatting
  viridis,     # Color palettes
  gridExtra,   # Plot arrangement
  units,       # Unit handling
  lwgeom,      # Advanced geometry operations
  ozmaps,      # Australia maps
  here
)

# Turn off s2 processing to avoid geometry errors (degenerate/duplicate
# vertices in the source shapefiles fail s2's strict validity checks during
# st_union(), even after st_make_valid() -- same fix used in
# NZAU2_QLD_mapping.R for the same class of input data).
sf::sf_use_s2(FALSE)

source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# --- File Paths ---
base_path   <- paths$tx_outputs
output_path <- here("results", "transmission_scenario_comparison")

# --- Step-Specific Overwrite Flags ---
# Set to TRUE to force re-running and overwriting outputs for each step
OVERWRITE_STEP1_SPATIAL_ANALYSIS <- FALSE     # Overall TX1 vs TX2 comparison
OVERWRITE_STEP2_TECH_OVERLAP     <- FALSE     # Technology-specific TX1 vs TX2
OVERWRITE_STEP3_COOCCURRENCE     <- FALSE     # Wind-solar co-occurrence within scenarios
OVERWRITE_STEP4_VISUALIZATION    <- FALSE     # Plots and charts
OVERWRITE_STEP5_MAPS             <- FALSE     # Spatial maps

# --- Analysis Parameters ---
TARGET_CRS <- 4326          # WGS 84 for mapping
MAP_DPI    <- 300           # Map resolution (reduced from 600 -- matches other Figure_code scripts)

# Define scenarios and thresholds
scenarios <- c("tx1", "tx2")
thresholds <- c(0, 10, 30, 50, 70, 90)

# Technologies to analyze
technologies <- c("wind", "solar_pv", "offshore")

# --- Color Schemes ---
CUSTOM_TECH_COLORS <- list(
  "solar_pv" = c(tx1 = "#FFEB99", tx2 = "#FFC266", overlap = "#CC6600"),
  "wind" = c(tx1 = "#99CCFF", tx2 = "#66B2B2", overlap = "#004C99"),
  "offshore" = c(tx1 = "#E0B3FF", tx2 = "#B366FF", overlap = "#6600CC"),
  "other" = c(tx1 = "#AAAAAA", tx2 = "#777777", overlap = "#444444")
)

# Create output directory
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# ==============================================================================
# UTILITY FUNCTIONS
# ==============================================================================

#' Log a message with timestamp
#' @param msg Message to log
#' @param type Type of message ("INFO", "SUCCESS", "WARNING", "ERROR")
log_message <- function(msg, type = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  prefix <- switch(type,
                   "INFO" = "\u2139",
                   "SUCCESS" = "\u2705",
                   "WARNING" = "\u26a0",
                   "ERROR" = "\u274c",
                   "\u2192")
  cat(sprintf("[%s] %s %s\n", timestamp, prefix, msg))
}

#' Check if all files in a list exist
#' @param filepaths Vector of file paths
#' @return TRUE if all exist, FALSE otherwise
all_files_exist <- function(filepaths) {
  all(file.exists(filepaths))
}

#' Safely make geometry valid with error handling
#' @param geom sf geometry object
#' @return Valid geometry or NULL on error
make_valid_safe <- function(geom) {
  tryCatch({
    st_make_valid(geom)
  }, error = function(e) {
    log_message(sprintf("Error making geometry valid: %s", e$message), "WARNING")
    return(NULL)
  })
}

# ==============================================================================
# DATA LOADING FUNCTIONS
# ==============================================================================

#' Read shapefile for a specific scenario and threshold
#' @param scenario Transmission scenario ("tx1" or "tx2")
#' @param threshold Biodiversity avoidance threshold (0-90)
#' @param base_path Base directory path
#' @param target_crs Target coordinate reference system
#' @return sf object or NULL if file not found
read_scenario_shapefile <- function(scenario, threshold, base_path, target_crs) {
  file_path <- file.path(
    base_path,
    sprintf("domestic_%s_shapefiles", scenario),
    sprintf("combined_renewables_2050_threshold_%d.shp", threshold)
  )

  if (!file.exists(file_path)) {
    log_message(sprintf("File not found: %s", file_path), "WARNING")
    return(NULL)
  }

  tryCatch({
    shp <- st_read(file_path, quiet = TRUE) %>%
      st_transform(target_crs) %>%
      mutate(
        scenario = scenario,
        threshold = threshold
      )

    log_message(sprintf("Loaded %s, threshold %d: %d features",
                        scenario, threshold, nrow(shp)), "INFO")
    return(shp)
  }, error = function(e) {
    log_message(sprintf("Error reading %s: %s", file_path, e$message), "ERROR")
    return(NULL)
  })
}

# ==============================================================================
# STEP 1: OVERALL SPATIAL ANALYSIS (TX1 vs TX2)
# ==============================================================================

#' Calculate overall overlap statistics between two scenarios
#' @param shp1 sf object for scenario 1
#' @param shp2 sf object for scenario 2
#' @param threshold Biodiversity threshold
#' @return Data frame with overlap statistics
calculate_overall_overlap <- function(shp1, shp2, threshold) {

  # Dissolve and validate geometries
  shp1_dissolved <- st_union(make_valid_safe(shp1))
  shp2_dissolved <- st_union(make_valid_safe(shp2))

  if (is.null(shp1_dissolved) || is.null(shp2_dissolved)) {
    log_message("Invalid geometries after dissolving", "WARNING")
    return(NULL)
  }

  # Calculate areas (km²)
  area1 <- as.numeric(st_area(shp1_dissolved)) / 1e6
  area2 <- as.numeric(st_area(shp2_dissolved)) / 1e6

  # Calculate intersection
  intersection <- tryCatch({
    st_intersection(shp1_dissolved, shp2_dissolved)
  }, error = function(e) {
    log_message(sprintf("Intersection error: %s", e$message), "WARNING")
    return(NULL)
  })

  area_overlap <- if (!is.null(intersection) && length(intersection) > 0) {
    as.numeric(st_area(intersection)) / 1e6
  } else {
    0
  }

  # Calculate metrics
  area1_unique <- area1 - area_overlap
  area2_unique <- area2 - area_overlap
  overlap_pct1 <- (area_overlap / area1) * 100
  overlap_pct2 <- (area_overlap / area2) * 100
  area_union <- area1 + area2 - area_overlap
  jaccard_index <- area_overlap / area_union

  return(data.frame(
    threshold = threshold,
    area_tx1_total = area1,
    area_tx2_total = area2,
    area_overlap = area_overlap,
    area_tx1_unique = area1_unique,
    area_tx2_unique = area2_unique,
    overlap_pct_of_tx1 = overlap_pct1,
    overlap_pct_of_tx2 = overlap_pct2,
    jaccard_index = jaccard_index,
    area_difference = area2 - area1,
    area_change_pct = ((area2 - area1) / area1) * 100
  ))
}

#' Run Step 1: Overall spatial analysis
run_step1_overall_analysis <- function() {

  cat("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\n")
  cat("\u2551           STEP 1: OVERALL SPATIAL ANALYSIS                 \u2551\n")
  cat("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\n\n")

  output_file <- file.path(output_path, "overall_comparison_stats.csv")

  if (!OVERWRITE_STEP1_SPATIAL_ANALYSIS && file.exists(output_file)) {
    log_message("Step 1 outputs exist. Skipping.", "SUCCESS")
    return(read.csv(output_file))
  }

  log_message("Running Step 1: Overall Spatial Analysis", "INFO")

  overall_results <- list()

  for (thresh in thresholds) {
    log_message(sprintf("Processing threshold %d%%", thresh), "INFO")

    shp_tx1 <- read_scenario_shapefile("tx1", thresh, base_path, TARGET_CRS)
    shp_tx2 <- read_scenario_shapefile("tx2", thresh, base_path, TARGET_CRS)

    if (is.null(shp_tx1) || is.null(shp_tx2)) {
      log_message(sprintf("Missing data for threshold %d. Skipping.", thresh), "WARNING")
      next
    }

    overlap_stats <- calculate_overall_overlap(shp_tx1, shp_tx2, thresh)

    if (!is.null(overlap_stats)) {
      overall_results[[as.character(thresh)]] <- overlap_stats
    }
  }

  overall_results_df <- bind_rows(overall_results)

  # Save results
  write.csv(overall_results_df, output_file, row.names = FALSE)
  log_message(sprintf("Saved: %s", output_file), "SUCCESS")

  return(overall_results_df)
}

# ==============================================================================
# STEP 2: TECHNOLOGY-SPECIFIC OVERLAP ANALYSIS (TX1 vs TX2)
# ==============================================================================

#' Calculate technology-specific overlap between scenarios
#' @param shp1 sf object for scenario 1
#' @param shp2 sf object for scenario 2
#' @param threshold Biodiversity threshold
#' @return Data frame with technology-specific statistics
calculate_tech_overlap <- function(shp1, shp2, threshold) {

  all_techs <- unique(c(shp1$technology, shp2$technology))
  tech_stats <- list()

  for (tech in all_techs) {
    tech1 <- shp1 %>% filter(technology == tech)
    tech2 <- shp2 %>% filter(technology == tech)

    if (nrow(tech1) == 0 && nrow(tech2) == 0) next

    # Dissolve by technology
    tech1_dissolved <- if (nrow(tech1) > 0) {
      st_union(make_valid_safe(tech1))
    } else {
      NULL
    }

    tech2_dissolved <- if (nrow(tech2) > 0) {
      st_union(make_valid_safe(tech2))
    } else {
      NULL
    }

    # Calculate areas
    area1 <- if (!is.null(tech1_dissolved)) {
      as.numeric(st_area(tech1_dissolved)) / 1e6
    } else {
      0
    }

    area2 <- if (!is.null(tech2_dissolved)) {
      as.numeric(st_area(tech2_dissolved)) / 1e6
    } else {
      0
    }

    # Calculate overlap
    overlap_area <- 0
    if (!is.null(tech1_dissolved) && !is.null(tech2_dissolved)) {
      intersection <- tryCatch({
        st_intersection(tech1_dissolved, tech2_dissolved)
      }, error = function(e) NULL)

      overlap_area <- if (!is.null(intersection) && length(intersection) > 0) {
        as.numeric(st_area(intersection)) / 1e6
      } else {
        0
      }
    }

    tech_stats[[tech]] <- data.frame(
      threshold = threshold,
      technology = tech,
      area_tx1_total = area1,
      area_tx2_total = area2,
      area_overlap = overlap_area,
      area_tx1_unique = area1 - overlap_area,
      area_tx2_unique = area2 - overlap_area,
      overlap_pct_of_tx1 = if (area1 > 0) (overlap_area / area1) * 100 else 0,
      overlap_pct_of_tx2 = if (area2 > 0) (overlap_area / area2) * 100 else 0,
      jaccard_index = if ((area1 + area2 - overlap_area) > 0) {
        overlap_area / (area1 + area2 - overlap_area)
      } else {
        0
      }
    )
  }

  return(bind_rows(tech_stats))
}

#' Run Step 2: Technology-specific overlap analysis
run_step2_tech_overlap <- function() {

  cat("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\n")
  cat("\u2551      STEP 2: TECHNOLOGY-SPECIFIC OVERLAP ANALYSIS          \u2551\n")
  cat("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\n\n")

  output_file <- file.path(output_path, "technology_comparison_stats.csv")

  if (!OVERWRITE_STEP2_TECH_OVERLAP && file.exists(output_file)) {
    log_message("Step 2 outputs exist. Skipping.", "SUCCESS")
    return(read.csv(output_file))
  }

  log_message("Running Step 2: Technology-Specific Overlap", "INFO")

  tech_results <- list()

  for (thresh in thresholds) {
    log_message(sprintf("Processing threshold %d%%", thresh), "INFO")

    shp_tx1 <- read_scenario_shapefile("tx1", thresh, base_path, TARGET_CRS)
    shp_tx2 <- read_scenario_shapefile("tx2", thresh, base_path, TARGET_CRS)

    if (is.null(shp_tx1) || is.null(shp_tx2)) {
      log_message(sprintf("Missing data for threshold %d. Skipping.", thresh), "WARNING")
      next
    }

    tech_overlap <- calculate_tech_overlap(shp_tx1, shp_tx2, thresh)

    if (!is.null(tech_overlap) && nrow(tech_overlap) > 0) {
      tech_results[[as.character(thresh)]] <- tech_overlap
    }
  }

  tech_results_df <- bind_rows(tech_results)

  # Save results
  write.csv(tech_results_df, output_file, row.names = FALSE)
  log_message(sprintf("Saved: %s", output_file), "SUCCESS")

  return(tech_results_df)
}

# ==============================================================================
# STEP 3: WIND-SOLAR CO-OCCURRENCE ANALYSIS
# ==============================================================================

#' Calculate wind-solar co-occurrence within a single scenario
#' @param shp sf object containing wind and solar features
#' @param scenario Scenario name ("tx1" or "tx2")
#' @param threshold Biodiversity threshold
#' @return Data frame with co-occurrence statistics
calculate_wind_solar_cooccurrence <- function(shp, scenario, threshold) {

  # Filter for wind and solar technologies
  wind <- shp %>% filter(technology == "wind")
  solar <- shp %>% filter(technology == "solar_pv")

  if (nrow(wind) == 0 || nrow(solar) == 0) {
    log_message(sprintf("Missing wind or solar data for %s, threshold %d",
                        scenario, threshold), "WARNING")
    return(NULL)
  }

  # Dissolve by technology
  wind_dissolved <- st_union(make_valid_safe(wind))
  solar_dissolved <- st_union(make_valid_safe(solar))

  if (is.null(wind_dissolved) || is.null(solar_dissolved)) {
    log_message("Invalid geometries after dissolving", "WARNING")
    return(NULL)
  }

  # Calculate areas (km²)
  area_wind <- as.numeric(st_area(wind_dissolved)) / 1e6
  area_solar <- as.numeric(st_area(solar_dissolved)) / 1e6

  # Calculate intersection (co-occurrence)
  intersection <- tryCatch({
    st_intersection(wind_dissolved, solar_dissolved)
  }, error = function(e) {
    log_message(sprintf("Intersection error: %s", e$message), "WARNING")
    return(NULL)
  })

  area_cooccurrence <- if (!is.null(intersection) && length(intersection) > 0) {
    as.numeric(st_area(intersection)) / 1e6
  } else {
    0
  }

  # Calculate unique areas
  area_wind_only <- area_wind - area_cooccurrence
  area_solar_only <- area_solar - area_cooccurrence

  # Calculate percentages
  pct_wind_colocated <- (area_cooccurrence / area_wind) * 100
  pct_solar_colocated <- (area_cooccurrence / area_solar) * 100

  # Calculate Jaccard Index
  area_union <- area_wind + area_solar - area_cooccurrence
  jaccard_index <- area_cooccurrence / area_union

  return(data.frame(
    scenario = scenario,
    threshold = threshold,
    area_wind_total = area_wind,
    area_solar_total = area_solar,
    area_cooccurrence = area_cooccurrence,
    area_wind_only = area_wind_only,
    area_solar_only = area_solar_only,
    pct_wind_colocated = pct_wind_colocated,
    pct_solar_colocated = pct_solar_colocated,
    jaccard_index = jaccard_index
  ))
}

#' Run Step 3: Wind-solar co-occurrence analysis
run_step3_cooccurrence <- function() {

  cat("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\n")
  cat("\u2551        STEP 3: WIND-SOLAR CO-OCCURRENCE ANALYSIS           \u2551\n")
  cat("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\n\n")

  output_file <- file.path(output_path, "wind_solar_cooccurrence.csv")

  if (!OVERWRITE_STEP3_COOCCURRENCE && file.exists(output_file)) {
    log_message("Step 3 outputs exist. Skipping.", "SUCCESS")
    return(read.csv(output_file))
  }

  log_message("Running Step 3: Wind-Solar Co-occurrence Analysis", "INFO")

  cooccurrence_results <- list()

  for (scenario in scenarios) {
    log_message(sprintf("Analyzing scenario: %s", toupper(scenario)), "INFO")

    for (thresh in thresholds) {
      log_message(sprintf("  Processing threshold %d%%", thresh), "INFO")

      shp <- read_scenario_shapefile(scenario, thresh, base_path, TARGET_CRS)

      if (is.null(shp)) {
        log_message(sprintf("  Missing data for %s, threshold %d",
                            scenario, thresh), "WARNING")
        next
      }

      cooccurrence <- calculate_wind_solar_cooccurrence(shp, scenario, thresh)

      if (!is.null(cooccurrence)) {
        cooccurrence_results[[sprintf("%s_%d", scenario, thresh)]] <- cooccurrence
      }
    }
  }

  cooccurrence_df <- bind_rows(cooccurrence_results)

  # Save results
  write.csv(cooccurrence_df, output_file, row.names = FALSE)
  log_message(sprintf("Saved: %s", output_file), "SUCCESS")

  # Create comparison summary
  if (nrow(cooccurrence_df) == 0) {
    log_message("No co-occurrence data for any scenario/threshold -- skipping comparison summary.", "WARNING")
    return(cooccurrence_df)
  }

  comparison_summary <- cooccurrence_df %>%
    group_by(threshold) %>%
    summarise(
      tx1_cooccurrence = area_cooccurrence[scenario == "tx1"],
      tx2_cooccurrence = area_cooccurrence[scenario == "tx2"],
      tx1_jaccard = jaccard_index[scenario == "tx1"],
      tx2_jaccard = jaccard_index[scenario == "tx2"],
      tx1_pct_wind_colocated = pct_wind_colocated[scenario == "tx1"],
      tx2_pct_wind_colocated = pct_wind_colocated[scenario == "tx2"],
      tx1_pct_solar_colocated = pct_solar_colocated[scenario == "tx1"],
      tx2_pct_solar_colocated = pct_solar_colocated[scenario == "tx2"]
    )

  comparison_file <- file.path(output_path, "wind_solar_cooccurrence_comparison.csv")
  write.csv(comparison_summary, comparison_file, row.names = FALSE)
  log_message(sprintf("Saved comparison: %s", comparison_file), "SUCCESS")

  return(cooccurrence_df)
}

# ==============================================================================
# STEP 4: VISUALIZATION
# ==============================================================================

#' Create comprehensive visualizations
run_step4_visualization <- function(overall_results, tech_results, cooccurrence_results) {

  cat("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\n")
  cat("\u2551              STEP 4: VISUALIZATION                         \u2551\n")
  cat("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\n\n")

  viz_files <- c(
    "scenario_comparison_plots.png",
    "technology_comparison.png",
    "wind_solar_cooccurrence_plots.png"
  )

  viz_paths <- file.path(output_path, viz_files)

  if (!OVERWRITE_STEP4_VISUALIZATION && all_files_exist(viz_paths)) {
    log_message("Step 4 outputs exist. Skipping.", "SUCCESS")
    return(invisible(NULL))
  }

  log_message("Running Step 4: Creating Visualizations", "INFO")

  # --- Plot 1: Overall Scenario Comparison ---
  log_message("Creating overall comparison plots", "INFO")

  p1 <- ggplot(overall_results, aes(x = threshold)) +
    geom_line(aes(y = area_tx1_total, color = "TX1"), linewidth = 1.2) +
    geom_line(aes(y = area_tx2_total, color = "TX2"), linewidth = 1.2) +
    geom_point(aes(y = area_tx1_total, color = "TX1"), size = 3) +
    geom_point(aes(y = area_tx2_total, color = "TX2"), size = 3) +
    scale_color_manual(values = c("TX1" = "#2E86AB", "TX2" = "#A23B72")) +
    labs(title = "Total Area by Threshold",
         subtitle = "Comparison of TX1 and TX2 scenarios",
         x = "Biodiversity Threshold (%)",
         y = "Total Area (km²)",
         color = "Scenario") +
    theme_minimal(base_size = 12)

  p2 <- ggplot(overall_results, aes(x = threshold, y = area_overlap)) +
    geom_line(linewidth = 1.5, color = "#006E90") +
    geom_point(size = 3, color = "#006E90") +
    geom_ribbon(aes(ymin = 0, ymax = area_overlap), alpha = 0.2, fill = "#006E90") +
    labs(title = "Spatial Overlap Between Scenarios",
         x = "Biodiversity Threshold (%)",
         y = "Overlap Area (km²)") +
    theme_minimal(base_size = 12)

  p3 <- ggplot(overall_results, aes(x = threshold, y = jaccard_index)) +
    geom_line(linewidth = 1.5, color = "#F18F01") +
    geom_point(size = 3, color = "#F18F01") +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    labs(title = "Jaccard Similarity Index",
         subtitle = "Higher values = greater spatial overlap",
         x = "Biodiversity Threshold (%)",
         y = "Jaccard Index") +
    ylim(0, 1) +
    theme_minimal(base_size = 12)

  p4 <- ggplot(overall_results, aes(x = threshold, y = area_change_pct)) +
    geom_bar(stat = "identity", fill = "#C73E1D", alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid") +
    labs(title = "Area Change from TX1 to TX2",
         x = "Biodiversity Threshold (%)",
         y = "Change (%)") +
    theme_minimal(base_size = 12)

  combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)
  ggsave(viz_paths[1], combined_plot, width = 14, height = 10, dpi = MAP_DPI)
  log_message(sprintf("Saved: %s", viz_paths[1]), "SUCCESS")

  # --- Plot 2: Technology Comparison ---
  if (nrow(tech_results) > 0) {
    log_message("Creating technology comparison plot", "INFO")

    p5 <- tech_results %>%
      select(threshold, technology, area_tx1_total, area_tx2_total) %>%
      pivot_longer(cols = c(area_tx1_total, area_tx2_total),
                   names_to = "scenario",
                   values_to = "area") %>%
      mutate(scenario = gsub("area_|_total", "", scenario)) %>%
      ggplot(aes(x = threshold, y = area, color = scenario, linetype = technology)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("tx1" = "#2E86AB", "tx2" = "#A23B72")) +
      labs(title = "Area Comparison by Technology",
           x = "Biodiversity Threshold (%)",
           y = "Area (km²)",
           color = "Scenario",
           linetype = "Technology") +
      theme_minimal(base_size = 12)

    ggsave(viz_paths[2], p5, width = 12, height = 6, dpi = MAP_DPI)
    log_message(sprintf("Saved: %s", viz_paths[2]), "SUCCESS")
  }

  # --- Plot 3: Wind-Solar Co-occurrence ---
  if (!is.null(cooccurrence_results) && nrow(cooccurrence_results) > 0) {
    log_message("Creating wind-solar co-occurrence plots", "INFO")

    p6 <- ggplot(cooccurrence_results,
                 aes(x = threshold, y = area_cooccurrence, color = scenario)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("tx1" = "#2E86AB", "tx2" = "#A23B72")) +
      labs(title = "Wind-Solar Co-occurrence Area",
           subtitle = "Area where wind and solar deployment overlap",
           x = "Biodiversity Threshold (%)",
           y = "Co-occurrence Area (km²)",
           color = "Scenario") +
      theme_minimal(base_size = 12)

    p7 <- cooccurrence_results %>%
      select(scenario, threshold, pct_wind_colocated, pct_solar_colocated) %>%
      pivot_longer(cols = c(pct_wind_colocated, pct_solar_colocated),
                   names_to = "technology",
                   values_to = "pct_colocated") %>%
      mutate(technology = gsub("pct_|_colocated", "", technology)) %>%
      ggplot(aes(x = threshold, y = pct_colocated,
                 color = scenario, linetype = technology)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("tx1" = "#2E86AB", "tx2" = "#A23B72")) +
      labs(title = "Percentage of Technology Co-located",
           subtitle = "% of wind/solar that overlaps with the other technology",
           x = "Biodiversity Threshold (%)",
           y = "% Co-located",
           color = "Scenario",
           linetype = "Technology") +
      theme_minimal(base_size = 12)

    p8 <- ggplot(cooccurrence_results,
                 aes(x = threshold, y = jaccard_index, color = scenario)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("tx1" = "#2E86AB", "tx2" = "#A23B72")) +
      geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
      labs(title = "Wind-Solar Jaccard Index",
           subtitle = "Similarity of wind and solar spatial distributions",
           x = "Biodiversity Threshold (%)",
           y = "Jaccard Index",
           color = "Scenario") +
      ylim(0, 1) +
      theme_minimal(base_size = 12)

    # Create comparison plot
    p9 <- cooccurrence_results %>%
      group_by(threshold) %>%
      summarise(
        diff_cooccurrence = area_cooccurrence[scenario == "tx2"] -
          area_cooccurrence[scenario == "tx1"]
      ) %>%
      ggplot(aes(x = threshold, y = diff_cooccurrence)) +
      geom_bar(stat = "identity", fill = "#F18F01", alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "solid") +
      labs(title = "Change in Co-occurrence: TX2 - TX1",
           x = "Biodiversity Threshold (%)",
           y = "Difference in Co-occurrence Area (km²)") +
      theme_minimal(base_size = 12)

    cooccurrence_combined <- grid.arrange(p6, p7, p8, p9, ncol = 2)
    ggsave(viz_paths[3], cooccurrence_combined, width = 14, height = 10, dpi = MAP_DPI)
    log_message(sprintf("Saved: %s", viz_paths[3]), "SUCCESS")
  }

  return(invisible(NULL))
}

# ==============================================================================
# STEP 5: SUMMARY REPORT
# ==============================================================================

#' Generate comprehensive text summary
create_summary_report <- function(overall_results, tech_results, cooccurrence_results) {

  log_message("Generating summary report", "INFO")

  summary_file <- file.path(output_path, "analysis_summary.txt")

  sink(summary_file)

  cat("\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\n")
  cat("\u2551        NET ZERO AUSTRALIA - ANALYSIS SUMMARY               \u2551\n")
  cat("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\n\n")

  cat(sprintf("Analysis Date: %s\n", Sys.Date()))
  cat(sprintf("Thresholds Analyzed: %s\n\n",
              paste(thresholds, collapse = ", ")))

  # --- Overall Scenario Comparison ---
  cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n")
  cat("1. OVERALL SCENARIO COMPARISON (TX1 vs TX2)\n")
  cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n\n")

  for (i in 1:nrow(overall_results)) {
    row <- overall_results[i, ]
    cat(sprintf("Threshold %d%%:\n", row$threshold))
    cat(sprintf("  TX1 Total Area:    %10.2f km²\n", row$area_tx1_total))
    cat(sprintf("  TX2 Total Area:    %10.2f km²\n", row$area_tx2_total))
    cat(sprintf("  Overlap Area:      %10.2f km² (%.1f%% of TX1, %.1f%% of TX2)\n",
                row$area_overlap, row$overlap_pct_of_tx1, row$overlap_pct_of_tx2))
    cat(sprintf("  Jaccard Index:     %10.3f\n", row$jaccard_index))
    cat(sprintf("  Area Change:       %10.2f km² (%.1f%%)\n\n",
                row$area_difference, row$area_change_pct))
  }

  # Summary statistics
  cat("Summary Statistics:\n")
  cat(sprintf("  Mean TX1 area:     %10.2f km²\n", mean(overall_results$area_tx1_total)))
  cat(sprintf("  Mean TX2 area:     %10.2f km²\n", mean(overall_results$area_tx2_total)))
  cat(sprintf("  Mean overlap:      %10.2f km²\n", mean(overall_results$area_overlap)))
  cat(sprintf("  Mean Jaccard:      %10.3f\n", mean(overall_results$jaccard_index)))
  cat(sprintf("  Mean area change:  %10.1f%%\n\n", mean(overall_results$area_change_pct)))

  # --- Technology-Specific Analysis ---
  if (nrow(tech_results) > 0) {
    cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n")
    cat("2. TECHNOLOGY-SPECIFIC ANALYSIS (TX1 vs TX2)\n")
    cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n\n")

    tech_summary <- tech_results %>%
      group_by(technology) %>%
      summarise(
        mean_tx1_area = mean(area_tx1_total),
        mean_tx2_area = mean(area_tx2_total),
        mean_overlap = mean(area_overlap),
        mean_jaccard = mean(jaccard_index),
        mean_change_pct = ((mean_tx2_area - mean_tx1_area) / mean_tx1_area) * 100
      )

    for (i in 1:nrow(tech_summary)) {
      row <- tech_summary[i, ]
      cat(sprintf("%s:\n", toupper(row$technology)))
      cat(sprintf("  Mean TX1 area:     %10.2f km²\n", row$mean_tx1_area))
      cat(sprintf("  Mean TX2 area:     %10.2f km²\n", row$mean_tx2_area))
      cat(sprintf("  Mean overlap:      %10.2f km²\n", row$mean_overlap))
      cat(sprintf("  Mean Jaccard:      %10.3f\n", row$mean_jaccard))
      cat(sprintf("  Mean change:       %10.1f%%\n\n", row$mean_change_pct))
    }
  }

  # --- Wind-Solar Co-occurrence ---
  if (!is.null(cooccurrence_results) && nrow(cooccurrence_results) > 0) {
    cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n")
    cat("3. WIND-SOLAR CO-OCCURRENCE ANALYSIS\n")
    cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n\n")

    cooccurrence_summary <- cooccurrence_results %>%
      group_by(scenario) %>%
      summarise(
        mean_wind_area = mean(area_wind_total),
        mean_solar_area = mean(area_solar_total),
        mean_cooccurrence = mean(area_cooccurrence),
        mean_pct_wind_colocated = mean(pct_wind_colocated),
        mean_pct_solar_colocated = mean(pct_solar_colocated),
        mean_jaccard = mean(jaccard_index)
      )

    for (i in 1:nrow(cooccurrence_summary)) {
      row <- cooccurrence_summary[i, ]
      cat(sprintf("%s:\n", toupper(row$scenario)))
      cat(sprintf("  Mean wind area:            %10.2f km²\n", row$mean_wind_area))
      cat(sprintf("  Mean solar area:           %10.2f km²\n", row$mean_solar_area))
      cat(sprintf("  Mean co-occurrence area:   %10.2f km²\n", row$mean_cooccurrence))
      cat(sprintf("  Mean %% wind co-located:    %10.1f%%\n", row$mean_pct_wind_colocated))
      cat(sprintf("  Mean %% solar co-located:   %10.1f%%\n", row$mean_pct_solar_colocated))
      cat(sprintf("  Mean Jaccard index:        %10.3f\n\n", row$mean_jaccard))
    }

    # Comparison
    cat("TX1 vs TX2 Comparison:\n")
    tx1_summary <- cooccurrence_summary[cooccurrence_summary$scenario == "tx1", ]
    tx2_summary <- cooccurrence_summary[cooccurrence_summary$scenario == "tx2", ]

    if (nrow(tx1_summary) > 0 && nrow(tx2_summary) > 0) {
      diff_cooccurrence <- tx2_summary$mean_cooccurrence - tx1_summary$mean_cooccurrence
      pct_change <- (diff_cooccurrence / tx1_summary$mean_cooccurrence) * 100

      cat(sprintf("  Change in co-occurrence:   %10.2f km² (%.1f%%)\n",
                  diff_cooccurrence, pct_change))
    }
  }

  cat("\n")
  cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n")
  cat("KEY INSIGHTS\n")
  cat("\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n\n")

  # Maximum overlap threshold
  max_overlap_thresh <- overall_results$threshold[which.max(overall_results$jaccard_index)]
  cat(sprintf("\u2022 Maximum overlap occurs at threshold %d%% (Jaccard: %.3f)\n",
              max_overlap_thresh, max(overall_results$jaccard_index)))

  # Maximum change threshold
  max_change_idx <- which.max(abs(overall_results$area_change_pct))
  max_change_thresh <- overall_results$threshold[max_change_idx]
  max_change_val <- overall_results$area_change_pct[max_change_idx]
  cat(sprintf("\u2022 Maximum area change at threshold %d%% (%.1f%% change)\n",
              max_change_thresh, max_change_val))

  # Overall trend
  mean_change <- mean(overall_results$area_change_pct)
  if (mean_change > 0) {
    cat(sprintf("\u2022 TX2 generally requires MORE area than TX1 (avg +%.1f%%)\n", mean_change))
  } else {
    cat(sprintf("\u2022 TX2 generally requires LESS area than TX1 (avg %.1f%%)\n", mean_change))
  }

  cat(sprintf("\nAll outputs saved to:\n%s\n", output_path))

  sink()

  log_message(sprintf("Saved: %s", summary_file), "SUCCESS")
}

# ==============================================================================
# MAIN WORKFLOW
# ==============================================================================

main <- function() {

  cat("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\n")
  cat("\u2551     NET ZERO AUSTRALIA - SPATIAL ANALYSIS WORKFLOW         \u2551\n")
  cat("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\n")

  log_message("Starting analysis workflow", "INFO")
  log_message(sprintf("Output directory: %s", output_path), "INFO")

  # Run analysis steps
  overall_results <- run_step1_overall_analysis()
  tech_results <- run_step2_tech_overlap()
  cooccurrence_results <- run_step3_cooccurrence()

  # Create visualizations
  run_step4_visualization(overall_results, tech_results, cooccurrence_results)

  # Generate summary report
  create_summary_report(overall_results, tech_results, cooccurrence_results)

  cat("\n\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557\n")
  cat("\u2551                   WORKFLOW COMPLETE!                       \u2551\n")
  cat("\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d\n\n")

  log_message("Analysis complete!", "SUCCESS")
  log_message(sprintf("Check outputs in: %s", output_path), "INFO")

  return(list(
    overall = overall_results,
    technology = tech_results,
    cooccurrence = cooccurrence_results
  ))
}

# ==============================================================================
# RUN THE ANALYSIS
# ==============================================================================

# Execute the main workflow
results <- main()
