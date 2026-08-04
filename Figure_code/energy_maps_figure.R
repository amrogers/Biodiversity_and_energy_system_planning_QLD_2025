# =============================================================================
# VRE Map Figure Generation
# =============================================================================
# Reads combined-renewables shapefiles from results/figures/energy_maps/shapefiles_tx*/
# and writes domestic/export PNG maps to results/figures/energy_maps/.
#
# Can be run standalone or sourced from _RUN_ALL.R.
# When sourced from _RUN_ALL.R, inherits tx2_enabled and output_root if already set.
#
# Plotting logic (load_base_map, create_plot, map generation loop) is shared
# verbatim with domestic_export_map_iterations.R.
#
# Author: Andrew Rogers
# LLMs used: Claude AI and Gemini
# Date: Aug 2026
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, ggplot2, ozmaps, purrr, scales, here)

source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# output_root may already be set if called after domestic_export_map_iterations.R
if (!exists("output_root")) output_root <- here("results", "figures", "energy_maps")
if (!dir.exists(output_root)) dir.create(output_root, recursive = TRUE)

# tx2_enabled may be set by _RUN_ALL.R; default FALSE (TX1 only appears in manuscript)
if (!exists("tx2_enabled")) tx2_enabled <- FALSE

thresholds             <- c(0, 10, 30, 50, 70, 90)
years                  <- c(2030, 2040, 2050)
transmission_scenarios <- if (tx2_enabled) c("tx1", "tx2") else "tx1"

# =============================================================================
# Mapping Functions
# (Copied verbatim from domestic_export_map_iterations.R -- do not alter independently)
# =============================================================================

load_base_map <- function() {
  tryCatch({
    aus_states   <- ozmaps::ozmap_states
    qld_boundary <- aus_states[aus_states$NAME == "Queensland", ]
    return(qld_boundary)
  }, error = function(e) {
    cat("Warning: Could not load base map data:", e$message, "\n")
    return(NULL)
  })
}

create_plot <- function(tx_scenario, year, threshold, is_domestic = TRUE) {

  shp_folder <- file.path(output_root, paste0("shapefiles_", tx_scenario))
  shp_file   <- file.path(shp_folder,
                           sprintf("combined_renewables_2050_threshold_%d.shp", threshold))

  if (!file.exists(shp_file)) {
    cat("Warning: Shapefile not found:", shp_file, "\n")
    return(NULL)
  }

  # Skip if PNG already exists
  map_type <- ifelse(is_domestic, "domestic", "export")
  png_name <- sprintf("%s_layer_map_%d_%d.png", map_type, threshold, year)
  png_path <- file.path(output_root, paste0(map_type, "_maps_", tx_scenario), png_name)

  if (file.exists(png_path)) {
    cat("   - Map already exists:", png_name, "\n")
    return(NULL)
  }

  qld_boundary <- load_base_map()
  if (is.null(qld_boundary)) return(NULL)

  base_plot <- ggplot() +
    geom_sf(data = qld_boundary, fill = "white", color = "black", size = 0.5) +
    theme_minimal() +
    theme(
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      panel.grid      = element_blank(),
      plot.title      = element_text(size = 16, face = "bold"),
      plot.subtitle   = element_text(size = 12),
      legend.position = "bottom"
    )

  tryCatch({
    infrastructure_data <- st_read(shp_file, quiet = TRUE)

    if ("domestic" %in% colnames(infrastructure_data)) {
      filtered_data <- infrastructure_data[infrastructure_data$domestic == as.integer(is_domestic), ]
    } else {
      filtered_data <- infrastructure_data
      cat("Warning: No 'domestic' column found, showing all data\n")
    }

    if (nrow(filtered_data) > 0) {
      tech_colors <- c(
        "solar_pv"      = "#FFA500",
        "wind"          = "lightblue",
        "offshore_wind" = "blue",
        "other"         = "#808080"
      )
      base_plot <- base_plot +
        geom_sf(data = filtered_data,
                aes(fill = technology, color = technology),
                alpha = 0.7, size = 0.1) +
        scale_fill_manual(values  = tech_colors, name = "Technology") +
        scale_color_manual(values = tech_colors, name = "Technology")
    }

    map_label <- ifelse(is_domestic, "Domestic", "Export")
    base_plot <- base_plot +
      labs(
        title    = sprintf("%s Energy Infrastructure - %s", map_label, toupper(tx_scenario)),
        subtitle = sprintf("Threshold: %d%%, Year: %d", threshold, year),
        caption  = "Source: Energy system modeling results"
      )

  }, error = function(e) {
    cat("Error loading infrastructure data:", e$message, "\n")
  })

  return(base_plot)
}

# =============================================================================
# Generate Maps for All Scenarios
# =============================================================================

cat("\n=== Generating Maps ===\n")

for (tx_scenario in transmission_scenarios) {
  cat(sprintf("Creating maps for %s scenario...\n", tx_scenario))

  domestic_output <- file.path(output_root, paste0("domestic_maps_", tx_scenario))
  export_output   <- file.path(output_root, paste0("export_maps_",   tx_scenario))

  dir.create(domestic_output, recursive = TRUE, showWarnings = FALSE)
  dir.create(export_output,   recursive = TRUE, showWarnings = FALSE)

  for (year in years) {
    for (threshold in thresholds) {
      cat(sprintf("Creating maps for threshold %d%%, year %d...\n", threshold, year))

      domestic_plot <- create_plot(tx_scenario, year, threshold, is_domestic = TRUE)
      if (!is.null(domestic_plot)) {
        out_path <- file.path(domestic_output,
                              sprintf("domestic_layer_map_%d_%d.png", threshold, year))
        ggsave(domestic_plot, filename = out_path,
               width = 15, height = 15, units = "in", dpi = 300, bg = "white")
        cat("Domestic map saved to:", out_path, "\n")
      }

      export_plot <- create_plot(tx_scenario, year, threshold, is_domestic = FALSE)
      if (!is.null(export_plot)) {
        out_path <- file.path(export_output,
                              sprintf("export_layer_map_%d_%d.png", threshold, year))
        ggsave(export_plot, filename = out_path,
               width = 15, height = 15, units = "in", dpi = 300, bg = "white")
        cat("Export map saved to:", out_path, "\n")
      }
    }
  }
}

cat("\nMap generation complete.\n")
