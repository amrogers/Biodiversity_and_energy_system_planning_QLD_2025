# =============================================================================
# Energy Infrastructure Spatial Analysis and Mapping - Reproducible Version
# =============================================================================
# This script processes energy system modeling results and creates detailed maps
# showing renewable energy infrastructure under different biodiversity protection
# scenarios, separated by domestic vs export purposes.
#
# Data requirements:
# - Figshare data placed per README (BESP_data_qld_2025/ or data/)
# - GDB files: QLD_v202412_eplus_tx1.gdb and QLD_v202412_eplus_tx2.gdb
#
# Author: Andrew Rogers
# LLMs used: Claude AI and Gemini
# Date: June 2025; Updated Jan 2026
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr, scales, here)

# --- Path configuration via _paths.R (sets data_root) ---
source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# Set up parallel processing
future::plan(multisession, workers = max(1, parallel::detectCores() - 1))

# =============================================================================
# Setup and Path Configuration
# =============================================================================

cat("Starting energy infrastructure mapping analysis...\n")

# Standardized paths (data_root set by _paths.R)
scenarios_dir <- file.path(data_root, "Energy_system_model_outputs", "Energy_system_analysis_scenarios")
output_root   <- here("results", "figures", "energy_maps")

if (!dir.exists(output_root)) dir.create(output_root, recursive = TRUE)

# GDB names (paths constructed on-the-fly from scenarios_dir)
gdb_names <- c(tx1 = "QLD_v202412_eplus_tx1.gdb", tx2 = "QLD_v202412_eplus_tx2.gdb")

# Check for / unzip GDB files
for (tx in names(gdb_names)) {
  gdb_path <- file.path(scenarios_dir, gdb_names[tx])
  zip_path <- paste0(gdb_path, ".zip")
  if (!dir.exists(gdb_path)) {
    if (file.exists(zip_path)) {
      message(">>> Extracting missing GDB: ", gdb_names[tx], ". This may take a moment...")
      unzip(zip_path, exdir = dirname(gdb_path))
    } else {
      stop("Cannot find GDB file or ZIP archive at: ", gdb_path)
    }
  }
}

# Define analysis parameters
thresholds <- c(0, 10, 30, 50, 70, 90)
years <- c(2030, 2040, 2050)
transmission_scenarios <- c("tx1", "tx2")

cat("Configuration:\n")
cat("- Thresholds:", paste(thresholds, collapse = ", "), "\n")
cat("- Years:", paste(years, collapse = ", "), "\n")
cat("- Scenarios:", paste(transmission_scenarios, collapse = ", "), "\n")

# =============================================================================
# Data Processing Functions
# =============================================================================

# Read and process a single GDB layer, tagging it with technology type
process_layer <- function(gdb_path, layer_name) {
  tryCatch({
    layer <- st_read(gdb_path, layer = layer_name, quiet = TRUE)

    tech_type <- case_when(
      grepl("^pv",   layer_name, ignore.case = TRUE) ~ "solar_pv",
      grepl("^wind", layer_name, ignore.case = TRUE) ~ "wind",
      grepl("^off",  layer_name, ignore.case = TRUE) ~ "offshore_wind",
      TRUE ~ "other"
    )

    available_cols <- colnames(layer)
    selected_cols  <- c("geometry")
    if ("areakm"   %in% available_cols) selected_cols <- c(selected_cols, "areakm")
    if ("domestic" %in% available_cols) selected_cols <- c(selected_cols, "domestic")
    if ("capacity" %in% available_cols) selected_cols <- c(selected_cols, "capacity")

    layer <- layer %>%
      select(any_of(selected_cols)) %>%
      mutate(technology = tech_type, layer_name = layer_name) %>%
      st_make_valid()

    return(layer)
  }, error = function(e) {
    cat("Warning: Could not process layer", layer_name, "\n")
    return(NULL)
  })
}

# =============================================================================
# Process and Save Shapefiles  (skip if shapefile already exists)
# =============================================================================

for (tx_scenario in transmission_scenarios) {
  cat(sprintf("\n=== Processing %s scenario ===\n", tx_scenario))

  input_gdb_path <- file.path(scenarios_dir, gdb_names[tx_scenario])
  output_folder  <- file.path(output_root, paste0("shapefiles_", tx_scenario))
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

  gdb_layers <- st_layers(input_gdb_path)$name

  for (threshold in thresholds) {
    cat(sprintf("Processing threshold: %d\n", threshold))

    output_filename <- file.path(output_folder,
                                 sprintf("combined_renewables_2050_threshold_%d.shp", threshold))

    # Skip if shapefile already exists
    if (file.exists(output_filename)) {
      cat(sprintf("   - Skipping threshold %d (shapefile already exists)\n", threshold))
      next
    }

    # Pattern-match layers for each technology type
    wind_pattern     <- sprintf("^wind.*_%d_.*_2050_cpa$", threshold)
    pv_pattern       <- sprintf("^pv.*_%d_.*_2050_cpa$", threshold)
    offshore_pattern <- sprintf("^off.*_%d_.*_2050_cpa$", threshold)

    wind_layers     <- gdb_layers[grep(wind_pattern,     gdb_layers)]
    pv_layers       <- gdb_layers[grep(pv_pattern,       gdb_layers)]
    offshore_layers <- gdb_layers[grep(offshore_pattern, gdb_layers)]

    cat("Found layers:\n")
    cat("Wind:", paste(wind_layers,     collapse = ", "), "\n")
    cat("PV:",   paste(pv_layers,       collapse = ", "), "\n")
    cat("Offshore:", paste(offshore_layers, collapse = ", "), "\n")

    all_layers <- c(wind_layers, pv_layers, offshore_layers)

    if (length(all_layers) > 0) {
      combined_layers <- furrr::future_map(all_layers, function(layer_name) {
        cat(sprintf("Processing layer: %s\n", layer_name))
        process_layer(input_gdb_path, layer_name)
      }) %>%
        bind_rows()

      if (nrow(combined_layers) > 0) {
        st_write(combined_layers, output_filename,
                 driver = "ESRI Shapefile", append = FALSE, quiet = TRUE)
        cat(sprintf("Saved combined layer for threshold %d to: %s\n", threshold, output_filename))
      }
    } else {
      cat(sprintf("No layers found for threshold %d\n", threshold))
    }
  }
}

# =============================================================================
# Generate Summary Statistics
# =============================================================================

cat("\n=== Generating Summary Statistics ===\n")

for (tx_scenario in transmission_scenarios) {
  cat(sprintf("Creating summary for %s...\n", tx_scenario))

  shapefiles_folder <- file.path(output_root, paste0("shapefiles_", tx_scenario))
  shp_files <- list.files(shapefiles_folder, pattern = "combined_renewables_.*\\.shp$",
                          full.names = TRUE)

  if (length(shp_files) == 0) {
    cat("No shapefiles found for", tx_scenario, "\n")
    next
  }

  get_threshold <- function(filename) {
    as.numeric(gsub(".*threshold_([0-9]+)\\.shp$", "\\1", basename(filename)))
  }

  all_data <- list()

  for (file in shp_files) {
    tryCatch({
      shp       <- st_read(file, quiet = TRUE)
      threshold <- get_threshold(file)

      if (nrow(shp) > 0) {
        summary_stats <- shp %>%
          st_drop_geometry() %>%
          group_by(technology) %>%
          summarise(
            total_area = sum(areakm, na.rm = TRUE),
            n_sites    = n(),
            .groups    = "drop"
          ) %>%
          mutate(threshold = threshold, tx_scenario = tx_scenario)

        all_data[[length(all_data) + 1]] <- summary_stats
      }
    }, error = function(e) {
      cat("Error processing", basename(file), ":", e$message, "\n")
    })
  }

  if (length(all_data) > 0) {
    scenario_summary <- bind_rows(all_data) %>%
      select(tx_scenario, threshold, technology, total_area, n_sites) %>%
      arrange(threshold, technology)

    summary_filename <- file.path(output_root,
                                  paste0("renewable_infrastructure_summary_", tx_scenario, ".csv"))
    write.csv(scenario_summary, summary_filename, row.names = FALSE)
    cat("Summary for", tx_scenario, ":\n")
    print(scenario_summary)
    cat("Saved to:", summary_filename, "\n\n")
  }
}

# =============================================================================
# Mapping Functions
# =============================================================================

# Load QLD boundary with error handling
load_base_map <- function() {
  tryCatch({
    aus_states    <- ozmaps::ozmap_states
    qld_boundary  <- aus_states[aus_states$NAME == "Queensland", ]
    return(qld_boundary)
  }, error = function(e) {
    cat("Warning: Could not load base map data:", e$message, "\n")
    return(NULL)
  })
}

# Create and return a ggplot map; returns NULL if shapefile or PNG already exists
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
      axis.text      = element_blank(),
      axis.ticks     = element_blank(),
      panel.grid     = element_blank(),
      plot.title     = element_text(size = 16, face = "bold"),
      plot.subtitle  = element_text(size = 12),
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

# =============================================================================
# Completion Summary
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("ENERGY INFRASTRUCTURE MAPPING COMPLETE\n")
cat(strrep("=", 70), "\n")

cat("Generated outputs (under results/figures/energy_maps/):\n")
for (tx_scenario in transmission_scenarios) {
  cat(sprintf("- %s domestic maps: domestic_maps_%s/\n",  toupper(tx_scenario), tx_scenario))
  cat(sprintf("- %s export maps:   export_maps_%s/\n",    toupper(tx_scenario), tx_scenario))
  cat(sprintf("- %s shapefiles:    shapefiles_%s/\n",     toupper(tx_scenario), tx_scenario))
  cat(sprintf("- %s summary CSV:   renewable_infrastructure_summary_%s.csv\n",
              toupper(tx_scenario), tx_scenario))
}

cat("\nFile naming: domestic_layer_map_[threshold]_[year].png\n")
cat("Example:     domestic_layer_map_0_2050.png\n")
cat("\nAnalysis complete!\n")
