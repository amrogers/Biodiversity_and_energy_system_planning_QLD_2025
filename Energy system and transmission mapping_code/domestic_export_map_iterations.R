# =============================================================================
# Energy Infrastructure Spatial Analysis and Mapping - Complete Version
# =============================================================================
# This script processes energy system modeling results and creates detailed maps
# showing renewable energy infrastructure under different biodiversity protection
# scenarios, separated by domestic vs export purposes.
#
# Data requirements:
# - Supplementary data folder containing Energy_system_analysis_scenarios/
# - Extracted GDB files: QLD_v202412_eplus_tx1.gdb and QLD_v202412_eplus_tx2.gdb
#
# Author: Andrew Rogers
# Date: June 2025
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, data.table, progress, ggplot2, ozmaps, purrr, scales)

# Set up parallel processing
future::plan(multisession, workers = max(1, parallel::detectCores() - 1))

# =============================================================================
# Setup and Path Configuration
# =============================================================================

cat("Starting energy infrastructure mapping analysis...\n")

# Define base paths relative to supplementary data folder
scenarios_dir <- "Energy_system_analysis_scenarios"
if (!dir.exists(scenarios_dir)) {
  stop("Error: Cannot find Energy_system_analysis_scenarios directory.\n",
       "Please ensure you are running this script from the supplementary data folder.")
}

# Define GDB file paths (need to be extracted from ZIP files)
gdb_files <- list(
  tx1 = file.path(scenarios_dir, "QLD_v202412_eplus_tx1.gdb"),
  tx2 = file.path(scenarios_dir, "QLD_v202412_eplus_tx2.gdb")
)

# Check for extracted GDB files
for (tx_scenario in names(gdb_files)) {
  if (!dir.exists(gdb_files[[tx_scenario]])) {
    zip_file <- paste0(gdb_files[[tx_scenario]], ".zip")
    if (file.exists(zip_file)) {
      stop("Please extract ", zip_file, " before running this script.")
    } else {
      stop("Cannot find GDB file or ZIP archive for ", tx_scenario)
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

# Function to read and process layer with technology type
process_layer <- function(gdb_path, layer_name) {
  tryCatch({
    layer <- st_read(gdb_path, layer = layer_name, quiet = TRUE)
    
    # Determine technology type
    tech_type <- case_when(
      grepl("^pv", layer_name, ignore.case = TRUE) ~ "solar_pv",
      grepl("^wind", layer_name, ignore.case = TRUE) ~ "wind",
      grepl("^off", layer_name, ignore.case = TRUE) ~ "offshore_wind",
      TRUE ~ "other"
    )
    
    # Select necessary columns and add technology type
    available_cols <- colnames(layer)
    selected_cols <- c("geometry")
    
    if ("areakm" %in% available_cols) selected_cols <- c(selected_cols, "areakm")
    if ("domestic" %in% available_cols) selected_cols <- c(selected_cols, "domestic")
    if ("capacity" %in% available_cols) selected_cols <- c(selected_cols, "capacity")
    
    layer <- layer %>% 
      select(any_of(selected_cols)) %>%
      mutate(
        technology = tech_type,
        layer_name = layer_name
      ) %>%
      st_make_valid()
    
    return(layer)
  }, error = function(e) {
    cat("Warning: Could not process layer", layer_name, "\n")
    return(NULL)
  })
}

# =============================================================================
# Process and Save Shapefiles
# =============================================================================

# Process each transmission scenario
for (tx_scenario in transmission_scenarios) {
  cat(sprintf("\n=== Processing %s scenario ===\n", tx_scenario))
  
  input_gdb_path <- gdb_files[[tx_scenario]]
  output_folder <- file.path("figures", "energy_maps", paste0("shapefiles_", tx_scenario))
  
  # Create output directory
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Get the list of layers in the GDB
  gdb_layers <- st_layers(input_gdb_path)$name
  
  # Process each threshold
  for (threshold in thresholds) {
    cat(sprintf("Processing threshold: %d\n", threshold))
    
    # Get layers for each technology type with specific pattern matching
    wind_pattern <- sprintf("^wind.*_%d_.*_2050_cpa$", threshold)
    pv_pattern <- sprintf("^pv.*_%d_.*_2050_cpa$", threshold)
    offshore_pattern <- sprintf("^off.*_%d_.*_2050_cpa$", threshold)
    
    wind_layers <- gdb_layers[grep(wind_pattern, gdb_layers)]
    pv_layers <- gdb_layers[grep(pv_pattern, gdb_layers)]
    offshore_layers <- gdb_layers[grep(offshore_pattern, gdb_layers)]
    
    # Print found layers for debugging
    cat("Found layers:\n")
    cat("Wind:", paste(wind_layers, collapse=", "), "\n")
    cat("PV:", paste(pv_layers, collapse=", "), "\n")
    cat("Offshore:", paste(offshore_layers, collapse=", "), "\n")
    
    # Combine all layer names
    all_layers <- c(wind_layers, pv_layers, offshore_layers)
    
    if (length(all_layers) > 0) {
      # Process all layers and combine them
      combined_layers <- furrr::future_map(all_layers, function(layer_name) {
        cat(sprintf("Processing layer: %s\n", layer_name))
        process_layer(input_gdb_path, layer_name)
      }) %>%
        bind_rows()
      
      # Create output filename
      output_filename <- file.path(output_folder, 
                                   sprintf("combined_renewables_2050_threshold_%d.shp", threshold))
      
      # Save combined layer as shapefile
      if (nrow(combined_layers) > 0) {
        st_write(combined_layers, 
                 output_filename, 
                 driver = "ESRI Shapefile",
                 append = FALSE, quiet = TRUE)
        
        cat(sprintf("Saved combined layer for threshold %d to: %s\n", 
                    threshold, output_filename))
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

# Process each transmission scenario for summaries
for (tx_scenario in transmission_scenarios) {
  cat(sprintf("Creating summary for %s...\n", tx_scenario))
  
  shapefiles_folder <- file.path("figures", "energy_maps", paste0("shapefiles_", tx_scenario))
  
  # Get list of shapefile paths
  shp_files <- list.files(shapefiles_folder, pattern = "combined_renewables_.*\\.shp$", full.names = TRUE)
  
  if (length(shp_files) == 0) {
    cat("No shapefiles found for", tx_scenario, "\n")
    next
  }
  
  # Function to extract threshold from filename
  get_threshold <- function(filename) {
    as.numeric(gsub(".*threshold_([0-9]+)\\.shp$", "\\1", basename(filename)))
  }
  
  # Initialize empty list to store data
  all_data <- list()
  
  # Read and process each shapefile
  for(file in shp_files) {
    tryCatch({
      # Read shapefile
      shp <- st_read(file, quiet = TRUE)
      
      # Extract threshold
      threshold <- get_threshold(file)
      
      # Calculate summary statistics
      if (nrow(shp) > 0) {
        summary_stats <- shp %>%
          st_drop_geometry() %>%
          group_by(technology) %>%
          summarise(
            total_area = sum(areakm, na.rm = TRUE),
            n_sites = n(),
            .groups = "drop"
          ) %>%
          mutate(
            threshold = threshold,
            tx_scenario = tx_scenario
          )
        
        # Store in list
        all_data[[length(all_data) + 1]] <- summary_stats
      }
    }, error = function(e) {
      cat("Error processing", basename(file), ":", e$message, "\n")
    })
  }
  
  # Combine all summaries for this scenario
  if (length(all_data) > 0) {
    scenario_summary <- bind_rows(all_data) %>%
      select(tx_scenario, threshold, technology, total_area, n_sites) %>%
      arrange(threshold, technology)
    
    # Write the results to CSV
    summary_filename <- file.path("figures", "energy_maps", 
                                  paste0("renewable_infrastructure_summary_", tx_scenario, ".csv"))
    write.csv(scenario_summary, summary_filename, row.names = FALSE)
    
    # Display the results
    cat("Summary for", tx_scenario, ":\n")
    print(scenario_summary)
    cat("Saved to:", summary_filename, "\n\n")
  }
}

# =============================================================================
# Mapping Functions
# =============================================================================

# Function to load and prepare base map data
load_base_map <- function() {
  tryCatch({
    # Load Australian state boundaries
    aus_states <- ozmaps::ozmap_states
    qld_boundary <- aus_states[aus_states$NAME == "Queensland", ]
    return(qld_boundary)
  }, error = function(e) {
    cat("Warning: Could not load base map data:", e$message, "\n")
    return(NULL)
  })
}

# Function to create plot for specific year, threshold, and domestic status
create_plot <- function(tx_scenario, year, threshold, is_domestic = TRUE) {
  
  # Load base map
  qld_boundary <- load_base_map()
  if (is.null(qld_boundary)) {
    return(NULL)
  }
  
  # Create base plot
  base_plot <- ggplot() +
    geom_sf(data = qld_boundary, fill = "white", color = "black", size = 0.5) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  
  # Load infrastructure data
  shapefiles_folder <- file.path("figures", "energy_maps", paste0("shapefiles_", tx_scenario))
  shp_file <- file.path(shapefiles_folder, 
                        sprintf("combined_renewables_2050_threshold_%d.shp", threshold))
  
  if (file.exists(shp_file)) {
    tryCatch({
      infrastructure_data <- st_read(shp_file, quiet = TRUE)
      
      # Filter by domestic status if the column exists
      if ("domestic" %in% colnames(infrastructure_data)) {
        filtered_data <- infrastructure_data[infrastructure_data$domestic == as.integer(is_domestic), ]
      } else {
        filtered_data <- infrastructure_data
        cat("Warning: No 'domestic' column found, showing all data\n")
      }
      
      if (nrow(filtered_data) > 0) {
        # Define colors for different technologies
        tech_colors <- c(
          "solar_pv" = "#FFA500",      # Orange
          "wind" = "lightblue",          # light Blue  
          "offshore_wind" = "blue",  # darkblue
          "other" = "#808080"          # Gray
        )
        
        # Add infrastructure to plot
        base_plot <- base_plot +
          geom_sf(data = filtered_data,
                  aes(fill = technology, color = technology),
                  alpha = 0.7,
                  size = 0.1) +
          scale_fill_manual(values = tech_colors, name = "Technology") +
          scale_color_manual(values = tech_colors, name = "Technology")
      }
      
      # Add title and labels
      map_type <- ifelse(is_domestic, "Domestic", "Export")
      base_plot <- base_plot +
        labs(
          title = sprintf("%s Energy Infrastructure - %s", map_type, toupper(tx_scenario)),
          subtitle = sprintf("Threshold: %d%%, Year: %d", threshold, year),
          caption = "Source: Energy system modeling results"
        )
      
    }, error = function(e) {
      cat("Error loading infrastructure data:", e$message, "\n")
    })
  } else {
    cat("Warning: Shapefile not found:", shp_file, "\n")
  }
  
  return(base_plot)
}

# =============================================================================
# Generate Maps for All Scenarios
# =============================================================================

cat("\n=== Generating Maps ===\n")

# Generate plots for all combinations
for (tx_scenario in transmission_scenarios) {
  cat(sprintf("Creating maps for %s scenario...\n", tx_scenario))
  
  # Create output directories
  domestic_output <- file.path("figures", "energy_maps", paste0("domestic_maps_", tx_scenario))
  export_output <- file.path("figures", "energy_maps", paste0("export_maps_", tx_scenario))
  
  dir.create(domestic_output, recursive = TRUE, showWarnings = FALSE)
  dir.create(export_output, recursive = TRUE, showWarnings = FALSE)
  
  for (year in years) {
    for (threshold in thresholds) {
      cat(sprintf("Creating maps for threshold %d%%, year %d...\n", threshold, year))
      
      # Create and save domestic plot
      domestic_plot <- create_plot(tx_scenario, year, threshold, is_domestic = TRUE)
      if (!is.null(domestic_plot)) {
        output_file_path_domestic <- file.path(domestic_output,
                                               sprintf("domestic_layer_map_%d_%d.png", 
                                                       threshold, year))
        ggsave(domestic_plot, filename = output_file_path_domestic, 
               width = 15, height = 15, units = "in", dpi = 300, bg = "white")
        cat("Domestic map saved to:", output_file_path_domestic, "\n")
      }
      
      # Create and save export plot
      export_plot <- create_plot(tx_scenario, year, threshold, is_domestic = FALSE)
      if (!is.null(export_plot)) {
        output_file_path_export <- file.path(export_output,
                                             sprintf("export_layer_map_%d_%d.png", 
                                                     threshold, year))
        ggsave(export_plot, filename = output_file_path_export, 
               width = 15, height = 15, units = "in", dpi = 300, bg = "white")
        cat("Export map saved to:", output_file_path_export, "\n")
      }
    }
  }
}

# =============================================================================
# Completion Summary
# =============================================================================

cat("\n", "="*70, "\n")
cat("ENERGY INFRASTRUCTURE MAPPING COMPLETE\n")
cat("="*70, "\n")

cat("Generated outputs:\n")
for (tx_scenario in transmission_scenarios) {
  cat(sprintf("- %s domestic maps: figures/energy_maps/domestic_maps_%s/\n", 
              toupper(tx_scenario), tx_scenario))
  cat(sprintf("- %s export maps: figures/energy_maps/export_maps_%s/\n", 
              toupper(tx_scenario), tx_scenario))
  cat(sprintf("- %s shapefiles: figures/energy_maps/shapefiles_%s/\n", 
              toupper(tx_scenario), tx_scenario))
  cat(sprintf("- %s summary: figures/energy_maps/renewable_infrastructure_summary_%s.csv\n", 
              toupper(tx_scenario), tx_scenario))
}

cat("\nFile naming convention:\n")
cat("- Domestic maps: domestic_layer_map_[threshold]_[year].png\n")
cat("- Export maps: export_layer_map_[threshold]_[year].png\n")
cat("- Example: domestic_layer_map_0_2050.png\n")

cat("\nTo reproduce your exact original path structure, you would run this script\n")
cat("from the parent directory of 'Energy_system_analysis_scenarios'\n")

cat("\nAnalysis complete!\n")