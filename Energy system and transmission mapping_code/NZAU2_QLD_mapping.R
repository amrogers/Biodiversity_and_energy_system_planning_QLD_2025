# Load required libraries
library(sf)
library(dplyr)
library(ggplot2)

# Turn off s2 processing to avoid geometry errors
sf_use_s2(FALSE)

# Set paths
gdb_dir <- "Z:/NZAU2_BESP/data/NZAU_BESP_scenario_outputs/2025_NZAu_Spoke"
state_shp <- "Z:/NZAU2_BESP/data/spatial/state_borders/Aus_state_borders.shp"
output_dir <- "Z:/NZAU2_BESP/data/NZAU_BESP_scenario_outputs/summary_maps/QLD"

# Create output directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Define scenarios
scenarios <- c("BAU", "gt30", "gt50", "gt70")
gdb_files <- c(
  "NZAu2_biodiversity_BAU_net_zero_2050.gdb",
  "NZAu2_biodiversity_gt30_net_zero_2050.gdb",
  "NZAu2_biodiversity_gt50_net_zero_2050.gdb",
  "NZAu2_biodiversity_gt70_net_zero_2050.gdb"
)

# Read and prepare Queensland boundary
cat("Reading Queensland boundary...\n")
qld <- st_read(state_shp, quiet = TRUE) %>%
  filter(state_name == "QUEENSLAND") %>%
  st_transform(crs = 7844) %>%
  st_make_valid()
cat("✓ Queensland boundary loaded\n")

# Function to process each scenario
process_scenario <- function(scenario, gdb_file) {
  
  cat("\n=== Processing scenario:", scenario, "===\n")
  
  # Check if PNG already exists
  png_output <- file.path(output_dir, paste0("QLD_", tolower(scenario), "_2050.png"))
  
  if (file.exists(png_output)) {
    cat("✓ PNG already exists, skipping:", png_output, "\n")
    return(NULL)
  }
  
  tryCatch({
    gdb_path <- file.path(gdb_dir, gdb_file)
    
    # List all layers in the gdb
    all_layers <- st_layers(gdb_path)$name
    
    # Filter layers matching the pattern (including interTX)
    target_layers <- all_layers[
      grepl("^(wind|pv).*case2_2050_(cpa|bulk|sink)$", all_layers) |
        grepl("^interTX.*case2_2050$", all_layers)
    ]
    
    if (length(target_layers) == 0) {
      warning(paste("No matching layers found in", gdb_file))
      return(NULL)
    }
    
    cat("Found", length(target_layers), "matching layers\n")
    
    # Read, clip, and categorize layers
    layer_list <- list()
    
    for (layer in target_layers) {
      cat("  Processing:", layer, "...")
      
      tryCatch({
        # Read layer
        lyr <- st_read(gdb_path, layer = layer, quiet = TRUE)
        
        # Make valid and transform to GDA2020
        lyr <- lyr %>%
          st_make_valid() %>%
          st_transform(crs = 7844) %>%
          st_make_valid()
        
        # Clip to QLD using st_intersection only (skip st_crop for better compatibility)
        lyr_qld <- st_intersection(lyr, qld) %>%
          st_make_valid()
        
        # Skip if empty after clipping
        if (nrow(lyr_qld) == 0) {
          cat(" skipped (no QLD features)\n")
          next
        }
        
        # Categorize layer
        lyr_qld$category <- case_when(
          grepl("^wind.*_cpa$", layer) ~ "wind_cpa",
          grepl("^pv.*_cpa$", layer) ~ "pv_cpa",
          grepl("^interTX.*case2_2050$", layer) ~ "interTX",
          grepl("_(bulk|sink)$", layer) ~ "transmission",
          TRUE ~ "other"
        )
        lyr_qld$layer_name <- layer
        
        # Keep only essential columns to avoid duplicates
        lyr_qld <- lyr_qld %>%
          select(category, layer_name)
        
        layer_list[[layer]] <- lyr_qld
        cat(" ✓", nrow(lyr_qld), "features\n")
        
      }, error = function(e) {
        cat(" ERROR:", conditionMessage(e), "\n")
      })
    }
    
    if (length(layer_list) == 0) {
      warning(paste("No valid layers after processing", scenario))
      return(NULL)
    }
    
    # Combine all layers
    combined <- bind_rows(layer_list)
    
    cat("\nLayer summary:\n")
    print(table(combined$category))
    
    cat("\nCreating map...\n")
    
    # Create map - LAYER ORDER: state fill, transmission (bottom), CPAs (top), state border
    p <- ggplot() +
      # White state fill (bottom layer)
      geom_sf(data = qld, fill = "white", color = NA)
    
    # Add transmission layers (underneath CPAs)
    if (any(combined$category == "transmission")) {
      trans_data <- filter(combined, category == "transmission")
      cat("  Adding", nrow(trans_data), "transmission features\n")
      p <- p + geom_sf(data = trans_data, 
                       color = "#404040", linewidth = 0.4, alpha = 0.8)
    }
    
    # Add interTX layer
    if (any(combined$category == "interTX")) {
      intertx_data <- filter(combined, category == "interTX")
      cat("  Adding", nrow(intertx_data), "interTX features\n")
      p <- p + geom_sf(data = intertx_data, 
                       color = "#202020", linewidth = 0.5, alpha = 0.9)
    }
    
    # Add wind CPAs (on top of transmission)
    if (any(combined$category == "wind_cpa")) {
      wind_data <- filter(combined, category == "wind_cpa")
      cat("  Adding", nrow(wind_data), "wind CPA features\n")
      p <- p + geom_sf(data = wind_data, 
                       fill = "#87CEEB", color = NA, alpha = 0.7)
    }
    
    # Add PV CPAs (on top of transmission)
    if (any(combined$category == "pv_cpa")) {
      pv_data <- filter(combined, category == "pv_cpa")
      cat("  Adding", nrow(pv_data), "PV CPA features\n")
      p <- p + geom_sf(data = pv_data, 
                       fill = "#FFA500", color = NA, alpha = 0.7)
    }
    
    # Add Queensland border on top (last layer)
    p <- p + 
      geom_sf(data = qld, fill = NA, color = "black", linewidth = 0.8) +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Save map directly to QLD folder
    cat("Saving map to:", png_output, "\n")
    
    ggsave(png_output, plot = p, width = 10, height = 12, dpi = 300, bg = "white")
    
    # Check if PNG was created
    if (file.exists(png_output)) {
      file_size <- file.info(png_output)$size
      cat("✓ PNG created successfully (", round(file_size/1024/1024, 2), "MB )\n")
    } else {
      warning("✗ PNG file not created!")
    }
    
    cat("✓ Completed scenario:", scenario, "\n")
    
    return(combined)
    
  }, error = function(e) {
    cat("✗ ERROR in scenario", scenario, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Process all scenarios
for (i in seq_along(scenarios)) {
  result <- process_scenario(scenarios[i], gdb_files[i])
}

cat("\n=== COMPLETE ===\n")

# Final summary check
cat("\n=== FINAL FILE CHECK ===\n")
for (scenario in scenarios) {
  png_file <- file.path(output_dir, paste0("QLD_", tolower(scenario), "_2050.png"))
  
  cat("Scenario:", scenario, "- PNG exists:", file.exists(png_file))
  if (file.exists(png_file)) {
    cat(" (", round(file.info(png_file)$size/1024/1024, 2), "MB )")
  }
  cat("\n")
}