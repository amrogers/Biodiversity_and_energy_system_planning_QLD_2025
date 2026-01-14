# =============================================================================
# Energy Cost Increase Analysis and Visualization - Reproducible Version
# =============================================================================
# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================

# Load required libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, readr, dplyr, tidyr, here, magick)

# --- USER CONTROL SETTINGS ---
# Set to TRUE to force re-generation of the plot even if it exists
overwrite_mode <- FALSE 

# =============================================================================
# Setup and Path Configuration
# =============================================================================

# Define standardized paths using here()
# Paths are relative to your .Rproj file
data_file  <- here("data", "Energy_system_model_outputs", "cost_increase_results.csv")
output_dir <- here("results", "figures")
output_file <- file.path(output_dir, "energy_cost_increase_plot.png")

# Ensure output directory exists
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# =============================================================================
# Smart Execution Logic
# =============================================================================

cat("Checking for existing cost analysis plot...\n")

if (file.exists(output_file) && !overwrite_mode) {
  
  # --- CASE 1: SKIP AND DISPLAY ---
  cat(">>> Found existing plot at:", output_file, "\n")
  cat(">>> Skipping processing (Set overwrite_mode = TRUE to re-run).\n")
  
  if (interactive()) {
    img <- magick::image_read(output_file)
    print(img)
  }
  
} else {
  
  # --- CASE 2: RUN ANALYSIS ---
  cat(">>> Processing cost increase data...\n")
  
  if (!file.exists(data_file)) {
    stop(paste("Error: Data file not found at:", data_file,
               "\nPlease ensure the Figshare data is unzipped into the /data folder."))
  }
  
  # Read the data
  data <- read_csv(data_file, show_col_types = FALSE)
  
  # Data processing (using your column index logic)
  plot_data_long <- data.frame(
    tx_scenario = data[[1]],
    HBVA_exclusion = data[[2]],
    residential = data[[3]],
    industrial = data[[4]]
  ) %>%
    mutate(HBVA_exclusion = trimws(HBVA_exclusion)) %>%
    pivot_longer(
      cols = c(residential, industrial),
      names_to = "cost_type",
      values_to = "cost_increase"
    ) %>%
    filter(!is.na(cost_increase)) %>%
    filter(HBVA_exclusion %in% c("BAU", "30", "50", "70", "90")) %>%
    mutate(HBVA_exclusion = factor(HBVA_exclusion, 
                                   levels = c("BAU", "30", "50", "70", "90")))
  
  # Create visualization
  cat("Creating visualization...\n")
  cost_plot <- ggplot(plot_data_long, 
                      aes(x = HBVA_exclusion, 
                          y = cost_increase,
                          group = interaction(tx_scenario, cost_type),
                          color = cost_type,
                          linetype = tx_scenario)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_linetype_manual(
      values = c("tx_1" = "solid", "tx_2" = "longdash"),
      name = "Transmission Scenario",
      labels = c("tx_1" = "Scenario 1", "tx_2" = "Scenario 2")
    ) +
    scale_color_brewer(
      palette = "Set1", 
      name = "Cost Type",
      labels = c("residential" = "Residential", "industrial" = "Industrial")
    ) +
    theme_minimal() +
    labs(
      x = "% High Biodiversity Value Area Excluded",
      y = "Increase in Energy Costs (%)",
      caption = "Data source: Energy system modeling results"
    ) +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.position = "right"
    )
  
  # Save and Display
  ggsave(output_file, plot = cost_plot, width = 12, height = 6, dpi = 300, bg = "white")
  cat("✓ Plot saved to:", output_file, "\n")
  print(cost_plot)
}

cat("\n=== ANALYSIS COMPLETE ===\n")