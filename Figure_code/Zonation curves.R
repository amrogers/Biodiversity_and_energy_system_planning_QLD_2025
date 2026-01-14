# =============================================================================
# Zonation Performance Curves Analysis
# =============================================================================
# Description: Processes Zonation .run files to plot performance curves.
# =============================================================================
# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyr, readr, here, purrr)

# --- USER CONTROL SETTINGS ---
# Set to TRUE if you want to force re-processing even if files exist
overwrite_mode <- FALSE 

# =============================================================================
# Setup and Path Configuration
# =============================================================================

# Define standardized paths relative to the .Rproj file
zonation_dir <- here("data", "Zonation_analysis", "Zonation_output")
output_dir   <- here("results", "zonation_figures")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Target Zonation result file (based on your folder map)
# Adjust 'results.txt' or '.run' filename as per your specific Zonation output
zonation_results_path <- file.path(zonation_dir, "250m_SNES_ECNES_red_zones_weighted_QLD", "out_example1", "results.txt")

# Define the final Plot output path
final_plot_path <- file.path(output_dir, "zonation_performance_curves.png")

# =============================================================================
# Smart Execution Logic
# =============================================================================

cat("Checking for existing Zonation analysis...\n")

if (file.exists(final_plot_path) && !overwrite_mode) {
  
  # --- CASE 1: SKIP AND DISPLAY ---
  cat(">>> Found existing plot at:", final_plot_path, "\n")
  cat(">>> Skipping processing (Set overwrite_mode = TRUE to re-run).\n")
  
  # Attempt to display the existing plot in the RStudio Viewer
  if (interactive()) {
    library(magick)
    img <- magick::image_read(final_plot_path)
    print(img)
  }
  
} else {
  
  # --- CASE 2: RUN ANALYSIS ---
  cat(">>> Processing Zonation curves...\n")
  
  if (!file.exists(zonation_results_path)) {
    stop("Error: Could not find Zonation results file at: ", zonation_results_path)
  }
  
  # 1. Read Zonation curves (assuming standard .txt format)
  curves_data <- read_table(zonation_results_path)
  
  # 2. Data Cleaning / Transformation
  # (Placeholder: Adjust column names based on your Zonation output)
  curves_clean <- curves_data %>%
    select(1:10) %>% # Adjust based on number of species/features
    pivot_longer(-`Prop_landscape_lost`, names_to = "Feature", values_to = "Performance")
  
  # 3. Create Plot
  p <- ggplot(curves_clean, aes(x = 1 - Prop_landscape_lost, y = Performance, color = Feature)) +
    geom_line(linewidth = 1) +
    scale_x_reverse() + # Zonation usually goes from 1.0 down to 0
    labs(
      title = "Zonation Performance Curves",
      x = "Proportion of Landscape Protected",
      y = "Proportion of Distribution Represented",
      caption = paste("Source:", basename(zonation_results_path))
    ) +
    theme_minimal() +
    theme(legend.position = "none") # Hide legend if too many features
  
  # 4. Save and Display
  ggsave(final_plot_path, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
  cat("✓ New plot saved to:", final_plot_path, "\n")
  print(p)
}

cat("\n=== ZONATION ANALYSIS COMPLETE ===\n")