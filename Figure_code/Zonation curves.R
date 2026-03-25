# =============================================================================
# Zonation Performance Curves Analysis
# =============================================================================
# Description: Processes Zonation .run files to plot performance curves.
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyr, readr, here, purrr)
source(here::here("_paths.R"))

# --- USER CONTROL SETTINGS ---
overwrite_mode <- FALSE

# =============================================================================
# Setup and Path Configuration
# =============================================================================

zonation_dir <- file.path(data_root, "Zonation_analysis", "Zonation_output")
output_dir   <- here("results", "zonation_figures")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Adjust filename as per your specific Zonation output
zonation_results_path <- file.path(zonation_dir, "250m_SNES_ECNES_red_zones_weighted_QLD", "out_example1", "results.txt")
final_plot_path       <- file.path(output_dir, "zonation_performance_curves.png")

# =============================================================================
# Load and Process Data (always runs)
# =============================================================================

if (!file.exists(zonation_results_path)) {
  stop("Error: Could not find Zonation results file at: ", zonation_results_path)
}

cat(">>> Loading Zonation data...\n")
curves_data <- read_table(zonation_results_path)

# (Adjust column selection based on your Zonation output)
curves_clean <- curves_data %>%
  select(1:10) %>%
  pivot_longer(-`Prop_landscape_lost`, names_to = "Feature", values_to = "Performance")

# =============================================================================
# Build Plot (always runs)
# =============================================================================

p <- ggplot(curves_clean, aes(x = 1 - Prop_landscape_lost, y = Performance, color = Feature)) +
  geom_line(linewidth = 1) +
  scale_x_reverse() +
  labs(
    title   = "Zonation Performance Curves",
    x       = "Proportion of Landscape Protected",
    y       = "Proportion of Distribution Represented",
    caption = paste("Source:", basename(zonation_results_path))
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# =============================================================================
# Save (only if needed) and Display (always)
# =============================================================================

if (!file.exists(final_plot_path) || overwrite_mode) {
  ggsave(final_plot_path, plot = p, width = 8, height = 6, dpi = 300, bg = "white")
  cat("✓ Plot saved to:", final_plot_path, "\n")
} else {
  cat(">>> Plot already exists (set overwrite_mode = TRUE to regenerate).\n")
}

print(p)

cat("\n=== ZONATION ANALYSIS COMPLETE ===\n")
