# =============================================================================
# Energy Cost Increase Analysis and Visualization
# =============================================================================
# This script analyzes and visualizes energy cost increases under different
# High Biodiversity Value Area (HBVA) exclusion scenarios.
#
# Data requirements:
# - Supplementary data folder containing Energy_system_model_outputs/cost_increase_results.csv
# 
# Author: Andrew Rogers
# Date: June 2025
# =============================================================================

# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(here)

# Set up project paths using here package for reproducibility
# This assumes the script is run from the supplementary data folder or a subfolder
if (!dir.exists("Energy_system_model_outputs")) {
  stop("Error: Cannot find 'Energy_system_model_outputs' folder. 
       Please ensure you are running this script from the supplementary data folder 
       or adjust the working directory accordingly.")
}

# Define data file path relative to supplementary data folder
data_file <- file.path("Energy_system_model_outputs", "cost_increase_results.csv")

# Check if data file exists
if (!file.exists(data_file)) {
  stop(paste("Error: Data file not found at", data_file,
             "\nPlease ensure the supplementary data folder is complete."))
}

# Read the data file
cat("Reading cost increase data...\n")
data <- read_csv(data_file, show_col_types = FALSE)

# Data processing and cleaning
cat("Processing data...\n")
plot_data <- data.frame(
  tx_scenario = data[[1]],
  HBVA_exclusion = data[[2]],
  residential = data[[3]],
  industrial = data[[4]]
)

# Clean HBVA values and ensure they match expected categories
plot_data$HBVA_exclusion <- trimws(plot_data$HBVA_exclusion)

# Convert to long format for visualization
plot_data_long <- plot_data %>%
  pivot_longer(
    cols = c(residential, industrial),
    names_to = "cost_type",
    values_to = "cost_increase"
  ) %>%
  # Remove rows with missing cost values
  filter(!is.na(cost_increase)) %>%
  # Filter for expected HBVA exclusion levels
  filter(HBVA_exclusion %in% c("BAU", "30", "50", "70", "90")) %>%
  # Set factor levels to ensure proper ordering on x-axis
  mutate(HBVA_exclusion = factor(HBVA_exclusion, 
                                 levels = c("BAU", "30", "50", "70", "90")))

# Verify data processing
cat("Data summary:\n")
cat("- Transmission scenarios:", paste(unique(plot_data_long$tx_scenario), collapse = ", "), "\n")
cat("- HBVA exclusion levels:", paste(levels(plot_data_long$HBVA_exclusion), collapse = ", "), "\n")
cat("- Cost types:", paste(unique(plot_data_long$cost_type), collapse = ", "), "\n")
cat("- Total observations:", nrow(plot_data_long), "\n\n")

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
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.key.width = unit(3, "cm"),
    legend.key.height = unit(1, "cm"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.caption = element_text(size = 12, color = "grey50")
  )

# Display the plot
print(cost_plot)

# Save plot with relative path
# Create output directory if it doesn't exist
output_dir <- "figures"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

output_file <- file.path(output_dir, "energy_cost_increase_plot.png")
ggsave(
  filename = output_file,
  plot = cost_plot,
  width = 12, 
  height = 6, 
  dpi = 300, 
  bg = "white"
)

cat("Plot saved to:", output_file, "\n")
cat("Analysis complete!\n")