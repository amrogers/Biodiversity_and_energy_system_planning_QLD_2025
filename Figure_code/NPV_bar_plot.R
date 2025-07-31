# =============================================================================
# Net Present Value (NPV) Analysis and Visualization
# =============================================================================
# This script creates bar plots showing NPV of energy infrastructure investments
# under different biodiversity protection scenarios, with components for
# transmission infrastructure and renewable energy projects.
#
# Data requirements:
# - Supplementary data folder containing Energy_system_model_outputs/eplus_Domestic_NPV_2025.xlsx
# - Additional file needed: Build percent_build_vs_trans.xlsx (if available)
#
# Author: Andrwe Rogers 
# Date: June 2025
# =============================================================================

# Load required libraries
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(ggpattern)
library(forcats)
library(cowplot)
library(grid)
library(gridExtra)

# =============================================================================
# Setup and Path Configuration
# =============================================================================

# Define data file paths relative to supplementary data folder
npv_file <- file.path("Energy_system_model_outputs", "eplus_Domestic_NPV_2025.xlsx")

# Note: The percent data file is not in the current supplementary data structure
# If you have this file, place it in the appropriate folder and update the path
percent_file <- "Build percent_build_vs_trans.xlsx"  # Update path as needed

# Verify NPV data file exists
if (!file.exists(npv_file)) {
  stop("Error: NPV data file not found at: ", npv_file,
       "\nPlease ensure you are running this script from the supplementary data folder.")
}

cat("Reading NPV data from:", npv_file, "\n")

# =============================================================================
# Data Loading and Processing
# =============================================================================

# Read NPV data with explicit column types for reliability
npv_data <- read_excel(
  npv_file,
  col_types = c("text", "text", "numeric", "numeric", "numeric")
)

cat("NPV data loaded successfully.\n")
cat("Columns:", paste(names(npv_data), collapse = ", "), "\n")
cat("Scenarios:", paste(unique(npv_data[[1]]), collapse = ", "), "\n")
cat("Thresholds:", paste(unique(npv_data[[2]]), collapse = ", "), "\n")

# Process NPV data and ensure proper threshold ordering
npv_long <- npv_data %>%
  pivot_longer(
    cols = c(`2030`, `2040`, `2050`),
    names_to = "year",
    values_to = "npv_value"
  ) %>%
  # Use column names directly (adjust if your column names differ)
  rename(
    scenario = 1,
    thresholds = 2
  ) %>%
  # Filter out 0.1 threshold if present (as per original script)
  filter(thresholds != "0.1") %>%
  # Set factor levels for proper ordering
  mutate(thresholds = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9")))

# =============================================================================
# Handle Build Percentage Data (Optional)
# =============================================================================

# Check if percentage data file exists
if (file.exists(percent_file)) {
  cat("Reading build percentage data from:", percent_file, "\n")
  
  percent_data <- read_excel(
    percent_file,
    col_types = c("text", "text", "numeric", "numeric", "numeric")
  )
  
  # Process percent data
  percent_long <- percent_data %>%
    pivot_longer(
      cols = c(`2030`, `2040`, `2050`),
      names_to = "year",
      values_to = "build_percent"
    ) %>%
    rename(scenario = 1, thresholds = 2) %>%
    filter(thresholds != "0.1")
  
  # Combine datasets
  combined_data <- npv_long %>%
    left_join(percent_long, by = c("scenario", "thresholds", "year")) %>%
    mutate(
      build_value = npv_value * (build_percent / 100),  # Convert percent to proportion
      thresholds = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9"))
    )
  
  use_build_data <- TRUE
  cat("Build percentage data integrated successfully.\n")
  
} else {
  cat("Warning: Build percentage data file not found at:", percent_file, "\n")
  cat("Creating visualization with NPV data only.\n")
  
  # Use NPV data only
  combined_data <- npv_long
  use_build_data <- FALSE
}

# =============================================================================
# Data Validation and Summary
# =============================================================================

cat("\nData Summary:\n")
cat("- Scenarios:", paste(unique(combined_data$scenario), collapse = ", "), "\n")
cat("- Threshold levels:", paste(levels(combined_data$thresholds), collapse = ", "), "\n")
cat("- Years:", paste(unique(combined_data$year), collapse = ", "), "\n")
cat("- Total observations:", nrow(combined_data), "\n")

if (use_build_data) {
  cat("- Build component analysis: Enabled\n")
} else {
  cat("- Build component analysis: Disabled (data not available)\n")
}

# =============================================================================
# Create Visualization
# =============================================================================

cat("\nCreating NPV visualization...\n")

# Base plot setup
p <- ggplot(combined_data, aes(x = factor(year)))

if (use_build_data) {
  # Create plot with build components
  p <- p +
    # Base bars (total NPV)
    geom_col_pattern(
      aes(y = npv_value, fill = thresholds),
      pattern = 'none',
      colour = "black",
      position = position_dodge(width = 0.9),
      width = 0.8
    ) +
    # Overlay pattern bars (build portion)
    geom_col_pattern(
      aes(y = build_value, fill = thresholds),
      pattern = 'stripe',
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      colour = "black",
      position = position_dodge(width = 0.9),
      width = 0.8
    )
} else {
  # Simple bars without build components
  p <- p +
    geom_col(
      aes(y = npv_value, fill = thresholds),
      colour = "black",
      position = position_dodge(width = 0.9),
      width = 0.8
    )
}

# Add common plot elements
p <- p +
  facet_wrap(~scenario, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "NPV (billion AUD)",
    fill = "Protection Threshold",
    title = "Net Present Value of Energy Infrastructure Investments",
    subtitle = "Under different biodiversity protection scenarios",
    caption = if (use_build_data) {
      "Solid bars: Total NPV; Striped pattern: Solar and wind projects component"
    } else {
      "Source: Energy system modeling results"
    }
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 18),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 16, color = "grey50"),
    plot.caption = element_text(size = 12, color = "grey50"),
    legend.key.size = unit(1.2, "cm")
  ) +
  scale_fill_brewer(
    palette = "RdYlBu", 
    direction = -1,
    labels = c("BAU" = "Business as Usual", 
               "0.3" = "30% Protected", 
               "0.5" = "50% Protected",
               "0.7" = "70% Protected", 
               "0.9" = "90% Protected")
  )

# =============================================================================
# Create Legend Components (if using build data)
# =============================================================================

if (use_build_data) {
  # Adjust legend to remove pattern from threshold legend
  p <- p + guides(
    fill = guide_legend(
      override.aes = list(pattern = "none"),
      title = "Protection Threshold"
    )
  )
  
  # Create component legend
  component_legend_data <- data.frame(
    x = c(1, 1),
    y = c(1, 2),
    component = c("Transmission Infrastructure", "Solar & Wind Projects")
  )
  
  legend_plot <- ggplot(component_legend_data, aes(x = x, y = y, fill = component)) +
    geom_col_pattern(
      aes(pattern = component),
      colour = "black",
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      width = 0.7
    ) +
    scale_pattern_manual(
      values = c("Transmission Infrastructure" = "none", 
                 "Solar & Wind Projects" = "stripe"),
      name = "Component"
    ) +
    scale_fill_manual(
      values = c("Transmission Infrastructure" = "white", 
                 "Solar & Wind Projects" = "white"),
      name = "Component"
    ) +
    theme_void() +
    theme(
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18),
      legend.key.size = unit(1.2, "cm")
    )
  
  # Extract component legend
  component_legend <- get_legend(legend_plot)
  
  # Create final plot with both legends
  threshold_legend <- get_legend(p)
  
  final_plot <- plot_grid(
    p + theme(legend.position = "none"),
    plot_grid(
      threshold_legend,
      component_legend,
      ncol = 1,
      align = 'v',
      rel_heights = c(3, 2)
    ),
    rel_widths = c(3.5, 1),
    align = "h",
    nrow = 1
  )
} else {
  final_plot <- p
}

# Display the plot
print(final_plot)

# =============================================================================
# Save Outputs
# =============================================================================

# Create output directory
figures_dir <- "figures"
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
  cat("Created figures directory:", figures_dir, "\n")
}

# Save the plot
output_filename <- file.path(figures_dir, "npv_analysis_plot.png")
ggsave(
  plot = final_plot,
  filename = output_filename,
  width = if (use_build_data) 16 else 12,
  height = 8,
  dpi = 300,
  bg = "white",
  units = "in"
)

cat("NPV analysis plot saved to:", output_filename, "\n")

# Save data summary
summary_data <- combined_data %>%
  group_by(scenario, thresholds, year) %>%
  summarise(
    mean_npv = mean(npv_value, na.rm = TRUE),
    .groups = "drop"
  )

summary_filename <- file.path(figures_dir, "npv_summary_data.csv")
write.csv(summary_data, summary_filename, row.names = FALSE)
cat("NPV summary data saved to:", summary_filename, "\n")

cat("\nNPV analysis complete!\n")