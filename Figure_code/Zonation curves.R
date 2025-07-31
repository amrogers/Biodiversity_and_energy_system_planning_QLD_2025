# =============================================================================
# Zonation Conservation Priority Analysis and Visualization
# =============================================================================
# This script analyzes Zonation output to create performance curves showing
# feature coverage across conservation priority ranks, with confidence intervals.
#
# Data requirements:
# - Supplementary data folder containing Zonation_output/250m_SNES_ECNES_red_zones_weighted_QLD/
# - Specifically requires: out_example1/feature_curves.csv
#
# Author: Andrew Rogers
# Date: June 2025
# =============================================================================

# Load necessary libraries
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(gridExtra)

# Optional: Load fonts (comment out if not needed or causing issues)
# library(extrafont)
# loadfonts(device = "win", quiet = TRUE)
# windowsFonts(Arial = windowsFont("TT Arial"))

# =============================================================================
# Setup and Path Configuration
# =============================================================================

# Define paths relative to supplementary data folder
zonation_base_dir <- file.path("Zonation_output", "250m_SNES_ECNES_red_zones_weighted_QLD")
output_folder <- file.path(zonation_base_dir, "out_example1")

# Verify required directories exist
if (!dir.exists(zonation_base_dir)) {
  stop("Error: Cannot find Zonation output directory at: ", zonation_base_dir,
       "\nPlease ensure you are running this script from the supplementary data folder.")
}

if (!dir.exists(output_folder)) {
  stop("Error: Cannot find Zonation analysis output at: ", output_folder,
       "\nPlease ensure the Zonation analysis results are complete.")
}

# Define input file path
input_file <- file.path(output_folder, "feature_curves.csv")

# =============================================================================
# Data Loading and Validation
# =============================================================================

cat("Loading Zonation feature curves data...\n")
cat("Input file:", input_file, "\n")

# Check if input file exists and load data
if (file.exists(input_file)) {
  df <- read.csv(input_file)
  cat("Successfully loaded file with", nrow(df), "rows and", ncol(df), "columns\n")
} else {
  stop("Error: Feature curves file not found at: ", input_file)
}

# Identify feature columns (all columns except 'rank')
feature_cols <- setdiff(names(df), "rank")
cat("Number of conservation features:", length(feature_cols), "\n")

# Validate data structure
if (!"rank" %in% names(df)) {
  stop("Error: 'rank' column not found in data. Please check file format.")
}

if (length(feature_cols) == 0) {
  stop("Error: No feature columns found in data.")
}

cat("Rank range:", min(df$rank, na.rm = TRUE), "to", max(df$rank, na.rm = TRUE), "\n")

# =============================================================================
# Statistical Analysis
# =============================================================================

cat("Calculating summary statistics across features...\n")

# Calculate mean performance across all features
df$mean <- rowMeans(df[, feature_cols], na.rm = TRUE)

# Calculate confidence intervals
df$sd <- apply(df[, feature_cols], 1, sd, na.rm = TRUE)
df$n <- length(feature_cols)
df$se <- df$sd / sqrt(df$n)
df$ci_lower <- df$mean - 1.96 * df$se
df$ci_upper <- df$mean + 1.96 * df$se

# Print summary statistics
summary_stats <- df %>%
  summarise(
    min_mean = min(mean, na.rm = TRUE),
    max_mean = max(mean, na.rm = TRUE),
    min_ci = min(ci_lower, na.rm = TRUE),
    max_ci = max(ci_upper, na.rm = TRUE),
    .groups = "drop"
  )

cat("Summary of calculated statistics:\n")
print(summary_stats)

# =============================================================================
# Visualization: Performance Curves with Confidence Intervals
# =============================================================================

cat("Creating performance curve visualization...\n")

# Reshape dataframe for individual feature lines
df_long <- df %>%
  gather(key = "Feature", value = "Value", all_of(feature_cols))

# Create the main performance curve plot
performance_plot <- ggplot() +
  # Individual feature curves (light grey background)
  geom_line(data = df_long, 
            aes(x = rank, y = Value, group = Feature), 
            color = "grey", alpha = 0.3, linewidth = 0.3) +
  # Confidence interval ribbon
  geom_ribbon(data = df, 
              aes(x = rank, ymin = ci_lower, ymax = ci_upper),
              fill = "lightblue", alpha = 0.4) +
  # Mean performance curve (prominent black line)
  geom_line(data = df, 
            aes(x = rank, y = mean), 
            color = "black", linewidth = 1.2) +
  # Formatting and labels
  labs(
    x = "Priority Rank (0 = highest priority, 1 = lowest priority)",
    y = "Average Feature Coverage",
    title = "Conservation Feature Performance Curves",
    subtitle = paste("Analysis of", length(feature_cols), "conservation features"),
    caption = "Grey lines: individual features; Black line: mean; Blue ribbon: 95% confidence interval"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    text = element_text(size = 14),
    axis.text = element_text(color = "black"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey50"),
    plot.caption = element_text(size = 10, color = "grey50")
  ) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1))

# Display the plot
print(performance_plot)

# =============================================================================
# Feature Coverage Analysis at Different Thresholds
# =============================================================================

cat("Analyzing feature coverage at different priority thresholds...\n")

# Function to count features with full coverage within a rank range
count_full_coverage <- function(data, rank_min, rank_max, threshold = 0.99) {
  filtered_data <- data %>%
    filter(rank >= rank_min & rank <= rank_max)
  
  feature_cols <- setdiff(names(filtered_data), 
                          c("rank", "mean", "sd", "n", "se", "ci_lower", "ci_upper"))
  
  full_coverage_counts <- filtered_data %>%
    select(all_of(feature_cols)) %>%
    summarise(across(everything(), ~max(.x, na.rm = TRUE) >= threshold)) %>%
    summarise(count = sum(c_across(everything()), na.rm = TRUE))
  
  return(full_coverage_counts$count)
}

# Function to get mean statistics at specific rank
get_stats_at_rank <- function(data, target_rank) {
  closest_row <- data %>%
    mutate(rank_diff = abs(rank - target_rank)) %>%
    arrange(rank_diff) %>%
    slice(1)
  
  return(data.frame(
    mean_coverage = closest_row$mean,
    ci_lower = closest_row$ci_lower,
    ci_upper = closest_row$ci_upper
  ))
}

# Define analysis thresholds corresponding to different conservation scenarios
rank_ranges <- data.frame(
  scenario = c("Business as Usual", "90% excluded", "70% excluded", 
               "50% excluded", "30% excluded", "10% excluded"),
  min_rank = c(0.87, 0.9, 0.7, 0.5, 0.3, 0.1),
  max_rank = 1.0,
  stringsAsFactors = FALSE
)

# Calculate coverage statistics for each scenario
results <- rank_ranges %>%
  rowwise() %>%
  mutate(
    features_fully_covered = count_full_coverage(df, min_rank, max_rank, 0.99),
    features_well_covered = count_full_coverage(df, min_rank, max_rank, 0.5),
    features_poorly_covered = {
      filtered_data <- df %>% filter(rank >= min_rank & rank <= max_rank)
      feature_cols <- setdiff(names(filtered_data), 
                              c("rank", "mean", "sd", "n", "se", "ci_lower", "ci_upper"))
      
      poor_counts <- filtered_data %>%
        select(all_of(feature_cols)) %>%
        summarise(across(everything(), ~max(.x, na.rm = TRUE) < 0.5)) %>%
        summarise(count = sum(c_across(everything()), na.rm = TRUE))
      
      as.numeric(poor_counts)
    }
  ) %>%
  ungroup()

# Add mean coverage statistics at minimum rank thresholds
stats_at_ranks <- map_dfr(rank_ranges$min_rank, ~get_stats_at_rank(df, .x))
results <- bind_cols(results, stats_at_ranks)

# Round for presentation
results <- results %>%
  mutate(across(c(mean_coverage, ci_lower, ci_upper), ~round(.x, 3)))

# Display results
cat("\nFeature Coverage Analysis Results:\n")
print(results, width = Inf)

total_features <- length(feature_cols)
cat("\nTotal number of conservation features analyzed:", total_features, "\n")

# =============================================================================
# Save Outputs
# =============================================================================

# Create output directory for figures if it doesn't exist
figures_dir <- "figures"
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE)
  cat("Created figures directory:", figures_dir, "\n")
}

# Save the performance curve plot
plot_filename <- file.path(figures_dir, "zonation_performance_curves.png")
ggsave(
  filename = plot_filename,
  plot = performance_plot,
  width = 10, height = 8, dpi = 300, bg = "white"
)
cat("Performance curve plot saved to:", plot_filename, "\n")

# Save high-resolution TIFF for publication (optional)
tiff_filename <- file.path(figures_dir, "zonation_performance_curves.tif")
ggsave(
  filename = tiff_filename,
  plot = performance_plot,
  width = 8, height = 8, dpi = 600, bg = "white"
)
cat("High-resolution TIFF saved to:", tiff_filename, "\n")

# Save coverage analysis results
results_filename <- file.path(output_folder, "feature_coverage_summary_with_CI.csv")
write.csv(results, results_filename, row.names = FALSE)
cat("Coverage analysis results saved to:", results_filename, "\n")

cat("\nZonation analysis complete!\n")
cat("Summary: Analyzed", total_features, "conservation features across priority ranks.\n")