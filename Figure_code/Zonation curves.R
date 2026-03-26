# =============================================================================
# Zonation Performance Curves Analysis
# =============================================================================
# Description: Processes Zonation feature_curves.csv to plot performance curves.
#              Individual species shown as light grey lines; national mean shown
#              as a bold black line with 95% confidence intervals.
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
thin_rows      <- FALSE   # Set TRUE to thin rank steps for faster rendering (large datasets)
thin_every_n   <- 5       # Keep every nth row when thinning (only used if thin_rows = TRUE)

# =============================================================================
# Setup and Path Configuration
# =============================================================================
zonation_dir <- file.path(data_root, "Zonation_analysis", "Zonation_output")
output_dir   <- here("results", "zonation_figures")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

zonation_results_path <- file.path(zonation_dir, "250m_QLD_2024", "out_example1", "feature_curves.csv")
final_plot_path       <- file.path(output_dir, "zonation_performance_curves.png")

# =============================================================================
# Load and Process Data (always runs)
# =============================================================================
if (!file.exists(zonation_results_path)) {
  stop("Error: Could not find Zonation results file at: ", zonation_results_path)
}

cat(">>> Loading Zonation data...\n")
curves_data <- read_csv(zonation_results_path, show_col_types = FALSE)

n_species <- ncol(curves_data) - 1  # exclude rank column
cat(">>> Found", n_species, "species/features in file.\n")

# Optionally thin rows to speed up rendering with large datasets
if (thin_rows) {
  cat(">>> Thinning rows: keeping every", thin_every_n, "rows...\n")
  curves_data <- curves_data %>%
    filter(row_number() %% thin_every_n == 0)
}

# Pivot ALL species — no select() restriction
curves_clean <- curves_data %>%
  pivot_longer(-rank, names_to = "Feature", values_to = "Performance")

cat(">>> Pivoted data:", nrow(curves_clean), "rows (", n_species, "species x", nrow(curves_data), "rank steps).\n")

# =============================================================================
# Compute National Mean and 95% Confidence Intervals
# =============================================================================
cat(">>> Computing national mean and 95% CI...\n")

curves_summary <- curves_clean %>%
  group_by(rank) %>%
  summarise(
    mean_perf = mean(Performance, na.rm = TRUE),
    sd_perf   = sd(Performance,   na.rm = TRUE),
    n         = n(),
    se_perf   = sd_perf / sqrt(n),
    ci_lower  = mean_perf - 1.96 * se_perf,
    ci_upper  = mean_perf + 1.96 * se_perf,
    .groups   = "drop"
  ) %>%
  # Clamp CI to [0, 1]
  mutate(
    ci_lower = pmax(ci_lower, 0),
    ci_upper = pmin(ci_upper, 1)
  )

# =============================================================================
# Build Plot (always runs)
# =============================================================================
cat(">>> Building plot...\n")

p <- ggplot() +
  # Individual species lines — light grey, thin, semi-transparent
  geom_line(
    data      = curves_clean,
    aes(x = 1 - rank, y = Performance, group = Feature),
    colour    = "grey80",
    linewidth = 0.3,
    alpha     = 0.4
  ) +
  # 95% CI ribbon around the mean
  geom_ribbon(
    data = curves_summary,
    aes(x = 1 - rank, ymin = ci_lower, ymax = ci_upper),
    fill  = "black",
    alpha = 0.15
  ) +
  # National mean line
  geom_line(
    data      = curves_summary,
    aes(x = 1 - rank, y = mean_perf),
    colour    = "black",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    labels = scales::percent_format()
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format()
  ) +
  labs(
    title    = "Zonation Performance Curves",
    subtitle = paste0(n_species, " species — bold line = national mean ± 95% CI"),
    x        = "Proportion of Landscape Protected",
    y        = "Proportion of Distribution Represented",
    caption  = paste("Source:", basename(zonation_results_path))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey60", size = 8),
    panel.grid.minor = element_blank()
  )

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