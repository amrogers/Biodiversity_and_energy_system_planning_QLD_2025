# =============================================================================
# Net Present Value (NPV) Analysis and Visualization
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================

# Load required libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, readxl, tidyr, dplyr, ggpattern, forcats, cowplot,
               grid, gridExtra, here)
source(here::here("_paths.R"))

# --- USER CONTROL SETTINGS ---
overwrite_mode <- FALSE

# =============================================================================
# Setup and Path Configuration
# =============================================================================

data_dir        <- file.path(data_root, "Energy_system_model_outputs")
output_dir      <- here("results", "figures")
npv_file        <- file.path(data_dir, "eplus_Domestic_NPV_2025.xlsx")
percent_file    <- file.path(data_root, "Build percent_build_vs_trans.xlsx")
output_filename <- file.path(output_dir, "npv_analysis_plot.png")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# =============================================================================
# Load and Process Data (always runs)
# =============================================================================

if (!file.exists(npv_file)) {
  stop("Error: NPV data file not found at: ", npv_file,
       "\nPlease ensure the Figshare data is in the /data folder.")
}

cat(">>> Loading NPV data...\n")
npv_data <- read_excel(npv_file, col_types = c("text", "text", "numeric", "numeric", "numeric"))

npv_long <- npv_data %>%
  pivot_longer(cols = c(`2030`, `2040`, `2050`), names_to = "year", values_to = "npv_value") %>%
  rename(scenario = 1, thresholds = 2) %>%
  filter(thresholds != "0.1") %>%
  mutate(thresholds = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9")))

if (file.exists(percent_file)) {
  cat(">>> Loading build percentage data...\n")
  percent_data <- read_excel(percent_file, col_types = c("text", "text", "numeric", "numeric", "numeric"))

  percent_long <- percent_data %>%
    pivot_longer(cols = c(`2030`, `2040`, `2050`), names_to = "year", values_to = "build_percent") %>%
    rename(scenario = 1, thresholds = 2) %>%
    filter(thresholds != "0.1")

  combined_data <- npv_long %>%
    left_join(percent_long, by = c("scenario", "thresholds", "year")) %>%
    mutate(
      build_value = npv_value * (build_percent / 100),
      thresholds  = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9"))
    )
  use_build_data <- TRUE
} else {
  cat("Warning: Build percentage data file not found. Plotting NPV only.\n")
  combined_data  <- npv_long
  use_build_data <- FALSE
}

# =============================================================================
# Build Plot (always runs)
# =============================================================================

cat(">>> Building NPV plot...\n")
p <- ggplot(combined_data, aes(x = factor(year)))

if (use_build_data) {
  p <- p +
    geom_col_pattern(aes(y = npv_value, fill = thresholds),
                     pattern = "none", colour = "black",
                     position = position_dodge(width = 0.9), width = 0.8) +
    geom_col_pattern(aes(y = build_value, fill = thresholds),
                     pattern = "stripe", pattern_fill = "black",
                     pattern_angle = 45, pattern_density = 0.1, pattern_spacing = 0.025,
                     colour = "black", position = position_dodge(width = 0.9), width = 0.8)
} else {
  p <- p + geom_col(aes(y = npv_value, fill = thresholds), colour = "black",
                    position = position_dodge(width = 0.9), width = 0.8)
}

final_plot <- p +
  facet_wrap(~scenario, scales = "free_y") +
  theme_minimal() +
  labs(
    x       = "Year",
    y       = "NPV (billion AUD)",
    fill    = "Protection Threshold",
    title   = "Net Present Value of Energy Infrastructure Investments",
    caption = "Source: Energy system modeling results"
  ) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1)

# =============================================================================
# Save (only if needed) and Display (always)
# =============================================================================

if (!file.exists(output_filename) || overwrite_mode) {
  ggsave(output_filename, plot = final_plot, width = 14, height = 8, dpi = 300, bg = "white")
  summary_filename <- file.path(output_dir, "npv_summary_data.csv")
  write.csv(combined_data, summary_filename, row.names = FALSE)
  cat("✓ NPV plot and summary saved to:", output_dir, "\n")
} else {
  cat(">>> Plot already exists (set overwrite_mode = TRUE to regenerate).\n")
}

print(final_plot)

cat("\n=== NPV ANALYSIS COMPLETE ===\n")
