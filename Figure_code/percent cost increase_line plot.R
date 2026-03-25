# =============================================================================
# Energy Cost Increase Analysis and Visualization
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================

# Load required libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, readr, dplyr, tidyr, here)
source(here::here("_paths.R"))

# --- USER CONTROL SETTINGS ---
overwrite_mode <- FALSE

# =============================================================================
# Setup and Path Configuration
# =============================================================================

data_file   <- file.path(data_root, "Energy_system_model_outputs", "cost_increase_results.csv")
output_dir  <- here("results", "figures")
output_file <- file.path(output_dir, "energy_cost_increase_plot.png")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# =============================================================================
# Load and Process Data (always runs)
# =============================================================================

if (!file.exists(data_file)) {
  stop(paste("Error: Data file not found at:", data_file,
             "\nPlease ensure the Figshare data is in the /data folder."))
}

cat(">>> Loading cost increase data...\n")
data <- read_csv(data_file, show_col_types = FALSE)

plot_data_long <- data.frame(
  tx_scenario    = data[[1]],
  HBVA_exclusion = data[[2]],
  residential    = data[[3]],
  industrial     = data[[4]]
) %>%
  mutate(HBVA_exclusion = trimws(HBVA_exclusion)) %>%
  pivot_longer(
    cols      = c(residential, industrial),
    names_to  = "cost_type",
    values_to = "cost_increase"
  ) %>%
  filter(!is.na(cost_increase)) %>%
  filter(HBVA_exclusion %in% c("BAU", "30", "50", "70", "90")) %>%
  mutate(HBVA_exclusion = factor(HBVA_exclusion,
                                 levels = c("BAU", "30", "50", "70", "90")))

# =============================================================================
# Build Plot (always runs)
# =============================================================================

cat(">>> Building cost plot...\n")
cost_plot <- ggplot(plot_data_long,
                    aes(x       = HBVA_exclusion,
                        y       = cost_increase,
                        group   = interaction(tx_scenario, cost_type),
                        color   = cost_type,
                        linetype = tx_scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_linetype_manual(
    values = c("tx_1" = "solid", "tx_2" = "longdash"),
    name   = "Transmission Scenario",
    labels = c("tx_1" = "Scenario 1", "tx_2" = "Scenario 2")
  ) +
  scale_color_brewer(
    palette = "Set1",
    name    = "Cost Type",
    labels  = c("residential" = "Residential", "industrial" = "Industrial")
  ) +
  theme_minimal() +
  labs(
    x       = "% High Biodiversity Value Area Excluded",
    y       = "Increase in Energy Costs (%)",
    caption = "Data source: Energy system modeling results"
  ) +
  theme(
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 16),
    legend.position = "right"
  )

# =============================================================================
# Save (only if needed) and Display (always)
# =============================================================================

if (!file.exists(output_file) || overwrite_mode) {
  ggsave(output_file, plot = cost_plot, width = 12, height = 6, dpi = 300, bg = "white")
  cat("✓ Plot saved to:", output_file, "\n")
} else {
  cat(">>> Plot already exists (set overwrite_mode = TRUE to regenerate).\n")
}

print(cost_plot)

cat("\n=== ANALYSIS COMPLETE ===\n")
