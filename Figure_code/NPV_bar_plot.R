# =============================================================================
# Figure 2: Net Present Value (NPV) Analysis and Visualization
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: Creates Figure 2 — stacked bar plots showing Net Present Value of
#          VRE and transmission infrastructure investments by year (2030/2040/2050)
#          and biodiversity avoidance threshold, faceted by TX scenario.
#
# Input:  BESP_data_qld_2025/Energy_system_model_outputs/
#           eplus_Domestic_NPV_figure.csv
#
# Output: results/figures/npv_analysis_plot.png
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, readr, tidyr, dplyr, ggpattern, scales, RColorBrewer, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

npv_file        <- file.path(paths$energy_outputs, "eplus_Domestic_NPV_figure.csv")
out_dir         <- here("results", "figures")
output_filename <- file.path(out_dir, "npv_analysis_plot.png")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =============================================================================
# 2. Load and Process Data
# =============================================================================

if (!file.exists(npv_file)) {
  stop(
    "Input file not found:\n  ", npv_file, "\n\n",
    "Expected: BESP_data_qld_2025/Energy_system_model_outputs/eplus_Domestic_NPV_figure.csv"
  )
}

cat(">>> Loading NPV data...\n")

npv_data <- read_csv(npv_file, show_col_types = FALSE)

npv_long <- npv_data %>%
  rename(scenario = TX_scenario, thresholds = BV_protection_scenario) %>%
  select(scenario, thresholds, starts_with("VRE_"), starts_with("TX_")) %>%
  pivot_longer(
    cols      = -c(scenario, thresholds),
    names_to  = c("component", "year"),
    names_sep = "_",
    values_to = "npv_value"
  ) %>%
  mutate(
    thresholds = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9")),
    year       = factor(year, levels = c("2030", "2040", "2050")),
    scenario   = factor(scenario, levels = c("tx1", "tx2"),
                        labels = c("TX Scenario 1", "TX Scenario 2")),
    component  = factor(component, levels = c("VRE", "TX"))  # VRE bottom, TX top
  )

# =============================================================================
# 3. Pre-compute Stacking Positions
# =============================================================================

n_thresh  <- 5
bar_width <- 0.12
gap       <- 0.02

thresh_levels  <- c("BAU", "0.3", "0.5", "0.7", "0.9")
thresh_offsets <- seq(
  from       = -((n_thresh - 1) / 2) * (bar_width + gap),
  by         = bar_width + gap,
  length.out = n_thresh
)
offset_map <- setNames(thresh_offsets, thresh_levels)
year_pos   <- setNames(c(1, 2, 3), c("2030", "2040", "2050"))

npv_rect <- npv_long %>%
  arrange(scenario, year, thresholds, component) %>%
  group_by(scenario, year, thresholds) %>%
  mutate(
    ymax = cumsum(npv_value),
    ymin = ymax - npv_value
  ) %>%
  ungroup() %>%
  mutate(
    x_centre = year_pos[as.character(year)] + offset_map[as.character(thresholds)],
    xmin     = x_centre - bar_width / 2,
    xmax     = x_centre + bar_width / 2
  )

# =============================================================================
# 4. Fix Y-axis to TX2 Max
# =============================================================================

tx2_max <- npv_rect %>%
  filter(scenario == "TX Scenario 2") %>%
  summarise(max_y = max(ymax)) %>%
  pull(max_y)

y_upper <- ceiling(tx2_max / 10) * 10
cat(">>> Y-axis upper limit set to:", y_upper, "\n")

# =============================================================================
# 5. Colour Palette
# =============================================================================

threshold_colours <- setNames(
  brewer.pal(5, "RdYlBu")[5:1],
  thresh_levels
)

# =============================================================================
# 6. Build Plot
# =============================================================================

cat(">>> Building NPV plot...\n")

final_plot <- ggplot(npv_rect) +
  geom_rect_pattern(
    aes(
      xmin    = xmin,
      xmax    = xmax,
      ymin    = ymin,
      ymax    = ymax,
      fill    = thresholds,
      pattern = component
    ),
    colour                   = "black",
    linewidth                = 0.3,
    pattern_fill             = "black",
    pattern_colour           = "black",
    pattern_angle            = 45,
    pattern_density          = 0.1,
    pattern_spacing          = 0.025,
    pattern_key_scale_factor = 0.6
  ) +
  facet_wrap(~scenario) +
  scale_fill_manual(
    values = threshold_colours,
    name   = "Avoidance\nThreshold"
  ) +
  scale_pattern_manual(
    values = c(VRE = "stripe", TX = "none"),
    name   = "Component",
    labels = c(VRE = "VRE", TX = "Transmission (TX)")
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("2030", "2040", "2050"),
    limits = c(0.6, 3.4)
  ) +
  scale_y_continuous(
    labels = label_comma(),
    limits = c(0, y_upper),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    x       = "Year",
    y       = "NPV (billion AUD)",
    caption = paste("Source:", basename(npv_file))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.caption     = element_text(colour = "grey60", size = 8),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold"),
    legend.position  = "right"
  ) +
  guides(
    pattern = guide_legend(order = 2, override.aes = list(fill = "white")),
    fill    = guide_legend(order = 1, override.aes = list(pattern = "none"))
  )

# =============================================================================
# 7. Save and Display
# =============================================================================

if (!file.exists(output_filename) || overwrite_mode) {
  ggsave(output_filename, plot = final_plot, width = 16, height = 8, dpi = 300, bg = "white")
  cat("✓ Plot saved to:", output_filename, "\n")
} else {
  cat(">>> Plot already exists (set overwrite_mode = TRUE to regenerate).\n")
}

print(final_plot)
cat("\n=== NPV ANALYSIS COMPLETE ===\n")
