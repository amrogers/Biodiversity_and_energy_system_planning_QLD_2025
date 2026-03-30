# =============================================================================
# Figure 3: Total New Transmission Length vs Biodiversity Avoidance
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Mar 2026
# =============================================================================
# Purpose: Generates Figure 3 — total new transmission line length (km)
#          required to achieve net-zero 2050 under increasing biodiversity
#          protection, comparing TX1 (reference cost) and TX2 (doubled TX
#          cost) scenarios. A horizontal reference line marks the existing
#          QLD transmission network baseline.
#
# Input:  BESP_data_qld_2025/Energy_system_model_outputs/
#           tx1_new_transmission_summary.csv
#           tx2_new_transmission_summary.csv
#         These are pre-computed by QLD_new_tx_processing_summary.R.
#         Copy each scenario's 'QLD_threshold_tx_new_summary.csv' to the
#         above location and rename to tx1_... / tx2_... respectively.
#
# Output: results/figures/transmission_length_tx1_tx2.png
#         results/tables/tx_length_summary.csv
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, readr, stringr, scales, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

tx1_csv  <- file.path(paths$energy_outputs, "tx1_new_transmission_summary.csv")
tx2_csv  <- file.path(paths$energy_outputs, "tx2_new_transmission_summary.csv")

out_dir  <- here("results", "figures")
tbl_dir  <- here("results", "tables")
out_plot <- file.path(out_dir, "transmission_length_tx1_tx2.png")
out_tbl  <- file.path(tbl_dir, "tx_length_summary.csv")

for (d in c(out_dir, tbl_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Existing QLD transmission network baseline (km)
EXISTING_TX_KM <- 1900

# Scenario mapping: threshold string → paper label
# Threshold 10 is excluded — consistent with other published figures
THRESHOLD_MAP <- c(
  "0"  = "BAU",
  "30" = "Top 30%",
  "50" = "Top 50%",
  "70" = "Top 70%",
  "90" = "Top 90%"
)

SCENARIO_LEVELS <- c("BAU", "Top 30%", "Top 50%", "Top 70%", "Top 90%")

# =============================================================================
# 2. Load and Process Data
# =============================================================================
# CSV schema produced by QLD_new_tx_processing_summary.R:
#   layer_name : character — e.g. "threshold_0", "threshold_30"
#   capacity   : numeric  — voltage class (kV)
#   length_km  : numeric  — total new TX length for this kV at this threshold
#
# We sum across all kV classes to get total new TX length per threshold.

load_and_sum <- function(csv_path, tx_label) {

  if (!file.exists(csv_path)) {
    stop(
      "Input file not found:\n  ", csv_path, "\n\n",
      "Run 'QLD_new_tx_processing_summary.R' for the relevant scenario,\n",
      "then copy the output 'QLD_threshold_tx_new_summary.csv' to:\n  ",
      dirname(csv_path), "\n",
      "and rename it to '", basename(csv_path), "'."
    )
  }

  read_csv(csv_path, show_col_types = FALSE) %>%
    mutate(threshold = str_extract(layer_name, "\\d+$")) %>%
    filter(threshold %in% names(THRESHOLD_MAP)) %>%
    group_by(threshold) %>%
    summarise(total_length_km = sum(length_km, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(
      scenario    = factor(THRESHOLD_MAP[threshold], levels = SCENARIO_LEVELS),
      tx_scenario = tx_label
    ) %>%
    select(scenario, total_length_km, tx_scenario)
}

cat(">>> Loading TX1 summary...\n")
tx1 <- load_and_sum(tx1_csv, "TX1 (Reference cost)")

cat(">>> Loading TX2 summary...\n")
tx2 <- load_and_sum(tx2_csv, "TX2 (Doubled TX cost)")

plot_data <- bind_rows(tx1, tx2) %>%
  mutate(tx_scenario = factor(
    tx_scenario,
    levels = c("TX1 (Reference cost)", "TX2 (Doubled TX cost)")
  ))

cat(">>> Rows loaded:", nrow(plot_data), "\n")
print(plot_data)

# Save intermediate summary table
if (!file.exists(out_tbl) || overwrite_mode) {
  write_csv(plot_data, out_tbl)
  cat("✓ Summary table saved to:", out_tbl, "\n")
}

# =============================================================================
# 3. Build Plot
# =============================================================================

cat(">>> Building plot...\n")

TX_COLOURS <- c(
  "TX1 (Reference cost)"  = "#2166AC",
  "TX2 (Doubled TX cost)" = "#D6604D"
)

p <- ggplot(plot_data,
            aes(x      = scenario,
                y      = total_length_km,
                colour = tx_scenario,
                group  = tx_scenario)) +

  # Existing TX network reference line
  geom_hline(
    yintercept = EXISTING_TX_KM,
    linetype   = "dashed",
    linewidth  = 0.7,
    colour     = "grey50"
  ) +
  annotate(
    "text",
    x      = 0.55,
    y      = EXISTING_TX_KM,
    label  = paste0("Existing network (", scales::comma(EXISTING_TX_KM), " km)"),
    hjust  = 0,
    vjust  = -0.45,
    size   = 3.5,
    colour = "grey40"
  ) +

  # Scenario lines and points
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +

  # Scales
  scale_colour_manual(
    values = TX_COLOURS,
    name   = "Transmission scenario"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0.05, 0.12))
  ) +

  # Labels
  labs(
    x       = "Level of biodiversity avoidance",
    y       = "Total new transmission length (km)",
    caption = paste(
      "Source: QLD_new_tx_processing_summary.R;",
      "pre-computed from QLD_v202412_eplus_tx1/tx2.gdb"
    )
  ) +

  coord_cartesian(clip = "off") +

  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(size = 11),
    axis.title       = element_text(size = 13),
    axis.text        = element_text(size = 11),
    plot.caption     = element_text(colour = "grey60", size = 8),
    plot.margin      = margin(t = 10, r = 10, b = 10, l = 10)
  )

# =============================================================================
# 4. Save and Display
# =============================================================================

if (!file.exists(out_plot) || overwrite_mode) {
  ggsave(out_plot, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
  cat("✓ Plot saved to:", out_plot, "\n")
} else {
  cat(">>> Plot already exists (set overwrite_mode = TRUE to regenerate).\n")
}

print(p)
cat("\n=== TRANSMISSION LENGTH ANALYSIS COMPLETE ===\n")
