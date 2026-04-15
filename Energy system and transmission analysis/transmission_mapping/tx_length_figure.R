# =============================================================================
# Figure 3: New Transmission Build Length vs Biodiversity Avoidance
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Mar 2026
# =============================================================================
# Purpose: Generates Figure 3 and the supporting summary table. Reads the
#          pre-computed new-build-only transmission length CSVs for TX1 and
#          TX2 scenarios, sums across voltage classes per biodiversity
#          threshold, maps thresholds to paper labels, and produces:
#
#   - results/tables/tx_new_build_length_tx1_tx2.csv  (wide format summary)
#   - results/figures/tx_length_figure.png             (Figure 3)
#
# Pipeline position: Step 4 of 4 for Figure 3
#   Step 1 → Transmission_processing.R
#   Step 2 → Transmission_save_layers_as_shapefiles.R
#   Step 3 → QLD_new_tx_processing_summary.R
#   Step 4 → THIS SCRIPT
#
# Input files (paths$tx1_new_summary / paths$tx2_new_summary in _paths.R):
#   Tx_outputs/tx1_domestic_transmission/QLD_threshold_tx_new/
#       QLD_threshold_tx1_new_summary.csv
#   Tx_outputs/tx2_domestic_transmission/QLD_threshold_tx_new/
#       QLD_threshold_tx2_new_summary.csv
#
# CSV schema:
#   layer_name  : threshold_0 | threshold_10 | threshold_30 | ... | threshold_90
#   capacity    : voltage class (kV) — summed over / ignored in final table
#   length_km   : new-build-only TX length for this kV class at this threshold
#
# Threshold → biodiversity scenario mapping (threshold_10 excluded):
#   threshold_0  → BAU       (no biodiversity constraint)
#   threshold_10 → Top 90%   (10% excluded)
#   threshold_30 → Top 70%   (30% excluded)
#   threshold_50 → Top 50%   (50% excluded)
#   threshold_70 → Top 30%   (70% excluded)
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, readr, tidyr, here, extrafont)

# Load Arial font — requires extrafont::font_import() to have been run once
if ("Arial" %in% fonts()) {
  loadfonts(device = "win", quiet = TRUE)
} else {
  message(
    "Arial not found in font database. Run the following once in your console:\n",
    "  library(extrafont); font_import(); loadfonts(device = 'win')\n",
    "Falling back to default font."
  )
}

source(here::here("_paths.R"))

# Load local path overrides if present (not committed to git)
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# --- USER CONTROL ---
if (!exists("overwrite_mode")) overwrite_mode <- TRUE

# =============================================================================
# 1. Path Configuration
# =============================================================================

out_dir  <- here("results", "figures")
tbl_dir  <- here("results", "tables")
out_plot <- file.path(out_dir, "tx_length_figure.png")
out_tbl  <- file.path(tbl_dir, "tx_new_build_length_tx1_tx2.csv")

for (d in c(out_dir, tbl_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# =============================================================================
# 2. Threshold → Scenario Label Mapping
# =============================================================================

THRESHOLD_MAP <- c(
  "threshold_0"  = "BAU",
  "threshold_10" = "Top 90%",
  "threshold_70" = "Top 30%",
  "threshold_50" = "Top 50%",
  "threshold_30" = "Top 70%"
)

SCENARIO_LEVELS <- c("BAU", "Top 30%", "Top 50%", "Top 70%", "Top 90%")

# =============================================================================
# 3. Load and Summarise (sum length_km across voltage classes per threshold)
# =============================================================================

load_and_sum <- function(csv_path, tx_label) {
  if (!file.exists(csv_path)) {
    stop(
      "Input file not found:\n  ", csv_path, "\n\n",
      "Run 'QLD_new_tx_processing_summary.R' for the relevant TX scenario,\n",
      "then place the output 'QLD_threshold_tx_new_summary.csv' at:\n  ",
      csv_path
    )
  }
  
  read_csv(csv_path, show_col_types = FALSE) %>%
    filter(layer_name %in% names(THRESHOLD_MAP)) %>%
    group_by(layer_name) %>%
    summarise(total_length_km = sum(length_km, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      bv_scenario = factor(THRESHOLD_MAP[layer_name], levels = SCENARIO_LEVELS),
      tx_scenario = tx_label
    )
}

cat(">>> Loading TX1 summary...\n")
tx1 <- load_and_sum(paths$tx1_new_summary, "TX1 (Reference cost)")

cat(">>> Loading TX2 summary...\n")
tx2 <- load_and_sum(paths$tx2_new_summary, "TX2 (Doubled TX cost)")

TX_LEVELS <- c("TX1 (Reference cost)", "TX2 (Doubled TX cost)")

plot_data <- bind_rows(tx1, tx2) %>%
  mutate(tx_scenario = factor(tx_scenario, levels = TX_LEVELS))

cat(">>> Rows loaded:", nrow(plot_data), "\n")
print(plot_data)

# =============================================================================
# 3b. Load Existing Transmission Length (fixed baseline)
# =============================================================================

cat(">>> Loading existing transmission...\n")

existing_tx_csv <- file.path(dirname(paths$existing_tx),
                             "QLD_exisiting_tx_projected.csv")

if (!file.exists(existing_tx_csv)) {
  warning("Existing TX csv not found — baseline line will be omitted:\n  ",
          existing_tx_csv)
  existing_tx_km <- NULL
} else {
  existing_tx_km <- read_csv(existing_tx_csv, show_col_types = FALSE) %>%
    summarise(total_km = sum(length_km, na.rm = TRUE)) %>%
    pull(total_km)
  cat(">>> Existing TX total length:", scales::comma(existing_tx_km), "km\n")
}

# =============================================================================
# 4. Save Wide Summary Table
# =============================================================================

wide_tbl <- plot_data %>%
  select(bv_scenario, tx_scenario, total_length_km) %>%
  pivot_wider(
    names_from  = tx_scenario,
    values_from = total_length_km
  ) %>%
  arrange(match(bv_scenario, SCENARIO_LEVELS)) %>%
  rename(
    `Biodiversity scenario` = bv_scenario,
    `TX1_total_length_km`   = `TX1 (Reference cost)`,
    `TX2_total_length_km`   = `TX2 (Doubled TX cost)`
  )

if (!file.exists(out_tbl) || overwrite_mode) {
  write_csv(wide_tbl, out_tbl)
  cat("✓ Summary table saved to:", out_tbl, "\n")
} else {
  cat(">>> Summary table exists (overwrite_mode = FALSE).\n")
}
print(wide_tbl)


# =============================================================================
# 4b. Console Summary: % increase relative to BAU
# =============================================================================

cat("\n>>> % increase in new transmission length relative to BAU:\n")

bau_summary <- plot_data %>%
  filter(bv_scenario == "BAU") %>%
  select(tx_scenario, bau_km = total_length_km)

plot_data %>%
  filter(bv_scenario != "BAU") %>%
  left_join(bau_summary, by = "tx_scenario") %>%
  mutate(pct_increase = ((total_length_km - bau_km) / bau_km) * 100) %>%
  arrange(tx_scenario, bv_scenario) %>%
  mutate(
    line = sprintf("  %-25s | %-20s | BAU: %s km | Scenario: %s km | Change: +%.1f%%",
                   as.character(tx_scenario),
                   as.character(bv_scenario),
                   scales::comma(round(bau_km)),
                   scales::comma(round(total_length_km)),
                   pct_increase)
  ) %>%
  pull(line) %>%
  cat(sep = "\n")

cat("\n")
# =========================================================================
# 5. Build Plot
# =============================================================================

cat(">>> Building Figure 3...\n")

# Use Arial if available, fall back to default
plot_font <- if ("Arial" %in% fonts()) "Arial" else ""

TX_COLOURS <- c(
  "TX1 (Reference cost)"  = "#2166AC",
  "TX2 (Doubled TX cost)" = "#D6604D",
  "Existing network"      = "grey50"
)

p <- ggplot(plot_data,
            aes(x      = bv_scenario,
                y      = total_length_km,
                colour = tx_scenario,
                group  = tx_scenario)) +
  
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.2)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.2)) +
  
  # Existing transmission baseline in legend
  {if (!is.null(existing_tx_km))
    geom_hline(
      aes(yintercept = existing_tx_km, colour = "Existing network"),
      linetype  = "dashed",
      linewidth = 0.9
    )
  } +
  
  scale_colour_manual(
    values = TX_COLOURS,
    name   = "Transmission scenario",
    guide  = guide_legend(
      override.aes = list(
        linetype = c("solid", "solid", "dashed"),
        shape    = c(16, 16, NA)
      )
    )
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  
  labs(
    x = "Biodiversity avoidance scenario",
    y = "Total new transmission length (km)"
  ) +
  
  theme_minimal(base_size = 12, base_family = plot_font) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(size = 11),
    axis.title       = element_text(size = 13),
    axis.text        = element_text(size = 11),
    plot.margin      = margin(t = 10, r = 10, b = 10, l = 10)
  )

# =============================================================================
# 6. Save and Display
# =============================================================================

if (!file.exists(out_plot) || overwrite_mode) {
  ggsave(out_plot, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
  cat("✓ Figure 3 saved to:", out_plot, "\n")
} else {
  cat(">>> Figure 3 already exists (set overwrite_mode = TRUE to regenerate).\n")
}

print(p)
cat("\n=== TX LENGTH FIGURE (STEP 4) COMPLETE ===\n")