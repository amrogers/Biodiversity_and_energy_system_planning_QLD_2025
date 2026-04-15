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
#   threshold_0  → BAU       (top 14% excluded)
#   threshold_10 → Top 90%   (top 10% excluded)
#   threshold_30 → Top 70%   (top 30% excluded)
#   threshold_50 → Top 50%   (top 50% excluded)
#   threshold_70 → Top 30%   (top 70% excluded)
#
# NOTE: Stacked bars show existing (bottom) + new build (top) per scenario.
#       % increase labels on bars are relative to BAU total network length.
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

local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# --- USER CONTROL ---
standalone_overwrite <- TRUE
effective_overwrite  <- if (exists("overwrite_mode")) overwrite_mode else standalone_overwrite

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
# 3. Colour Configuration
# =============================================================================

EXISTING_COLOUR <- "grey70"

NEWBUILD_COLOURS <- c(
  "BAU"     = "#ac0505",
  "Top 30%" = "#2887a1",
  "Top 50%" = "#85adaf",
  "Top 70%" = "#cbd5bc",
  "Top 90%" = "#e0cfa2"
)

# =============================================================================
# 4. Load and Summarise
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
    summarise(new_length_km = sum(length_km, na.rm = TRUE), .groups = "drop") %>%
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
# 5. Load Existing Transmission Length
# =============================================================================

cat(">>> Loading existing transmission...\n")

existing_tx_csv <- paths$existing_tx_csv

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

if (is.null(existing_tx_km)) {
  stop("Existing TX length required for stacked bar chart.")
}

# =============================================================================
# 6. Build Stacked Bar Data and % Increase Labels
# =============================================================================

bau_total <- plot_data %>%
  filter(bv_scenario == "BAU") %>%
  mutate(bau_total_km = new_length_km + existing_tx_km) %>%
  select(tx_scenario, bau_total_km)

# Long format: existing first (bottom of stack), new build second (top)
stack_data <- plot_data %>%
  mutate(existing_km = existing_tx_km) %>%
  pivot_longer(
    cols      = c(existing_km, new_length_km),
    names_to  = "segment",
    values_to = "length_km"
  ) %>%
  mutate(
    segment = factor(segment,
                     levels = c("existing_km", "new_length_km"),
                     labels = c("Existing network", "New build"))
  )

# % increase labels (total bar height relative to BAU)
label_data <- plot_data %>%
  mutate(total_km = new_length_km + existing_tx_km) %>%
  left_join(bau_total, by = "tx_scenario") %>%
  mutate(
    pct_increase = ((total_km - bau_total_km) / bau_total_km) * 100,
    pct_label    = ifelse(
      bv_scenario == "BAU",
      "BAU",
      sprintf("+%.1f%%", pct_increase)
    )
  )

cat(">>> Label data:\n")
print(label_data %>% select(bv_scenario, tx_scenario, total_km, pct_label))

# =============================================================================
# 7. Save Wide Summary Table
# =============================================================================

wide_tbl <- plot_data %>%
  mutate(
    existing_km      = existing_tx_km,
    total_network_km = new_length_km + existing_tx_km
  ) %>%
  left_join(bau_total, by = "tx_scenario") %>%
  mutate(
    pct_increase = ifelse(
      bv_scenario == "BAU", 0,
      ((total_network_km - bau_total_km) / bau_total_km) * 100
    )
  ) %>%
  select(bv_scenario, tx_scenario, existing_km, new_length_km,
         total_network_km, pct_increase) %>%
  pivot_wider(
    names_from  = tx_scenario,
    values_from = c(existing_km, new_length_km, total_network_km, pct_increase),
    names_glue  = "{tx_scenario}_{.value}"
  ) %>%
  arrange(match(bv_scenario, SCENARIO_LEVELS)) %>%
  rename(`Biodiversity scenario` = bv_scenario)

if (!file.exists(out_tbl) || effective_overwrite) {
  write_csv(wide_tbl, out_tbl)
  cat("✓ Summary table saved to:", out_tbl, "\n")
} else {
  cat(">>> Summary table exists (effective_overwrite = FALSE).\n")
}
print(wide_tbl)

# =============================================================================
# 8. Console Summary: % Increase Relative to BAU
# =============================================================================

cat("\n>>> % increase in total transmission network length relative to BAU:\n")

label_data %>%
  filter(bv_scenario != "BAU") %>%
  arrange(tx_scenario, bv_scenario) %>%
  mutate(
    line = sprintf("  %-25s | %-20s | BAU: %s km | Scenario: %s km | Change: %s",
                   as.character(tx_scenario),
                   as.character(bv_scenario),
                   scales::comma(round(bau_total_km)),
                   scales::comma(round(total_km)),
                   pct_label)
  ) %>%
  pull(line) %>%
  cat(sep = "\n")

cat("\n")

# =============================================================================
# 9. Build Plot
# =============================================================================

cat(">>> Building Figure 3...\n")

plot_font <- if ("Arial" %in% fonts()) "Arial" else ""

# Build named fill vector using interaction of segment × scenario.
# This allows position_stack to work correctly while giving independent
# control over existing network (grey) and new build (scenario colour) fills.
fill_values <- c(
  setNames(rep(EXISTING_COLOUR, length(SCENARIO_LEVELS)),
           paste("Existing network", SCENARIO_LEVELS, sep = "|")),
  setNames(NEWBUILD_COLOURS,
           paste("New build", names(NEWBUILD_COLOURS), sep = "|"))
)

# Legend shows only new build scenario colours
fill_breaks <- paste("New build", SCENARIO_LEVELS, sep = "|")
fill_labels <- SCENARIO_LEVELS

p <- ggplot(
  data = stack_data,
  aes(x    = bv_scenario,
      y    = length_km,
      fill = interaction(segment, bv_scenario, sep = "|"))
) +
  
  geom_col(
    width    = 0.6,
    position = position_stack(reverse = TRUE)
  ) +
  
  # % increase labels above each bar
  geom_text(
    data        = label_data,
    aes(x       = bv_scenario,
        y       = total_km,
        label   = pct_label),
    vjust       = -0.5,
    size        = 3.5,
    family      = plot_font,
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(
    values = fill_values,
    breaks = fill_breaks,
    labels = fill_labels,
    name   = "Biodiversity scenario"
  ) +
  
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  labs(
    x = "Biodiversity avoidance scenario",
    y = "Total transmission network length (km)"
  ) +
  
  facet_wrap(~ tx_scenario) +
  
  theme_minimal(base_size = 12, base_family = plot_font) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "right",
    legend.title       = element_text(size = 11),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 10),
    axis.text.x        = element_text(angle = 30, hjust = 1),
    strip.text         = element_text(size = 11, face = "bold"),
    plot.margin        = margin(t = 10, r = 10, b = 10, l = 10)
  )

# =============================================================================
# 10. Save and Display
# =============================================================================

if (!file.exists(out_plot) || effective_overwrite) {
  ggsave(out_plot, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
  cat("✓ Figure 3 saved to:", out_plot, "\n")
} else {
  cat(">>> Figure 3 already exists (set standalone_overwrite = TRUE to regenerate).\n")
}

print(p)
cat("\n=== TX LENGTH FIGURE (STEP 4) COMPLETE ===\n")