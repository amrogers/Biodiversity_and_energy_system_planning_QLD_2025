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
               grid, gridExtra, here, scales)
source(here::here("_paths.R"))

# --- USER CONTROL SETTINGS ---
overwrite_mode <- FALSE

# =============================================================================
# Setup and Path Configuration
# =============================================================================

data_dir        <- file.path(data_root, "Energy_system_model_outputs")
output_dir      <- here("results", "figures")
npv_file        <- file.path(data_dir, "eplus_Domestic_NPV_2025.xlsx")
output_filename <- file.path(output_dir, "npv_analysis_plot.png")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# =============================================================================
# Load and Process Data (always runs)
# =============================================================================

if (!file.exists(npv_file)) {
  stop("Error: NPV data file not found at: ", npv_file,
       "\nPlease ensure the data file is in the correct folder.")
}

cat(">>> Loading NPV data...\n")

# File has 6 columns: scenario, thresholds, 2030, 2040, 2050, percent_increase
npv_data <- read_excel(npv_file, col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric"))

cat(">>> Columns found:", paste(names(npv_data), collapse = ", "), "\n")
cat(">>> Rows found:", nrow(npv_data), "\n")

# Pivot year columns to long format; keep percent_increase as a separate column
npv_long <- npv_data %>%
  rename(scenario = 1, thresholds = 2, percent_increase = 6) %>%
  pivot_longer(
    cols      = c(`2030`, `2040`, `2050`),
    names_to  = "year",
    values_to = "npv_value"
  ) %>%
  filter(thresholds != "0.1") %>%
  mutate(
    thresholds = factor(thresholds, levels = c("BAU", "0.3", "0.5", "0.7", "0.9")),
    year       = factor(year, levels = c("2030", "2040", "2050")),
    scenario   = factor(scenario)
  )

cat(">>> Data processed:", nrow(npv_long), "rows after pivoting.\n")

# =============================================================================
# Build Plot (always runs)
# =============================================================================

cat(">>> Building NPV plot...\n")

final_plot <- ggplot(npv_long, aes(x = year, y = npv_value, fill = thresholds)) +
  geom_col(
    colour   = "black",
    position = position_dodge(width = 0.9),
    width    = 0.8
  ) +
  facet_wrap(~scenario, scales = "free_y", labeller = label_both) +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title   = "Net Present Value of Energy Infrastructure Investments",
    x       = "Year",
    y       = "NPV (billion AUD)",
    fill    = "Avoidance\nThreshold",
    caption = paste("Source:", basename(npv_file))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.caption     = element_text(colour = "grey60", size = 8),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

# =============================================================================
# Save (only if needed) and Display (always)
# =============================================================================

if (!file.exists(output_filename) || overwrite_mode) {
  ggsave(output_filename, plot = final_plot, width = 14, height = 8, dpi = 300, bg = "white")
  summary_filename <- file.path(output_dir, "npv_summary_data.csv")
  write.csv(npv_long, summary_filename, row.names = FALSE)
  cat("✓ NPV plot saved to:", output_filename, "\n")
  cat("✓ Summary CSV saved to:", summary_filename, "\n")
} else {
  cat(">>> Plot already exists (set overwrite_mode = TRUE to regenerate).\n")
}

print(final_plot)

cat("\n=== NPV ANALYSIS COMPLETE ===\n")