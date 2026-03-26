# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# Updated: Mar 2026 — scenario-based coverage summary table
# =============================================================================
# Description: Summarizes mean species coverage across biodiversity value 
# protection scenarios from Zonation outputs (all species in feature_curves).
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, readr, purrr, scales, here)
source(here::here("_paths.R"))

# --- USER CONTROL SETTINGS ---
overwrite_mode <- TRUE

# =============================================================================
# Setup and Path Configuration
# =============================================================================

zonation_base <- file.path(data_root, "Zonation_analysis", "Zonation_output", "250m_QLD_2024", "out_example1")
curves_file   <- file.path(zonation_base, "feature_curves.csv")

output_dir  <- here("results", "tables")
output_file <- file.path(output_dir, "scenario_coverage_results.csv")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- SCENARIO DEFINITIONS ---
scenarios <- tibble(
  label    = c("BAU (14% of Qld)", "Top 30% of Qld", "Top 50% of Qld",
               "Top 70% of Qld",   "Top 90% of Qld"),
  rank_min = c(0.87, 0.70, 0.50, 0.30, 0.10)
)

# =============================================================================
# Smart Execution Logic
# =============================================================================

cat("Checking for existing scenario analysis...\n")

if (file.exists(output_file) && !overwrite_mode) {
  
  cat(">>> Found existing results at:", output_file, "\n")
  results <- read_csv(output_file, show_col_types = FALSE)
  
} else {
  
  cat(">>> Processing Zonation species coverage scenarios...\n")
  
  if (!file.exists(curves_file)) {
    stop("Error: feature_curves.csv not found in: ", zonation_base)
  }
  
  curves <- read_csv(curves_file, show_col_types = FALSE)
  
  # All species columns = everything except 'rank'
  species_cols <- setdiff(names(curves), "rank")
  cat("Total species found in curves:", length(species_cols), "\n")
  
  # --- Helper: mean + 95% CI ---
  calc_mean_ci <- function(values) {
    values <- as.numeric(values)
    values <- values[!is.na(values)]
    n      <- length(values)
    if (n == 0) return(c(mean = NA, ci_lower = NA, ci_upper = NA))
    mean_val <- mean(values)
    se       <- sd(values) / sqrt(n)
    c(mean = mean_val, ci_lower = mean_val - 1.96 * se, ci_upper = mean_val + 1.96 * se)
  }
  
  # --- Main scenario loop ---
  results <- purrr::map_dfr(seq_len(nrow(scenarios)), function(i) {
    
    rank_idx     <- which.min(abs(curves$rank - scenarios$rank_min[i]))
    actual_rank  <- curves$rank[rank_idx]
    species_vals <- as.numeric(curves[rank_idx, species_cols])
    stats        <- calc_mean_ci(species_vals)
    
    tibble(
      `Protection scenario`            = scenarios$label[i],
      `Actual rank used`               = actual_rank,
      `N species`                      = length(species_vals),
      `Average distribution coverage`  = stats["mean"],
      `Species with full coverage`     = sum(species_vals == 1.0, na.rm = TRUE),
      `Species with no coverage`       = sum(species_vals == 0.0, na.rm = TRUE),
      `95 CI (lower)`                  = stats["ci_lower"],
      `95 CI (upper)`                  = stats["ci_upper"]
    )
  })
  
  write_csv(results, output_file)
  cat("✓ Results saved to:", output_file, "\n")
}

# =============================================================================
# Display formatted table
# =============================================================================

results %>%
  mutate(across(c(`Average distribution coverage`, `95 CI (lower)`, `95 CI (upper)`),
                ~ percent(., accuracy = 1))) %>%
  select(-`Actual rank used`, -`N species`) %>%
  print()

cat("\n=== ANALYSIS COMPLETE ===\n")