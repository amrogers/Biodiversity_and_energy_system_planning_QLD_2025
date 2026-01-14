# =============================================================================
# Author: Andrew Rogers 
# LLMs used: Claude AI and Gemini
# Date: Jan 2026
# =============================================================================
# Description: Calculates mean coverage for critically endangered (CE) and 
# endangered (EN) species from Zonation outputs.
# =============================================================================

# Load required libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, readr, here, magick)

# --- USER CONTROL SETTINGS ---
overwrite_mode <- FALSE 

# =============================================================================
# Setup and Path Configuration
# =============================================================================

# Define standardized paths using here()
zonation_base <- here("data", "Zonation_analysis", "Zonation_output", "250m_SNES_ECNES_red_zones_weighted_QLD", "out_example1")
weights_file  <- file.path(zonation_base, "species_weights.csv")
curves_file   <- file.path(zonation_base, "feature_curves.csv")

output_dir    <- here("results", "tables")
output_file   <- file.path(output_dir, "CE_EN_mean_coverage_results.csv")

# Ensure output directory exists
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Target rank values
target_ranks <- c(0.10, 0.30, 0.50, 0.70, 0.84)

# =============================================================================
# Smart Execution Logic
# =============================================================================

cat("Checking for existing coverage analysis...\n")

if (file.exists(output_file) && !overwrite_mode) {
  
  # --- CASE 1: SKIP AND DISPLAY ---
  cat(">>> Found existing results at:", output_file, "\n")
  results <- read_csv(output_file, show_col_types = FALSE)
  print(results)
  
} else {
  
  # --- CASE 2: RUN ANALYSIS ---
  cat(">>> Processing Zonation species coverage...\n")
  
  if (!file.exists(weights_file) | !file.exists(curves_file)) {
    stop("Error: Required Zonation CSV files (weights/curves) not found in: ", zonation_base)
  }
  
  # Read data
  weights <- read_csv(weights_file, show_col_types = FALSE)
  curves  <- read_csv(curves_file, show_col_types = FALSE)
  
  # Identify critically endangered (weight = 5) and endangered (weight = 4)
  ce_features <- weights %>% filter(weight == 5) %>% pull(1) # Using index 1 if OBJECTID name varies
  en_features <- weights %>% filter(weight == 4) %>% pull(1)
  
  # Helper Functions
  find_closest_rank <- function(target, rank_column) {
    which.min(abs(rank_column - target))
  }
  
  calc_mean_ci <- function(values) {
    n <- length(values)
    mean_val <- mean(values, na.rm = TRUE)
    se <- sd(values, na.rm = TRUE) / sqrt(n)
    return(c(mean = mean_val, ci_lower = mean_val - 1.96 * se, ci_upper = mean_val + 1.96 * se))
  }
  
  # Calculation Loop
  results <- data.frame(
    target_rank = target_ranks,
    actual_rank = numeric(length(target_ranks)),
    mean_coverage_CE = numeric(length(target_ranks)),
    ci_lower_CE = numeric(length(target_ranks)),
    ci_upper_CE = numeric(length(target_ranks)),
    mean_coverage_EN = numeric(length(target_ranks)),
    ci_lower_EN = numeric(length(target_ranks)),
    ci_upper_EN = numeric(length(target_ranks))
  )
  
  for (i in seq_along(target_ranks)) {
    rank_idx <- find_closest_rank(target_ranks[i], curves$rank)
    results$actual_rank[i] <- curves$rank[rank_idx]
    
    # CE Stats
    ce_cols <- as.character(ce_features)
    ce_stats <- calc_mean_ci(as.numeric(curves[rank_idx, ce_cols]))
    results$mean_coverage_CE[i] <- ce_stats["mean"]
    results$ci_lower_CE[i] <- ce_stats["ci_lower"]
    results$ci_upper_CE[i] <- ce_stats["ci_upper"]
    
    # EN Stats
    en_cols <- as.character(en_features)
    en_stats <- calc_mean_ci(as.numeric(curves[rank_idx, en_cols]))
    results$mean_coverage_EN[i] <- en_stats["mean"]
    results$ci_lower_EN[i] <- en_stats["ci_lower"]
    results$ci_upper_EN[i] <- en_stats["ci_upper"]
  }
  
  # Save and Display
  write_csv(results, output_file)
  cat("✓ Results saved to:", output_file, "\n")
  print(results)
}

cat("\n=== ANALYSIS COMPLETE ===\n")