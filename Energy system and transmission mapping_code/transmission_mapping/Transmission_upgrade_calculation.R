# =============================================================================
# Transmission Upgrade Calculation — Easement Area Analysis
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: For each biodiversity protection threshold and TX scenario,
#          calculates the difference in transmission easement area between
#          modelled upgrades and the existing network. Accounts for shared
#          corridors (parallel existing lines) and produces per-threshold
#          summaries plus a combined total area increase table.
#
# Input:  results/transmission_processing/{scenario}/ex_mod_join_tables/
#              transmission_y2050_t{THRESHOLD}.xlsx
#         (produced by ArcGIS spatial join of modelled vs existing TX)
#         Expected columns: TARGET_FID, kv, length_k_1, max_ex_kv,
#                           ex_easemen, count_exis
#
# Output: results/transmission_processing/{scenario}/QLD_ex_mod_summaries/
#              summarized_transmission_t{N}.xlsx  (per-threshold summaries)
#              total_area_increase_{scenario}.xlsx (combined table)
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(readxl, dplyr, writexl, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
scenarios      <- c("tx1", "tx2")   # process one or both scenarios
thresholds     <- c(0, 10, 30, 50, 70, 90)
overwrite_mode <- FALSE

# Existing QLD transmission network total area (km²) — used for % increase
EXISTING_TX_AREA_KM2 <- 663.64

# =============================================================================
# 1. Easement Width Lookup (km, based on modelled line voltage)
# =============================================================================

easement_width_km <- function(kv) {
  dplyr::case_when(
    kv < 100               ~ 0.02,
    kv >= 100 & kv <= 275  ~ 0.04,
    kv == 330              ~ 0.06,
    kv == 500              ~ 0.07,
    TRUE                   ~ NA_real_
  )
}

# =============================================================================
# 2. Processing Function (one scenario)
# =============================================================================

process_scenario <- function(scenario) {
  tx_proc_base <- if (scenario == "tx1") tx1_processing else tx2_processing

  input_dir  <- file.path(tx_proc_base, "ex_mod_join_tables")
  output_dir <- file.path(tx_proc_base, "QLD_ex_mod_summaries")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  total_area_increase <- data.frame(
    threshold        = thresholds,
    total_area_diff  = NA_real_,
    percent_increase = NA_real_
  )

  for (t in thresholds) {
    in_file  <- file.path(input_dir,  paste0("transmission_y2050_t", t, ".xlsx"))
    out_file <- file.path(output_dir, paste0("summarized_transmission_t", t, ".xlsx"))

    if (!file.exists(in_file)) {
      warning(sprintf("[%s] Input not found for threshold %d: %s", scenario, t, in_file))
      next
    }

    if (file.exists(out_file) && !overwrite_mode) {
      cat(sprintf("  [%s] t%d: exists (skip)\n", scenario, t))
    } else {
      df <- read_excel(in_file) %>%
        filter(!is.na(kv) & kv != 0) %>%
        group_by(TARGET_FID) %>%
        summarise(
          max_mod_kv      = max(kv,          na.rm = TRUE),
          max_length_km   = max(length_k_1,  na.rm = TRUE),
          max_ex_kv       = max(max_ex_kv,   na.rm = TRUE),
          max_ex_easement = max(ex_easemen,  na.rm = TRUE),
          count_existing  = max(count_exis,  na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          mod_tx_easement = easement_width_km(max_mod_kv),
          area_ex         = max_ex_easement * max_length_km,
          area_mod        = mod_tx_easement  * max_length_km,
          area_diff       = ifelse(
            count_existing > 1,
            (area_mod - area_ex) / 2,
             area_mod - area_ex
          )
        )

      write_xlsx(df, out_file)
      cat(sprintf("  ✓ [%s] t%d saved\n", scenario, t))
    }

    # Accumulate totals
    df_s <- read_excel(out_file)
    total_area_diff <- sum(df_s$area_diff, na.rm = TRUE)
    idx <- total_area_increase$threshold == t
    total_area_increase$total_area_diff[idx]  <- total_area_diff
    total_area_increase$percent_increase[idx] <-
      (total_area_diff / EXISTING_TX_AREA_KM2) * 100
  }

  # Save combined area increase table
  totals_file <- file.path(output_dir, paste0("total_area_increase_", scenario, ".xlsx"))
  if (!file.exists(totals_file) || overwrite_mode) {
    write_xlsx(total_area_increase, totals_file)
    cat(sprintf("  ✓ [%s] Total area increase table saved\n", scenario))
  }

  cat(sprintf("[%s] Processing complete. Outputs: %s\n", scenario, output_dir))
}

# =============================================================================
# 3. Run for Selected Scenarios
# =============================================================================

for (sc in scenarios) {
  cat(sprintf("\n>>> Processing scenario: %s\n", sc))
  process_scenario(sc)
}

cat("\n=== TRANSMISSION UPGRADE CALCULATION COMPLETE ===\n")
