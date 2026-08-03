# =============================================================================
# QLD Existing TX — Summarize into Simplified Corridor Network
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: Utility script fragment. Summarises existing QLD transmission line
#          attributes (capacity, count) into a simplified corridor network by
#          finding the maximum-capacity and nearest-neighbour lines within
#          each simplified corridor buffer. Outputs a shapefile and CSV of
#          the simplified network with summary attributes.
#
# NOTE: This script is a utility fragment — it requires `simplified_tx` to be
#       loaded beforehand (the simplified existing TX corridor shapefile).
#       It is not part of the main Figure 3 pipeline.
#
# Input:  simplified_tx  (sf object, loaded externally before running)
#         BESP_data_qld_2025/Spatial_reference_data/
#              Electricity_Transmission_Lines.shp  (paths$existing_tx)
#
# Output: BESP_data_qld_2025/Spatial_reference_data/
#              QLD_existing_tx_simplified_with_results.shp
#              QLD_existing_tx_simplified_results.csv
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, future.apply, dplyr, here)
source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# NOTE: `simplified_tx` must be loaded before running this script, e.g.:
#   simplified_tx <- sf::st_read("path/to/simplified_corridor.shp")
if (!exists("simplified_tx")) {
  stop(
    "`simplified_tx` is not defined.\n",
    "Load the simplified TX corridor shapefile before sourcing this script:\n",
    "  simplified_tx <- sf::st_read('<path_to_simplified_corridor.shp>')"
  )
}

existing_tx <- sf::st_read(paths$existing_tx, quiet = TRUE)

# =============================================================================
# Processing function (one simplified corridor line)
# =============================================================================

process_simplified_line <- function(i) {
  geom         <- simplified_tx$geometry[i]
  nearby_mask  <- as.logical(sf::st_is_within_distance(geom, existing_tx, dist = 500))
  nearby_lines <- existing_tx[nearby_mask, ]
  n_nearby     <- sum(nearby_mask)

  list(
    nearby_line_count    = n_nearby,
    max_nearby_capacity  = if (n_nearby > 0) max(nearby_lines$cpcty_k, na.rm = TRUE) else NA_real_
  )
}

# =============================================================================
# Apply in parallel
# =============================================================================

plan(multisession)
results <- future_lapply(seq_len(nrow(simplified_tx)), process_simplified_line,
                         future.packages = c("sf", "dplyr"))
plan(sequential)

results_df <- do.call(rbind, lapply(results, as.data.frame))

simplified_tx_with_results <- simplified_tx %>%
  mutate(
    line_length_km       = as.numeric(sf::st_length(.) / 1000),
    nearby_line_count    = results_df$nearby_line_count,
    max_nearby_capacity  = results_df$max_nearby_capacity
  )

# =============================================================================
# Save outputs
# =============================================================================

out_shp <- file.path(dirname(paths$existing_tx),
                     "QLD_existing_tx_simplified_with_results.shp")
out_csv <- file.path(dirname(paths$existing_tx),
                     "QLD_existing_tx_simplified_results.csv")

sf::st_write(simplified_tx_with_results, out_shp, delete_dsn = TRUE)
write.csv(sf::st_drop_geometry(simplified_tx_with_results), out_csv, row.names = FALSE)

cat("✓ Shapefile saved to:", out_shp, "\n")
cat("✓ CSV saved to:      ", out_csv, "\n")
print(summary(simplified_tx_with_results))
