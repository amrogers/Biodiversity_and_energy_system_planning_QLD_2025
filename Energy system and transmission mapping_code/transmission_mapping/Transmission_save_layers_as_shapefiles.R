# =============================================================================
# Transmission Pipeline — Step 2: Save GeoPackage Layers as Shapefiles
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: Reads the combined GeoPackage produced by Transmission_processing.R
#          and saves each layer as an individual ESRI Shapefile for downstream
#          spatial analysis tools that require shapefiles rather than GPKG.
#
# Pipeline position: Step 2 of 4 for Figure 3
#   Step 1 → Transmission_processing.R
#   Step 2 → THIS SCRIPT
#   Step 3 → QLD_new_tx_processing_summary.R
#   Step 4 → transmission_length_tx1_tx2.R (Figure 3)
#
# Input:  results/transmission_processing/tx1/split_tx.gpkg
#         (produced by Transmission_processing.R)
#
# Output: results/transmission_processing/tx1/TX_domestic_layers/
#              {layer_name}.shp  (one shapefile per GeoPackage layer)
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, parallel, doParallel, here)
source(here::here("_paths.R"))

# --- USER CONTROL ---
tx_scenario    <- "tx1"   # "tx1" or "tx2"
overwrite_mode <- FALSE

# =============================================================================
# 1. Path Configuration
# =============================================================================

tx_proc_base <- if (tx_scenario == "tx1") tx1_processing else tx2_processing

input_gpkg  <- file.path(tx_proc_base, "split_tx.gpkg")
output_dir  <- file.path(tx_proc_base, "TX_domestic_layers")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

if (!file.exists(input_gpkg)) {
  stop(
    "GeoPackage not found:\n  ", input_gpkg, "\n\n",
    "Run Transmission_processing.R first."
  )
}

# =============================================================================
# 2. Extract Layers
# =============================================================================

layers <- sf::st_layers(input_gpkg)$name
cat(sprintf(">>> Found %d layers in GeoPackage\n", length(layers)))

process_layer <- function(layer_name) {
  out_path <- file.path(output_dir, paste0(gsub("^main\\.", "", layer_name), ".shp"))

  if (file.exists(out_path) && !overwrite_mode) {
    return(list(status = "skipped", name = layer_name))
  }

  tryCatch({
    ldata <- sf::st_read(input_gpkg, layer = layer_name, quiet = TRUE)
    sf::st_write(ldata, out_path, delete_dsn = TRUE, quiet = TRUE)
    list(status = "success", name = layer_name)
  }, error = function(e) {
    list(status = "error", name = layer_name, message = e$message)
  })
}

# =============================================================================
# 3. Parallel Execution
# =============================================================================

num_cores <- max(1, parallel::detectCores() - 1)
cat(sprintf(">>> Processing %d layers on %d cores...\n", length(layers), num_cores))

cl <- makeCluster(num_cores)
registerDoParallel(cl)
clusterExport(cl, c("process_layer", "input_gpkg", "output_dir", "overwrite_mode"))
clusterEvalQ(cl, { library(sf); library(dplyr) })

results <- parLapply(cl, layers, process_layer)
stopCluster(cl)

# =============================================================================
# 4. Summary
# =============================================================================

n_ok      <- sum(sapply(results, `[[`, "status") == "success")
n_skipped <- sum(sapply(results, `[[`, "status") == "skipped")
n_err     <- sum(sapply(results, `[[`, "status") == "error")

cat(sprintf("\n✓ Saved: %d  |  Skipped: %d  |  Errors: %d\n",
            n_ok, n_skipped, n_err))

errors <- Filter(function(x) x$status == "error", results)
if (length(errors) > 0) {
  cat("Errors:\n")
  for (e in errors) cat(sprintf("  %s: %s\n", e$name, e$message))
}

cat("\n=== SAVE LAYERS AS SHAPEFILES (STEP 2) COMPLETE ===\n")
cat("Outputs:", output_dir, "\n")
