# =============================================================================
# Transmission Pipeline — Step 1: Extract Model Transmission from GDB
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Jan 2026; Updated: Mar 2026
# =============================================================================
# Purpose: Reads domestic transmission line layers from the energy model GDB
#          (TX1 or TX2 scenario) and saves per-threshold, per-year layers to a
#          GeoPackage and to individual shapefiles, ready for downstream
#          spatial analysis.
#
# Pipeline position: Step 1 of 4 for Figure 3
#   Step 1 → THIS SCRIPT
#   Step 2 → Transmission_save_layers_as_shapefiles.R
#   Step 3 → QLD_new_tx_processing_summary.R
#   Step 4 → transmission_length_tx1_tx2.R (Figure 3)
#
# Input:  BESP_data_qld_2025/Energy_system_model_outputs/
#              Energy_system_analysis_scenarios/QLD_v202412_eplus_tx1.gdb  (or tx2)
#         (paths$gdb_tx1 / paths$gdb_tx2 in _paths.R)
#
# Output: results/transmission_processing/tx1/split_tx.gpkg
#         results/transmission_processing/tx1/TX_domestic_layers/
#              transmission_y{YEAR}_t{THRESHOLD}.shp  (one per year × threshold)
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(sf, dplyr, furrr, here)
source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# --- USER CONTROL ---
# Defaults used when run standalone. When called from tx_run_all.R these are
# set by the parent script — the if (!exists) guards prevent overwriting them.
if (!exists("tx_scenario"))    tx_scenario    <- "tx1"    # "tx1" or "tx2"
if (!exists("overwrite_mode")) overwrite_mode <- FALSE
years      <- c(2050)
thresholds <- c(0, 10, 30, 50, 70, 90)

# =============================================================================
# 1. Path Configuration
# =============================================================================

input_gdb    <- if (tx_scenario == "tx1") paths$gdb_tx1 else paths$gdb_tx2
tx_proc_base <- if (tx_scenario == "tx1") tx1_processing else tx2_processing

output_gpkg     <- file.path(tx_proc_base, "split_tx.gpkg")
output_shp_dir  <- file.path(tx_proc_base, "TX_domestic_layers")

for (d in c(tx_proc_base, output_shp_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

if (!file.exists(input_gdb)) {
  stop(
    "GDB not found:\n  ", input_gdb, "\n\n",
    "Unzip BESP_data_qld_2025/QLD_v202412_eplus_", tx_scenario, ".gdb.zip first."
  )
}

# =============================================================================
# 2. Identify Matching Layers
# =============================================================================

cat(">>> Reading GDB layer list...\n")
all_layers <- sf::st_layers(input_gdb)$name

tech_patterns <- c("off", "pv", "wind", "interTX")
line_types    <- c("bulk", "spur", "sink", "export")

layer_info <- data.frame(layer_name = all_layers, stringsAsFactors = FALSE) %>%
  filter(
    sapply(layer_name, function(l)
      any(sapply(years,      function(y) grepl(paste0("_", y, "_"), l))) &&
      any(sapply(thresholds, function(t) grepl(paste0("_", t, "_"), l))) &&
      any(sapply(tech_patterns, function(p) grepl(paste0("^", p), l))) &&
      any(sapply(line_types,    function(lt) grepl(paste0("_", lt, "$"), l)))
    )
  ) %>%
  mutate(
    technology = sapply(layer_name, function(l) {
      tech_patterns[sapply(tech_patterns, function(p) grepl(paste0("^", p), l))][1]
    }),
    line_type = sapply(layer_name, function(l) {
      line_types[sapply(line_types, function(lt) grepl(paste0("_", lt, "$"), l))][1]
    }),
    threshold = as.integer(gsub("_", "",
      regmatches(layer_name, regexpr("_\\d+_", layer_name))
    )),
    year = as.integer(
      regmatches(layer_name, regexpr("\\d{4}", layer_name))
    )
  )

cat(sprintf(">>> Found %d matching layers.\n", nrow(layer_info)))
print(table(layer_info$technology, layer_info$threshold))

# =============================================================================
# 3. Save to GeoPackage (all layers combined)
# =============================================================================

if (!file.exists(output_gpkg) || overwrite_mode) {
  cat(">>> Saving layers to GeoPackage...\n")
  plan(multisession, workers = max(1, parallel::detectCores() - 1))

  walk(seq_len(nrow(layer_info)), function(i) {
    lname <- layer_info$layer_name[i]
    ldata <- sf::st_read(input_gdb, lname, quiet = TRUE) %>%
      mutate(technology = layer_info$technology[i],
             line_type  = layer_info$line_type[i],
             year       = layer_info$year[i],
             threshold  = layer_info$threshold[i])
    sf::st_write(ldata, output_gpkg,
                 layer        = lname,
                 delete_layer = TRUE,
                 quiet        = TRUE)
  })

  plan(sequential)
  cat("✓ GeoPackage saved to:", output_gpkg, "\n")
} else {
  cat(">>> GeoPackage exists (overwrite_mode = FALSE).\n")
}

# =============================================================================
# 4. Split to Per-threshold Shapefiles (year 2050)
# =============================================================================

cat(">>> Splitting to per-threshold shapefiles...\n")

layers_gpkg <- sf::st_layers(output_gpkg)$name

for (thresh in thresholds) {
  for (yr in years) {
    out_shp <- file.path(output_shp_dir,
                         sprintf("transmission_y%d_t%d.shp", yr, thresh))

    if (file.exists(out_shp) && !overwrite_mode) {
      cat(sprintf("  Exists: transmission_y%d_t%d.shp (skip)\n", yr, thresh))
      next
    }

    # Collect all technology layers for this year × threshold
    target_layers <- layers_gpkg[
      grepl(paste0("_", yr, "_"), layers_gpkg) &
      grepl(paste0("_", thresh, "_"), layers_gpkg)
    ]

    if (length(target_layers) == 0) {
      cat(sprintf("  No layers found for y%d t%d\n", yr, thresh))
      next
    }

    combined <- bind_rows(lapply(target_layers, function(l)
      sf::st_read(output_gpkg, l, quiet = TRUE)
    ))

    sf::st_write(combined, out_shp, delete_dsn = TRUE, quiet = TRUE)
    cat(sprintf("  ✓ Saved: transmission_y%d_t%d.shp\n", yr, thresh))
  }
}

cat("\n=== TRANSMISSION PROCESSING (STEP 1) COMPLETE ===\n")
cat("Outputs:", output_shp_dir, "\n")
