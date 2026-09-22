# =============================================================================
# VRE Technology Map Iteration -- plot-only, no files written
# =============================================================================
# Purpose: build and print the Figure 1b-e VRE panels for visual debugging,
#          without writing anything to disk. build_vre_map() itself lives in
#          vre_map_helpers.R, shared with export_figures.R -- edit it there,
#          re-source, re-run this file. Nothing here needs porting anywhere;
#          when this looks right, export_figures.R's own export loop uses
#          the identical build_vre_map() call.
#
# This script is not executed by Claude Code -- run it yourself in Positron.
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, ggplot2, dplyr, sf, ozmaps)

source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

source(here("publication_figures", "compliance_helpers.R"))
source(here("publication_figures", "vre_map_helpers.R"))

# =============================================================================
# Diagnostics -- run this first
# =============================================================================
# Prints row counts per technology and, if a transmission layer loaded, its
# column names -- so you can see straight away whether offshore_wind rows
# exist at all for a given threshold, and what field to filter spurs on.

inspect_threshold <- function(threshold) {
  cat(sprintf("\n=== threshold %d ===\n", threshold))

  shp_dir_candidates <- c(
    here("results", "figures", "energy_maps", "shapefiles_tx1"),
    file.path(paths$tx_outputs, "domestic_tx1_shapefiles")
  )
  shp_name <- sprintf("combined_renewables_2050_threshold_%d.shp", threshold)
  shp_dir  <- Find(function(d) file.exists(file.path(d, shp_name)), shp_dir_candidates)

  if (is.null(shp_dir)) {
    cat("  VRE shapefile not found.\n")
  } else {
    infra <- sf::st_read(file.path(shp_dir, shp_name), quiet = TRUE)
    cat("  VRE technology counts:\n")
    print(table(infra$technology, useNA = "always"))
    # Reveals hidden whitespace/characters that table()'s column headers
    # can hide (e.g. DBF fixed-width padding) -- quoting makes a trailing
    # space visible before the closing quote, and nchar() confirms it.
    cat("  Raw unique values (quoted, with nchar):\n")
    u <- unique(as.character(infra$technology))
    for (v in u) cat(sprintf("    nchar=%d  value=%s\n", nchar(v), shQuote(v)))

    # The technology values matched correctly (previous check), so if
    # offshore rows still don't render, the geometry itself is the suspect:
    # empty geometry, invalid geometry, an unexpected geometry type, or a
    # bounding box that's nowhere near where the map is actually looking.
    off <- infra[trimws(as.character(infra$technology)) == "offshore", ]
    cat(sprintf("  offshore rows: %d\n", nrow(off)))
    if (nrow(off) > 0) {
      cat("  geometry type(s):", paste(unique(as.character(sf::st_geometry_type(off))), collapse = ", "), "\n")
      cat("  any empty geometry:", any(sf::st_is_empty(off)), "\n")
      cat("  any invalid geometry:", any(!sf::st_is_valid(off)), "\n")
      cat("  bbox (offshore only):\n")
      print(sf::st_bbox(off))
      cat("  bbox (all technologies, for comparison):\n")
      print(sf::st_bbox(infra))
      # build_vre_map() filters infrastructure_data to domestic == 1 BEFORE
      # plotting anything -- this diagnostic reads the raw file directly and
      # skips that filter, so it can look "present" here while being
      # entirely removed by the time build_vre_map() gets to geom_sf().
      if ("domestic" %in% colnames(off)) {
        cat("  offshore rows by `domestic` value (1 = kept by build_vre_map(), else dropped):\n")
        print(table(off$domestic, useNA = "always"))
      } else {
        cat("  no `domestic` column on this layer.\n")
      }
    }
  }

  tx <- load_tx_lines(threshold)
  if (is.null(tx)) {
    cat("  Transmission layer not found.\n")
  } else {
    cat("  Transmission layer columns:", paste(colnames(tx), collapse = ", "), "\n")
    cat("  Transmission layer rows:", nrow(tx), "\n")
    # lin_typ ("line_type"?) and src_lyr ("source layer"?) are the most
    # likely candidates for a spur/main distinction -- print both so the
    # right filter can be written into load_tx_lines().
    for (fld in c("lin_typ", "src_lyr", "tchnlgy")) {
      if (fld %in% colnames(tx)) {
        cat(sprintf("  %s values:\n", fld))
        print(table(tx[[fld]], useNA = "always"))
      }
    }
  }
}

inspect_threshold(30)

# =============================================================================
# Scenario ID pass -- print every available threshold file, labelled
# =============================================================================
# VRE_SCENARIO_LABELS (vre_map_helpers.R) now holds the confirmed mapping,
# from visual inspection of these exact panels: threshold_90 = BAU,
# threshold_70 = Top30, threshold_50 = Top50, threshold_30 = Top70 -- NOT
# the same direction as tx_length_figure.R's THRESHOLD_MAP, which is for a
# different dataset (threshold_0 = BAU there). threshold_0/10 have no entry
# since they're not needed for Figure 1; titled "unmapped" below rather
# than failing, so this loop still shows every file that exists on disk.
#
# Each title is ONLY added here, for this identification pass -- build_vre_map()
# itself deliberately has no labs(title=), since the compliant panels
# shouldn't carry baked-in titles (that's manuscript caption content).

all_thresholds <- c(0, 10, 30, 50, 70, 90)

for (thr in all_thresholds) {
  tryCatch({
    scenario_label <- VRE_SCENARIO_LABELS[[as.character(thr)]]
    if (is.null(scenario_label)) scenario_label <- "unmapped"
    cat(sprintf("\n--- previewing threshold_%d (%s) ---\n", thr, scenario_label))

    p <- build_vre_map(thr, show_legend = TRUE)
    p <- apply_compliance_font(p)
    p <- strip_map_axes(p)
    p <- p + theme(legend.position = "bottom") +
      labs(title = sprintf("threshold_%d (%s)", thr, scenario_label))

    print(p)

  }, error = function(e) {
    cat(sprintf("  FAILED (threshold_%d): %s\n", thr, conditionMessage(e)))
  })
}

# --- Or preview just one, faster to re-run while checking a single map -----
# preview_threshold <- 30
# show_legend        <- FALSE
#
# p_preview <- build_vre_map(preview_threshold, show_legend = show_legend)
# p_preview <- apply_compliance_font(p_preview)
# p_preview <- strip_map_axes(p_preview)
# if (show_legend) p_preview <- p_preview + theme(legend.position = "bottom")
# print(p_preview)
