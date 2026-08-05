# =============================================================================
# Figure 1b-e: VRE siting maps (tx1, 2050) -- verification step
# =============================================================================
# Sourced by _RUN_ALL.R.
#
# The Figure 1b-e panels are high-resolution PNG maps (4500 x 4500 px) produced
# by:
#   Energy system and transmission analysis/domestic_export_map_iterations.R
#
# They are stored in the analysis output folder:
#   <data_root>/Energy_system_model_outputs/Electricity_Transmission_Lines/
#     Tx_outputs/domestic_maps_tx1/
#
# This step verifies that all six maps are present and prints their full file
# paths so a reviewer can open them directly. The maps are not rendered in the
# R plot pane -- they are too large to display usefully, and repeated raster
# rendering inside source() is unreliable in the RStudio graphics device.
#
# To regenerate the maps from the source GDBs, set regenerate_fig1 <- TRUE in
# _RUN_ALL.R. This takes 30+ minutes and requires the full data download.
#
# Author: Andrew Rogers
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(here)

# _paths.R is not sourced in display_mode, so source it here.
source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) source(local_override)

if (!exists("regenerate_fig1")) regenerate_fig1 <- FALSE

fig1_expected <- sprintf("domestic_layer_map_%d_2050.png",
                         c(0, 10, 30, 50, 70, 90))

# Primary location: the analysis output folder (where the manuscript maps live).
# Fallback: results/, where domestic_export_map_iterations.R writes if rerun.
fig1_dirs <- c(
  "analysis folder" = file.path(paths$tx_outputs, "domestic_maps_tx1"),
  "results folder"  = here("results", "figures", "energy_maps",
                           "domestic_maps_tx1")
)

find_fig1 <- function() {
  for (lbl in names(fig1_dirs)) {
    d <- fig1_dirs[[lbl]]
    if (dir.exists(d) && all(file.exists(file.path(d, fig1_expected)))) {
      return(list(dir = d, label = lbl, complete = TRUE))
    }
  }
  # Nothing complete -- report on the primary location.
  list(dir = fig1_dirs[[1]], label = names(fig1_dirs)[1], complete = FALSE)
}

fig1 <- find_fig1()

# --- Optional regeneration -------------------------------------------------
if (regenerate_fig1 || !fig1$complete) {
  if (regenerate_fig1) {
    cat("  regenerate_fig1 = TRUE -- rebuilding maps from GDBs.\n")
    cat("  WARNING: this takes 30+ minutes and needs the full data download.\n")
    source(here("Energy system and transmission analysis",
                "domestic_export_map_iterations.R"))
    fig1 <- find_fig1()
  } else {
    cat("  One or more maps are missing.\n")
    cat("  Set regenerate_fig1 <- TRUE in _RUN_ALL.R to rebuild from the GDBs\n")
    cat("  (30+ minutes; requires the full data download).\n\n")
  }
}

# --- Report ----------------------------------------------------------------
cat("  Location:", fig1$label, "\n")
cat("  Folder:  ", normalizePath(fig1$dir, winslash = "/", mustWork = FALSE), "\n\n")

fig1_paths  <- file.path(fig1$dir, fig1_expected)
fig1_exists <- file.exists(fig1_paths)
fig1_size   <- ifelse(fig1_exists,
                      sprintf("%.0f KB", file.size(fig1_paths) / 1024),
                      "--")

fig1_report <- data.frame(
  Map     = fig1_expected,
  Present = ifelse(fig1_exists, "yes", "MISSING"),
  Size    = fig1_size,
  stringsAsFactors = FALSE
)
print(fig1_report, row.names = FALSE)

cat("\n  Full paths:\n")
for (p in fig1_paths[fig1_exists]) {
  cat("   ", normalizePath(p, winslash = "/", mustWork = FALSE), "\n")
}

cat(sprintf("\n  %d of %d maps present.\n", sum(fig1_exists), length(fig1_expected)))

if (!all(fig1_exists)) {
  stop(sprintf("Figure 1b-e: %d of %d maps missing from %s",
               sum(!fig1_exists), length(fig1_expected), fig1$dir))
}

cat("  Figure 1b-e verified. Open the paths above to view the maps.\n")