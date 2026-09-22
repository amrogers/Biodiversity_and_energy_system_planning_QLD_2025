# =============================================================================
# Publication Figure Export -- Phase 2 (runnable script)
# =============================================================================
# Purpose: builds Figures 1a, 1b-e (x4 panels + standalone legend), 2, 3 and 4
#          by sourcing each original, unmodified analysis script (or, where
#          the audit found no capturable plot object, reimplementing just the
#          data-loading and geom logic in a small local function) and
#          re-exporting each as a compliant figure -- vector PDF for the
#          chart/vector-source figures, bitmap TIFF for Figure 1a (see that
#          section for why). No original script is edited.
#
#          See publication_figures/audit_report.md (Phase 1) for the
#          per-figure evidence behind every override applied below.
#
# This script is not executed by Claude Code -- run it yourself in Positron.
# =============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, ggplot2, dplyr, sf, ozmaps, cowplot)

source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# =============================================================================
# Compliance settings (shared across all Figure 1-4 panels)
# =============================================================================
# target_family / target_size / apply_compliance_font() / strip_map_axes() /
# confirm_export() -- see compliance_helpers.R. Shared with export_vre_maps.R
# so the two scripts can never drift out of sync.
source(here("publication_figures", "compliance_helpers.R"))

out_root <- here("publication_figures")
for (d in c("Figure_1", "Figure_2", "Figure_3", "Figure_4")) {
  dp <- file.path(out_root, d)
  if (!dir.exists(dp)) dir.create(dp, recursive = TRUE)
}

cat("\n", strrep("=", 70), "\n", sep = "")
cat("EXPORT FIGURES -- publication compliance pass\n")
cat(strrep("=", 70), "\n", sep = "")

# =============================================================================
# Figure 1a -- Biodiversity prioritisation map (R rendition)
# =============================================================================
tryCatch({
  cat("\n--- Figure 1a ---\n")

  source(here("Biodiversity_analysis", "Biodiversity_value_map.R"))
  # `p` itself is NOT reused here. It's built with geom_tile(), which grid
  # draws as one discrete vector rectangle per cell -- at the ~10^5 cells
  # left after aggregation (agg_factor = 4, ~1km res, QLD-wide), that's what
  # made the first-pass vector PDF come back blank (Cairo silently gives up
  # past some path-count threshold; this isn't cairo_pdf-specific, base pdf
  # and svglite hit the same wall). Rebuilt below with geom_raster() instead
  # of geom_tile() -- same data (`rank_df`, `map_colours`, `qld_border`, all
  # left in the global env by the source() call above, unchanged), but
  # geom_raster() is drawn by grid as a single embedded rasterGrob rather
  # than N separate paths, so it embeds fine in a vector PDF: text, legend
  # and the QLD boundary line stay true vector, only the colour surface
  # itself is a raster image inside the page. rank_df's x/y are on a
  # regular grid (terra::aggregate() output, reprojected to EPSG:3857before
  # this point) so geom_raster()'s equal-cell-size assumption holds.
  p_1a <- ggplot() +
    geom_raster(data = rank_df, aes(x = x, y = y, fill = label)) +
    scale_fill_manual(
      values   = map_colours,
      na.value = "transparent",
      name     = "Priority Class",
      guide    = guide_legend(reverse = FALSE)
    ) +
    geom_sf(data = qld_border, fill = "transparent", colour = "black", linewidth = 0.4) +
    coord_sf(crs = 3857) +
    theme_void(base_size = 11) +
    theme(
      legend.position = "none",
      plot.margin     = margin(5, 5, 5, 5)
    )

  p_1a <- apply_compliance_font(p_1a)
  p_1a <- strip_map_axes(p_1a)

  out_1a <- here("publication_figures", "Figure_1", "Figure_1a.pdf")
  ggsave(out_1a, plot = p_1a, device = cairo_pdf,
         width = 88, height = 130, units = "mm")
  confirm_export(out_1a, 88, 130)

}, error = function(e) {
  cat("  FAILED (Figure 1a):", conditionMessage(e), "\n")
})

# =============================================================================
# Figure 1b-e -- VRE siting maps (tx1, 2050), individual panels
# =============================================================================
# create_plot() in domestic_export_map_iterations.R returns NULL whenever its
# target PNG already exists (audit_report.md, Figure 1b-e detail section), so
# it cannot be sourced and recaptured. build_vre_map() below reimplements
# just the data-loading (st_read) and geom_sf plotting logic from that
# function, against the pre-computed tx1 combined-renewables shapefiles
# already on disk. domestic_export_map_iterations.R itself is not touched.
#
# Transmission lines: create_plot() never drew these -- see load_tx_lines()
# in vre_map_helpers.R for how that was traced and where the per-threshold
# line data actually lives.
#
# Final assembly of Figure_1a.pdf + the 4 VRE panels below into the composite
# Figure 1 layout happens manually in Illustrator/PowerPoint (per NEE
# figure-compiling guidance) -- this script only produces the compliant
# individual panels.
#
# TECH_LEVELS / TECH_COLORS / TX_LABEL / load_tx_lines() / build_vre_map() --
# see vre_map_helpers.R. Shared with export_vre_maps.R (the fast standalone
# iteration script) so both always build the exact same panel.
# =============================================================================
source(here("publication_figures", "vre_map_helpers.R"))

thresholds_to_export <- c(30, 50, 70, 90)

for (thr in thresholds_to_export) {
  tryCatch({
    scenario_label <- VRE_SCENARIO_LABELS[[as.character(thr)]]
    if (is.null(scenario_label)) {
      stop("threshold_", thr, " has no entry in VRE_SCENARIO_LABELS (vre_map_helpers.R) -- add one before exporting.")
    }
    cat(sprintf("\n--- Figure 1 VRE panel: threshold_%d (%s) ---\n", thr, scenario_label))

    p_vre <- build_vre_map(thr, show_legend = FALSE)
    p_vre <- apply_compliance_font(p_vre)
    p_vre <- strip_map_axes(p_vre)
    p_vre <- p_vre + theme(legend.position = "none")

    out_vre <- here("publication_figures", "Figure_1",
                     sprintf("Figure_1_VRE_%s.pdf", scenario_label))
    ggsave(out_vre, plot = p_vre, device = cairo_pdf,
           width = 88, height = 88, units = "mm")
    confirm_export(out_vre, 88, 88)

  }, error = function(e) {
    cat(sprintf("  FAILED (Figure 1 VRE panel, threshold_%d): %s\n", thr, conditionMessage(e)))
  })
}

# --- Shared legend, exported once as its own file ---------------------------
tryCatch({
  cat("\n--- Figure 1 VRE legend (standalone) ---\n")

  p_legend_src <- build_vre_map(thresholds_to_export[1], show_legend = TRUE)
  p_legend_src <- apply_compliance_font(p_legend_src)

  legend_grob   <- extract_legend(p_legend_src)  # compliance_helpers.R -- cowplot, with an ggpubr fallback
  p_legend_only <- cowplot::ggdraw(legend_grob)

  out_legend <- here("publication_figures", "Figure_1", "Figure_1_VRE_legend.pdf")
  ggsave(out_legend, plot = p_legend_only, device = cairo_pdf,
         width = 88, height = 26, units = "mm")
  confirm_export(out_legend, 88, 26)
  cat("  NOTE: 26mm height is a placeholder (4 technology keys + transmission line key) -- check the render and adjust if it wraps or is clipped.\n")

}, error = function(e) {
  cat("  FAILED (Figure 1 VRE legend):", conditionMessage(e), "\n")
})

# =============================================================================
# Figure 2 -- NPV bar chart
# =============================================================================
tryCatch({
  cat("\n--- Figure 2 ---\n")

  source(here("Figure_code", "NPV_bar_plot.R"))
  # `final_plot` is created by the sourced script (NPV_bar_plot.R:143).
  # Note: NPV_bar_plot.R hardcodes overwrite_mode <- TRUE (line 28), so
  # sourcing it re-saves its own PNG to results/figures/npv_analysis_plot.png
  # as a side effect. Unavoidable while the original script stays unmodified;
  # idempotent (same data in, same PNG out).

  p_fig2 <- apply_compliance_font(final_plot)

  # Already stated in mm in the original (180 x 90mm) -- kept as-is, just
  # switched to a vector device and the compliance font.
  out_2 <- here("publication_figures", "Figure_2", "Figure_2.pdf")
  ggsave(out_2, plot = p_fig2, device = cairo_pdf,
         width = 180, height = 90, units = "mm")
  confirm_export(out_2, 180, 90)

}, error = function(e) {
  cat("  FAILED (Figure 2):", conditionMessage(e), "\n")
})

# =============================================================================
# Figure 3 -- Transmission build length
# =============================================================================
tryCatch({
  cat("\n--- Figure 3 ---\n")

  source(here("Figure_code", "tx_length_figure.R"))
  # `p` is created by the sourced script (tx_length_figure.R:290) -- captured
  # immediately below, before it could be overwritten by any later source()
  # call that also happens to assign a global `p` (none do, after this point,
  # but Figure 1a above used the same name).
  # Note: effective_overwrite resolves to TRUE here (line 68), so sourcing
  # re-saves its own PNG and summary CSV -- same caveat as Figure 2.

  p_fig3 <- apply_compliance_font(p)

  # Original ggsave used 10 x 6in (254 x 152mm) -- wider than NEE's ~183mm
  # double-column max. Scaled to 183mm width, original aspect ratio
  # preserved. Confirm the target width before final submission.
  out_3 <- here("publication_figures", "Figure_3", "Figure_3.pdf")
  ggsave(out_3, plot = p_fig3, device = cairo_pdf,
         width = 183, height = 110, units = "mm")
  confirm_export(out_3, 183, 110)

}, error = function(e) {
  cat("  FAILED (Figure 3):", conditionMessage(e), "\n")
})

# =============================================================================
# Figure 4 -- Energy cost increase
# =============================================================================
tryCatch({
  cat("\n--- Figure 4 ---\n")

  source(here("Figure_code", "percent cost increase_line plot.R"))
  # `cost_plot` is created by the sourced script (line 66). overwrite_mode is
  # hardcoded FALSE inside that script (line 20), so sourcing it does NOT
  # touch the existing results/figures/energy_cost_increase_plot.png.

  p_fig4 <- apply_compliance_font(cost_plot)

  # Original ggsave used 12 x 6in (305 x 152mm). Scaled to 183mm width,
  # original aspect ratio preserved. Confirm the target width before final
  # submission. Colour palette (scale_color_brewer("Set1")) is left as-is --
  # no palette fix for Figure 4 was specified in this pass; audit_report.md
  # flags it as not colour-blind-safe if you want that addressed too.
  out_4 <- here("publication_figures", "Figure_4", "Figure_4.pdf")
  ggsave(out_4, plot = p_fig4, device = cairo_pdf,
         width = 183, height = 92, units = "mm")
  confirm_export(out_4, 183, 92)

}, error = function(e) {
  cat("  FAILED (Figure 4):", conditionMessage(e), "\n")
})

cat("\n", strrep("=", 70), "\n", sep = "")
cat("EXPORT COMPLETE -- see publication_figures/Figure_*/ for outputs\n")
cat(strrep("=", 70), "\n", sep = "")
