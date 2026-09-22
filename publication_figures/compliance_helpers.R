# =============================================================================
# Compliance helpers -- shared by export_figures.R and export_vre_maps.R
# =============================================================================
# Assumes ggplot2 is already loaded by the caller. Sourced, not run directly.
# =============================================================================

target_family <- "Arial"  # NEE house-font assumption (audit_report.md, open
                           # item 4) -- confirm against the journal's current
                           # figure-preparation guidelines before submission.
target_size   <- 7        # pt at final print size.

# apply_compliance_font(): a blanket `theme(text = element_text(...))` only
# changes elements that don't already have an explicit size/family set --
# ggplot2 inherits from `text` solely for properties a child element leaves
# unset. Figure 3's theme sets axis.text = 10 / axis.title = 13, and Figure
# 4's sets axis.title = 18 / axis.text = 16 (both explicit, both larger than
# every other figure). A bare `text = element_text(size = 7)` override would
# NOT touch those and the PDFs would still ship with mismatched, oversized
# text. Setting every text element explicitly guarantees the override
# actually takes.
apply_compliance_font <- function(p) {
  p + theme(
    text          = element_text(family = target_family, size = target_size),
    axis.title    = element_text(family = target_family, size = target_size),
    axis.text     = element_text(family = target_family, size = target_size),
    legend.title  = element_text(family = target_family, size = target_size),
    legend.text   = element_text(family = target_family, size = target_size),
    strip.text    = element_text(family = target_family, size = target_size),
    plot.title    = element_text(family = target_family, size = target_size),
    plot.subtitle = element_text(family = target_family, size = target_size),
    plot.caption  = element_text(family = target_family, size = target_size)
  )
}

# strip_map_axes(): apply_compliance_font() sets axis.text = element_text(...)
# on every plot, which un-blanks any axis.text = element_blank() a map's own
# theme had already set (theme_void() for Fig 1a, the explicit blank in
# build_vre_map() for 1b-e). For a geom_sf/coord_sf plot, un-blanked
# axis.text means coord_sf()'s default lat/lon graticule tick labels
# reappear. Apply this AFTER apply_compliance_font() on every spatial figure
# (never on the Figure 2-4 charts, which need real axis text).
strip_map_axes <- function(p) {
  p + theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line  = element_blank(),
    panel.grid = element_blank()
  )
}

confirm_export <- function(path, width, height, units = "mm",
                            format = "vector PDF (cairo_pdf)") {
  cat(sprintf("  Exported: %s  [%g x %g %s, %s]\n", path, width, height, units, format))
}

# extract_legend(): cowplot::get_legend() is unmaintained and has known
# breakage on newer ggplot2 versions (the internal gtable structure for
# legends changed around ggplot2 3.5.0) -- it can fail either loudly (error/
# warning) or silently (returns a zero-size "zeroGrob" placeholder with no
# actual legend content, no error at all). Tries cowplot first since it's
# already a dependency here; falls back to ggpubr::get_legend() (same call
# signature) if cowplot errors, warns, or returns an empty grob.
extract_legend <- function(plot) {
  legend_grob <- tryCatch(
    withCallingHandlers(
      cowplot::get_legend(plot),
      warning = function(w) {
        message("  cowplot::get_legend() warned: ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      message("  cowplot::get_legend() failed: ", conditionMessage(e))
      NULL
    }
  )

  empty <- is.null(legend_grob) ||
    inherits(legend_grob, "zeroGrob") ||
    (is.list(legend_grob) && length(legend_grob) == 0)

  if (empty) {
    message("  cowplot returned no usable legend -- falling back to ggpubr::get_legend().")
    if (!requireNamespace("ggpubr", quietly = TRUE)) install.packages("ggpubr")
    legend_grob <- ggpubr::get_legend(plot)
  }

  legend_grob
}
