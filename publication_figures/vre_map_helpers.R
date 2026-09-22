# =============================================================================
# VRE technology map builder -- shared by export_figures.R and
# export_vre_maps.R
# =============================================================================
# Assumes `paths` (from _paths.R) and the here/ggplot2/dplyr/sf/ozmaps
# packages are already loaded by the caller. Sourced, not run directly.
#
# create_plot() in Energy system and transmission analysis/
# domestic_export_map_iterations.R returns NULL whenever its target PNG
# already exists, so it cannot be sourced and recaptured (see
# audit_report.md, Figure 1b-e detail section). build_vre_map() below
# reimplements just the data-loading (st_read) and geom_sf plotting logic
# from that function -- domestic_export_map_iterations.R itself is not
# touched by anything here.
# =============================================================================

TECH_LEVELS <- c("solar_pv", "wind", "offshore", "other")

# Explicit hex only -- replaces the original's mixed named/hex tech_colors
# (domestic_export_map_iterations.R:289-294). "lightblue" = #ADD8E6 and
# "blue" = #0000FF are R's stock named-colour RGB values.
#
# Key is "offshore", NOT "offshore_wind" -- confirmed from
# table(infrastructure_data$technology) on real data (threshold 30:
# offshore=8, solar_pv=99, wind=24, 0 NA). Note: domestic_export_map_
# iterations.R's own process_layer() (line 82) assigns "offshore_wind" via
# case_when(grepl("^off", ...)), which doesn't match what's actually in this
# deposited shapefile -- meaning combined_renewables_2050_threshold_*.shp
# predates that reconstructed script's category labelling (or the labelling
# drifted after the shapefiles were generated). Either way, "offshore" is
# what the real data says, so that's what's used here.
TECH_COLORS <- c(
  "solar_pv" = "#FFA500",
  "wind"     = "#ADD8E6",
  "offshore" = "#0000FF",
  "other"    = "#808080"
)

TECH_LABELS <- c(
  "solar_pv" = "Solar PV",
  "wind"     = "Wind",
  "offshore" = "Offshore wind",
  "other"    = "Other"
)

# Threshold file number -> biodiversity exclusion scenario, for the VRE
# siting maps specifically. Confirmed by visual inspection of the actual
# rendered panels (export_vre_maps.R's scenario-ID pass), NOT derived from
# tx_length_figure.R's THRESHOLD_MAP -- that mapping is for a different
# dataset (pre-computed transmission-length CSVs) and doesn't apply here;
# notably it maps threshold_0 -> BAU, while for these VRE files it's
# threshold_90 -> BAU.
VRE_SCENARIO_LABELS <- c(
  "90" = "BAU",
  "70" = "Top30",
  "50" = "Top50",
  "30" = "Top70"
)

TX_LABEL <- "Existing/modelled transmission (2050)"

# Transmission lines: create_plot() in domestic_export_map_iterations.R never
# drew these (confirmed via git history -- the original logic lived in a
# script called map_seperate_domestic_export.R, deleted in a later "remove
# old code" commit, and the LLM reconstruction of domestic_export_map_
# iterations.R after a bad merge never re-added it). The interTX_2050_
# threshold_*.shp set only has threshold_0 anywhere in the deposited data,
# which can't cover thresholds 30/50/70/90. This uses the complete,
# per-threshold set found instead:
#   Tx_outputs/tx1_domestic_transmission/QLD_model_tx/transmission_y2050_t<threshold>.shp
load_tx_lines <- function(threshold) {
  tx_dir <- file.path(paths$tx_outputs, "tx1_domestic_transmission", "QLD_model_tx")
  tx_file <- file.path(tx_dir, sprintf("transmission_y2050_t%d.shp", threshold))
  if (nchar(tx_file) > 255) {
    message("  NOTE: transmission shapefile path is ", nchar(tx_file),
            " characters -- long paths can fail on Windows depending on ",
            "the R/GDAL build: ", tx_file)
  }
  if (!file.exists(tx_file)) {
    warning("Transmission shapefile not found for threshold ", threshold,
            ": ", tx_file, " -- panel will be drawn without transmission lines.")
    return(NULL)
  }
  tx <- sf::st_read(tx_file, quiet = TRUE)

  # lin_typ values confirmed from real data (threshold 30, n=192):
  #   bulk=18, interTX=28, sink=15, spur=131.
  # "spur" is the vast majority of rows and is exactly what was showing up
  # as unwanted spur lines on the map -- the deleted map_seperate_domestic_
  # export.R excluded these too, just via a layer-name regex
  # (grep("_spur", layer_name, invert = TRUE)) rather than this per-feature
  # field, since this shapefile is a single already-merged layer.
  tx <- tx[tx$lin_typ != "spur", ]

  # Of the 28 "interTX" rows, src_lyr shows exactly 14 are the real lines
  # (interTX_vtx1_30_B12_case2_2050) and 14 are a "_buff" (buffered)
  # duplicate of the same lines (interTX_vtx1_30_B12_case2_2050_buff) --
  # drawing both would double up those lines on the map. Drop the buffered
  # duplicates; "bulk" and "sink" src_lyr values don't have this issue.
  tx <- tx[!grepl("_buff$", tx$src_lyr), ]

  tx
}

build_vre_map <- function(threshold, show_legend = FALSE) {

  shp_name <- sprintf("combined_renewables_2050_threshold_%d.shp", threshold)

  # The original script's own output folder is empty on this machine (see
  # audit_report.md, open item re: results/figures/energy_maps/shapefiles_tx1).
  # Falls back to the pre-computed shapefiles shipped in the data archive,
  # which use the exact same filename convention (domestic_export_map_
  # iterations.R:122) and are almost certainly the same intermediate,
  # relocated -- confirm this once if the rendered panels look off.
  shp_dir_candidates <- c(
    here("results", "figures", "energy_maps", "shapefiles_tx1"),
    file.path(paths$tx_outputs, "domestic_tx1_shapefiles")
  )

  shp_file <- NULL
  for (d in shp_dir_candidates) {
    candidate <- file.path(d, shp_name)
    if (file.exists(candidate)) {
      shp_file <- candidate
      # Diagnostic: this candidate path was ~300 characters and my own
      # read tools got "permission denied" on its .prj sidecar (ls -la
      # showed a real size, so it's not an empty OneDrive placeholder) --
      # consistent with Windows' 260-character MAX_PATH limit, not
      # necessarily a problem for R/GDAL's own long-path-aware file access,
      # but worth knowing about if st_read() below throws an obscure error.
      if (nchar(candidate) > 255) {
        message("  NOTE: shapefile path is ", nchar(candidate),
                " characters -- long paths can fail on Windows depending ",
                "on the R/GDAL build: ", candidate)
      }
      break
    }
  }
  if (is.null(shp_file)) {
    stop("No combined_renewables shapefile found for threshold ", threshold,
         " in any of:\n  ", paste(shp_dir_candidates, collapse = "\n  "))
  }

  qld_boundary <- ozmaps::ozmap_states %>% dplyr::filter(NAME == "Queensland")

  infrastructure_data <- sf::st_read(shp_file, quiet = TRUE)

  if ("domestic" %in% colnames(infrastructure_data)) {
    # `infrastructure_data$domestic == 1L` alone is the actual bug behind
    # the missing offshore polygons: all 8 offshore rows at threshold 30
    # have domestic = NA (confirmed via inspect_threshold()), not 0 or 1 --
    # solar_pv/wind rows have real 0/1 values, so this looks specific to
    # whatever produced the offshore rows in this shapefile. `NA == 1L` is
    # NA, and R's data-frame row subsetting keeps an NA-indexed row but
    # nulls its contents rather than dropping it -- so those rows were
    # silently corrupted to empty/NA, not filtered out on purpose.
    #
    # Defaulting to KEEP unknown-domestic rows rather than drop them: this
    # is specifically the domestic-siting map, and excluding them was never
    # a deliberate decision, just an accident of the NA comparison. Flip
    # the condition below to `keep <- domestic_col == 1L` (no NA handling)
    # if you'd rather exclude unknown-domestic rows strictly.
    domestic_col <- infrastructure_data$domestic
    keep <- domestic_col == 1L
    n_na <- sum(is.na(domestic_col))
    if (n_na > 0) {
      keep[is.na(keep)] <- TRUE
      warning(sprintf(
        "threshold %d: %d row(s) have domestic = NA (not 0/1) -- kept, not dropped. Technology breakdown: %s",
        threshold, n_na,
        paste(names(table(infrastructure_data$technology[is.na(domestic_col)])),
              table(infrastructure_data$technology[is.na(domestic_col)]),
              sep = "=", collapse = ", ")
      ))
    }
    infrastructure_data <- infrastructure_data[keep, ]
  }

  # Fixed factor levels + drop = FALSE below guarantee the legend always
  # shows all 4 technology keys in the same order, even for a threshold
  # whose data happens not to contain every technology -- otherwise the
  # legend would vary panel-to-panel, which is its own kind of "wrong" for
  # panels meant to share one legend.
  #
  # trimws() first: DBF is a fixed-width string format and OGR doesn't
  # always strip trailing padding, so a value can come back as "offshore "
  # (trailing space) -- table()'s printed column header hides that, but it
  # fails an exact match against "offshore" in TECH_LEVELS and silently
  # becomes NA (no polygon drawn, and drop = FALSE then shows an empty/
  # colourless legend key for a level with zero actual rows -- exactly the
  # two symptoms reported).
  infrastructure_data$technology <- factor(trimws(as.character(infrastructure_data$technology)),
                                            levels = TECH_LEVELS)

  tx_lines <- load_tx_lines(threshold)

  p <- ggplot() +
    geom_sf(data = qld_boundary, fill = "white", color = "black", linewidth = 0.5)

  if (!is.null(tx_lines)) {
    # Fixed grey colour (not mapped -- doesn't need/get its own colour
    # scale); linetype is mapped to a constant label purely so a legend key
    # for "transmission" appears alongside the technology fill/colour keys.
    p <- p + geom_sf(data = tx_lines, aes(linetype = TX_LABEL),
                      color = "grey40", linewidth = 0.3)
  }

  # No alpha here -- deliberately, not an oversight. The reconstructed
  # domestic_export_map_iterations.R uses alpha = 0.7, but ggplot2/grid only
  # applies a fixed `alpha=` to a geom_sf polygon's FILL, not its mapped
  # outline colour -- so fill and outline of the "same" colour render at
  # different opacities and visibly don't match. The actual pre-
  # reconstruction script (map_seperate_domestic_export.R, recovered from
  # git history) used scales::alpha(colour, 1) for both fill and outline --
  # i.e. fully opaque, deliberately avoiding this. Matching that here.
  p <- p +
    geom_sf(data = infrastructure_data,
            aes(fill = technology, color = technology),
            linewidth = 0.1) +
    scale_fill_manual(values = TECH_COLORS, labels = TECH_LABELS, name = "Technology", drop = FALSE) +
    scale_color_manual(values = TECH_COLORS, labels = TECH_LABELS, name = "Technology", drop = FALSE) +
    scale_linetype_manual(values = setNames("solid", TX_LABEL), name = NULL) +
    theme_minimal() +
    theme(
      axis.text       = element_blank(),
      axis.ticks      = element_blank(),
      panel.grid      = element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )
  # No labs(title=, subtitle=, caption=) -- that content belongs in the
  # manuscript caption per NEE style, not baked into the panel image.
  p
}
