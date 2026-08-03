# =============================================================================
# Zonation Rank Map — Queensland
# =============================================================================
# Description: Loads Zonation rankmap.tif, reclassifies into 5 priority
#              categories, and plots over the QLD state boundary using a
#              Blue2DarkRed colour scheme (colorBlindness package).
#              Designed for Nature-style publication figures.
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI
# Date: Apr 2026
# =============================================================================

# Load required packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, tidyr, readr, here, terra,
  ozmaps, sf, colorBlindness
)

source(here::here("_paths.R"))
local_override <- here::here("_paths_local.R")
if (file.exists(local_override)) {
  source(local_override)
  cat(">>> Using local path overrides from _paths_local.R\n")
}

# =============================================================================
# USER CONTROL SETTINGS
# =============================================================================
overwrite_mode    <- TRUE   # Set TRUE to re-render figure even if it exists
overwrite_reclass <- FALSE   # Set TRUE to re-reclassify even if cached file exists

thin_rows    <- FALSE        # Set TRUE to thin raster rows for faster rendering
thin_every_n <- 5            # Keep every nth row when thinning (large datasets only)

# --- Colour Settings ---
# If manual_colours = FALSE, colours are drawn evenly from Blue2DarkRed12Steps
# If manual_colours = TRUE, the values in manual_colour_map are used instead
manual_colours <- TRUE

manual_colour_map <- c(
  "Top 90%" = "#e0cfa2",
  "Top 70%" = "#cbd5bc",
  "Top 50%" = "#85adaf",
  "Top 30%" = "#2887a1",
  "BAU"     = "#ac0505"
)

# =============================================================================
# Setup and Path Configuration
# =============================================================================
zonation_dir <- file.path(data_root, "Zonation_analysis", "Zonation_output")
output_dir   <- here("results", "zonation_figures")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

rankmap_path    <- file.path(zonation_dir, "250m_QLD_2024", "out_example1", "rankmap.tif")
reclass_path    <- file.path(zonation_dir, "250m_QLD_2024", "out_example1", "rankmap_classified.tif")
final_plot_path <- file.path(output_dir, "zonation_rankmap.png")

# =============================================================================
# Resolve Colour Palette
# =============================================================================
category_labels <- c("Top 90%", "Top 70%", "Top 50%", "Top 30%", "BAU")

if (manual_colours) {
  cat(">>> Using manual colour palette.\n")
  # Validate all categories are covered
  missing_cats <- setdiff(category_labels, names(manual_colour_map))
  if (length(missing_cats) > 0) {
    stop("Error: manual_colour_map is missing entries for: ",
         paste(missing_cats, collapse = ", "))
  }
  map_colours <- manual_colour_map[category_labels]
  
} else {
  cat(">>> Using Blue2DarkRed12Steps palette.\n")
  # Select 5 evenly spaced colours: index 1 = blue (low), index 12 = red (high)
  idx         <- round(seq(1, 12, length.out = 5))
  map_colours <- Blue2DarkRed12Steps[idx]
  names(map_colours) <- category_labels
  cat(">>> Selected palette indices:", idx, "\n")
  cat(">>> Colours:", map_colours, "\n")
}

# =============================================================================
# Load and Reclassify Raster (with caching)
# =============================================================================
if (file.exists(reclass_path) && !overwrite_reclass) {
  cat(">>> Loading cached classified raster from:\n   ", reclass_path, "\n")
  rank_reclass <- rast(reclass_path)
  
  # Restore factor levels (not preserved in GeoTIFF)
  levels(rank_reclass) <- data.frame(
    value = 1:5,
    label = category_labels
  )
  
} else {
  
  if (!file.exists(rankmap_path)) {
    stop("Error: Could not find rankmap at: ", rankmap_path)
  }
  
  cat(">>> Loading raw rankmap...\n")
  rank_rast <- rast(rankmap_path)
  
  cat(">>> Reclassifying raster...\n")
  rcl_matrix <- matrix(
    c(
      0.00, 0.10, NA,
      0.10, 0.30,  1,   # Top 90%
      0.30, 0.50,  2,   # Top 70%
      0.50, 0.70,  3,   # Top 50%
      0.70, 0.87,  4,   # Top 30%
      0.87, 1.00,  5    # BAU
    ),
    ncol = 3, byrow = TRUE
  )
  
  rank_reclass <- classify(rank_rast, rcl_matrix, include.lowest = TRUE)
  
  levels(rank_reclass) <- data.frame(
    value = 1:5,
    label = category_labels
  )
  
  cat(">>> Saving classified raster to:\n   ", reclass_path, "\n")
  writeRaster(rank_reclass, reclass_path, overwrite = TRUE)
  cat("✓ Classified raster saved.\n")
}

# =============================================================================
# Reproject to EPSG:3857
# =============================================================================
cat(">>> Reprojecting raster to EPSG:3857...\n")
rank_reclass_3857 <- project(rank_reclass, "EPSG:3857", method = "near")

# Restore factor levels after reprojection (terra may drop them)
levels(rank_reclass_3857) <- data.frame(
  value = 1:5,
  label = category_labels
)

# =============================================================================
# Load Queensland Boundary
# =============================================================================
cat(">>> Loading QLD boundary...\n")
qld_border <- ozmaps::ozmap_states %>%
  filter(NAME == "Queensland") %>%
  st_transform(3857)

# =============================================================================
# Convert Raster to Data Frame for ggplot
# =============================================================================
cat(">>> Converting raster to data frame...\n")

rank_df <- as.data.frame(rank_reclass_3857, xy = TRUE) %>%
  rename(label = 3) %>%
  filter(!is.na(label)) %>%
  mutate(label = factor(label, levels = category_labels))

cat(">>> Data frame ready:", nrow(rank_df), "cells.\n")
cat(">>> Estimating tile size from raster resolution...\n")

# Derive tile dimensions from raster resolution (required for geom_tile after reprojection)
res_xy    <- res(rank_reclass_3857)
tile_w    <- res_xy[1]
tile_h    <- res_xy[2]

cat(">>> Tile size:", round(tile_w), "x", round(tile_h), "metres (EPSG:3857)\n")

# Aggregate to reduce cell count if dataset is very large
# Increases speed significantly for 250m QLD-wide rasters
agg_factor <- 4   # increase further if still slow (4 = effective 1km resolution)
cat(">>> Aggregating raster by factor", agg_factor, "to reduce render time...\n")
rank_agg <- aggregate(rank_reclass_3857, fact = agg_factor, fun = "modal")
levels(rank_agg) <- data.frame(value = 1:5, label = category_labels)

rank_df <- as.data.frame(rank_agg, xy = TRUE) %>%
  rename(label = 3) %>%
  filter(!is.na(label)) %>%
  mutate(label = factor(label, levels = category_labels))

res_agg  <- res(rank_agg)
tile_w   <- res_agg[1]
tile_h   <- res_agg[2]

cat(">>> Aggregated data frame ready:", nrow(rank_df), "cells.\n")
cat(">>> Aggregated tile size:", round(tile_w), "x", round(tile_h), "metres\n")
# =============================================================================
# Build Plot
# =============================================================================
cat(">>> Building plot...\n")

p <- ggplot() +
  geom_tile(
    data = rank_df,
    aes(x = x, y = y, width = tile_w, height = tile_h, fill = label)
  ) +
  scale_fill_manual(
    values   = map_colours,
    na.value = "transparent",
    name     = "Priority Class",
    guide    = guide_legend(reverse = FALSE)
  ) +
  geom_sf(
    data      = qld_border,
    fill      = "transparent",
    colour    = "black",
    linewidth = 0.4
  ) +
  coord_sf(crs = 3857) +
  theme_void(base_size = 11) +
  theme(
    legend.position   = "none", #c(0.18, 0.55),
    legend.title      = element_text(face = "bold", size = 9),
    legend.text       = element_text(size = 8),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key.size   = unit(0.45, "cm"),
    plot.title        = element_text(face = "bold", hjust = 0.5),
    plot.margin       = margin(5, 5, 5, 5)
  ) #+
  #labs(title = "Zonation Priority Ranking — Queensland")


print(p)
# =============================================================================
# Save and Display
# =============================================================================
if (!file.exists(final_plot_path) || overwrite_mode) {
  ggsave(final_plot_path, plot = p, width = 7, height = 8, dpi = 300, bg = "white")
  cat("✓ Plot saved to:", final_plot_path, "\n")
} else {
  cat(">>> Plot already exists (set overwrite_mode = TRUE to regenerate).\n")
}

cat("\n=== RANKMAP FIGURE COMPLETE ===\n")