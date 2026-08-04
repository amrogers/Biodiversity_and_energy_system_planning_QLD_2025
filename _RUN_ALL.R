# =============================================================================
# MASTER RUNNER: Biodiversity and Energy System Planning QLD 2025
# =============================================================================
# Author: Andrew Rogers
# LLMs used: Claude AI and Gemini
# Date: Jan 2026; Updated: Aug 2026
# =============================================================================
# Purpose: Executes the full analysis pipeline in two independent steps.
#
#   Step 1 -- Main manuscript figures and tables:
#     Figure 1a  -- Biodiversity prioritisation map (ArcGIS Pro -- path printed)
#     Figure 1b-e -- VRE siting maps (three-tier: see below)
#     Table 1    -- All-MNES scenario coverage (Mean_spp_scenario_coverage.R)
#     Table 1    -- CE/EN mean coverage (data table path printed to console)
#     Figure 2   -- NPV bar chart (NPV_bar_plot.R)
#     Figure 3   -- New TX build length (tx_length_figure.R)
#     Figure 4   -- Cost increase line plot (percent cost increase_line plot.R)
#
#   Step 2 -- Supplementary material figures:
#     Supp. Fig 1D -- Zonation performance curves (Zonation curves.R)
#     Supp. Fig 2  -- Zero-coverage species map (zero_coverage_species.R)
#     Supp. Fig 6  -- Exclusion area barplot (land_use_competition_QLD.R -> exclusion_overlap_barplot.R)
#
# Figure 1b-e three-tier fallback (controlled by overwrite_mode / tx2_enabled):
#   Tier 1 -- Final PNGs present and overwrite_mode FALSE: SKIPPED (maps exist)
#   Tier 2 -- Shapefiles present, PNGs absent, overwrite_mode FALSE:
#             source energy_maps_figure.R -> COMPUTED (from cached shapefiles)
#   Tier 3 -- Neither present, or overwrite_mode TRUE:
#             source domestic_export_map_iterations.R then energy_maps_figure.R
#             -> COMPUTED (full GDB processing) -- WARNING: may take 30+ minutes
#
# Set display_mode = TRUE to review existing outputs without rerunning anything.
# =============================================================================

# 1. INITIAL SETUP
if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, magick, readr)

# --- USER CONTROL ---
run_step1      <- TRUE   # Paper figures (ignored when display_mode = TRUE)
run_step2      <- TRUE   # Supplementary figures (ignored when display_mode = TRUE)
display_mode   <- FALSE  # TRUE = show existing outputs; FALSE = run scripts and save
overwrite_mode <- FALSE  # TRUE = always run full pipeline (Tier 3) for Figure 1b-e
tx2_enabled    <- FALSE  # TRUE = also process TX2 maps (only TX1 appears in manuscript)

cat("\n Starting Full Analysis Pipeline...\n")
start_time <- Sys.time()

# =============================================================================
# 2. DEFINE PIPELINE STEPS (Figure 1b-e handled separately via three-tier fallback)
# =============================================================================

pipeline_step1 <- list(
  "Table 1 (all MNES)"   = here("Biodiversity_analysis", "Mean_spp_scenario_coverage.R"),
  "Figure 2 (NPV)"       = here("Figure_code", "NPV_bar_plot.R"),
  "Figure 3 (TX length)" = here("Figure_code", "tx_length_figure.R"),
  "Figure 4 (costs)"     = here("Figure_code", "percent cost increase_line plot.R")
)

pipeline_step2 <- list(
  "Supp. Fig 1D (Zonation curves)"          = here("Figure_code", "Zonation curves.R"),
  "Supp. Fig 2 (zero-coverage spp.)"        = here("Biodiversity_analysis", "zero_coverage_species.R"),
  "Supp. Fig 6 data (land use competition)" = here("Biodiversity_analysis", "land_use_competition_QLD.R"),
  "Supp. Fig 6 (exclusion barplot)"         = here("Figure_code", "exclusion_overlap_barplot.R")
)

# =============================================================================
# 3. OUTPUT FILE MAP (used by display_mode = TRUE)
# =============================================================================

outputs_step1 <- list(
  "Figure 1a (priority map)"  = list(
    path = here("results", "figures",
                "Figure 1_protection scenario map and 2050 VRE mapping.tif"),
    type = "note",
    note = "Produced in ArcGIS Pro from Zonation rankmap.tif -- no R output."
  ),
  "Figure 1b-e (VRE maps)"    = list(
    path = here("results", "figures", "energy_maps"),
    type = "folder"
  ),
  "Table 1 (all MNES)"        = list(
    path = here("results", "tables", "scenario_coverage_results.csv"),
    type = "csv"
  ),
  "Table 1 (CE/EN)"           = list(
    path = here("results", "tables", "CE_EN_mean_coverage_results.csv"),
    type = "csv"
  ),
  "Figure 2 (NPV)"            = list(
    path = here("results", "figures", "npv_analysis_plot.png"),
    type = "png"
  ),
  "Figure 3 (TX length)"      = list(
    path = here("results", "figures", "tx_length_figure.png"),
    type = "png"
  ),
  "Figure 4 (costs)"          = list(
    path = here("results", "figures", "energy_cost_increase_plot.png"),
    type = "png"
  )
)

outputs_step2 <- list(
  "Supp. Fig 1D (Zonation curves)"   = list(
    path = here("results", "zonation_figures", "zonation_performance_curves.png"),
    type = "png"
  ),
  "Supp. Fig 2 (zero-coverage spp.)" = list(
    path = here("results", "zero_coverage", "species_distribution_map.png"),
    type = "png"
  ),
  "Supp. Fig 6 (exclusion barplot)"  = list(
    path = here("results", "figures", "Exclusions_stacked_bar_plot.png"),
    type = "png"
  )
)

# =============================================================================
# 4. DISPLAY ENGINE (display_mode = TRUE)
# =============================================================================

display_results <- function(outputs, step_label) {
  log <- data.frame(
    Output = names(outputs),
    Status = NA_character_,
    Path   = sapply(outputs, `[[`, "path"),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(outputs)) {
    item      <- outputs[[i]]
    item_name <- names(outputs)[i]
    p         <- item$path
    type      <- item$type

    cat(sprintf("\n--- %s: %s ---\n", step_label, item_name))

    if (type == "note") {
      cat(" ", item$note, "\n")
      log$Status[i] <- "Note (ArcGIS Pro)"
      next
    }

    if (type == "folder") {
      if (dir.exists(p)) {
        pngs <- list.files(p, pattern = "\\.png$", recursive = TRUE, full.names = FALSE)
        cat(sprintf("  Folder present: %d PNG file(s)\n", length(pngs)))
        if (length(pngs) > 0) cat(paste0("    ", head(pngs, 6), collapse = "\n"), "\n")
        log$Status[i] <- sprintf("Present (%d PNGs)", length(pngs))
      } else {
        cat("  Folder not found:", p, "\n")
        log$Status[i] <- "Missing"
      }
      next
    }

    if (!file.exists(p)) {
      cat("  File not found:", p, "\n")
      log$Status[i] <- "Missing"
      next
    }

    log$Status[i] <- "Present"

    if (type == "png") {
      cat("  Displaying:", basename(p), "\n")
      tryCatch(
        print(magick::image_read(p)),
        error = function(e) cat("  Could not display image:", e$message, "\n")
      )
    } else if (type == "csv") {
      cat("  Reading:", basename(p), "\n")
      tryCatch({
        df <- readr::read_csv(p, show_col_types = FALSE)
        cat(sprintf("  %d rows x %d columns\n", nrow(df), ncol(df)))
        print(as.data.frame(df))
      }, error = function(e) cat("  Could not read CSV:", e$message, "\n"))
    }
  }
  log
}

# =============================================================================
# 5. EXECUTION ENGINE (display_mode = FALSE)
# =============================================================================

run_pipeline <- function(pipeline, step_label) {
  log <- data.frame(
    Step   = names(pipeline),
    Status = "Pending",
    Time   = NA_character_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(pipeline)) {
    step_name <- names(pipeline)[i]
    step_path <- pipeline[[i]]
    cat(sprintf("\n--- %s: %s ---\n", step_label, step_name))
    if (file.exists(step_path)) {
      tryCatch({
        source(step_path)
        log$Status[i] <- "Success"
      }, error = function(e) {
        log$Status[i] <<- paste("Failed:", e$message)
      })
    } else {
      log$Status[i] <- "File Not Found"
    }
    log$Time[i] <- format(Sys.time(), "%H:%M:%S")
  }
  log
}

# =============================================================================
# 6. RUN
# =============================================================================

all_logs <- list()

if (display_mode) {

  cat("\n", strrep("=", 50), "\n")
  cat("DISPLAY MODE: Reviewing existing outputs\n")
  cat(strrep("=", 50), "\n")
  all_logs[["Step 1: Main Manuscript Figures & Tables"]] <- display_results(outputs_step1, "Step 1")
  all_logs[["Step 2: Supplementary Figures"]]            <- display_results(outputs_step2, "Step 2")

} else {

  source(here::here("_paths.R"))
  local_override <- here::here("_paths_local.R")
  if (file.exists(local_override)) {
    source(local_override)
    cat(">>> Using local path overrides from _paths_local.R\n")
  }

  if (run_step1) {
    cat("\n", strrep("=", 50), "\n")
    cat("STEP 1: Main Manuscript Figures & Tables\n")
    cat(strrep("=", 50), "\n")

    cat("\n--- Step 1: Figure 1a (Biodiversity prioritisation map) ---\n")
    cat("  NOTE: Figure 1a was produced in ArcGIS Pro using the Zonation rankmap.\n")
    cat("  Source raster:", paths$rankmap, "\n")
    cat("  Status: Skipped (ArcGIS Pro output)\n")

    cat("\n--- Step 1: Table 1 (CE/EN mean coverage) ---\n")
    cat("  NOTE: CE/EN mean coverage results are pre-computed.\n")
    cat("  Data table:", here("results", "tables", "CE_EN_mean_coverage_results.csv"), "\n")
    cat("  To regenerate, run: Figure_code/Critically_endangered_mean_coverage_and_line_plot.R\n")
    cat("  Status: Skipped (pre-computed)\n")

    # --- Figure 1b-e: Three-tier fallback ---
    cat("\n--- Step 1: Figure 1b-e (VRE maps) ---\n")
    tx_to_check <- if (tx2_enabled) c("tx1", "tx2") else "tx1"

    check_pngs <- function(tx) {
      d <- here("results", "figures", "energy_maps", paste0("domestic_maps_", tx))
      dir.exists(d) && length(list.files(d, pattern = "\\.png$")) > 0
    }
    check_shps <- function(tx) {
      d <- here("results", "figures", "energy_maps", paste0("shapefiles_", tx))
      dir.exists(d) && length(list.files(d, pattern = "\\.shp$")) > 0
    }
    pngs_present <- all(sapply(tx_to_check, check_pngs))
    shps_present <- all(sapply(tx_to_check, check_shps))

    vre_t0 <- Sys.time()
    vre_status <- tryCatch({
      if (!overwrite_mode && pngs_present) {
        cat("  Final PNGs found -- skipping.\n")
        "SKIPPED (maps exist)"
      } else if (!overwrite_mode && shps_present) {
        cat("  Shapefiles found; rendering PNGs only (Tier 2)...\n")
        source(here("Figure_code", "energy_maps_figure.R"))
        "COMPUTED (from cached shapefiles)"
      } else {
        cat("  WARNING: No cached shapefiles. Full GDB processing required.\n")
        cat("  This may take 30+ minutes depending on hardware.\n")
        source(here("Energy system and transmission analysis",
                    "domestic_export_map_iterations.R"))
        source(here("Figure_code", "energy_maps_figure.R"))
        "COMPUTED (full GDB processing)"
      }
    }, error = function(e) paste("Failed:", e$message))

    vre_elapsed <- round(as.numeric(difftime(Sys.time(), vre_t0, units = "mins")), 2)
    cat(sprintf("  Elapsed: %.2f min\n", vre_elapsed))

    vre_log_row <- data.frame(
      Step   = "Figure 1b-e (VRE maps)",
      Status = sprintf("%s (%.2f min)", vre_status, vre_elapsed),
      Time   = format(Sys.time(), "%H:%M:%S"),
      stringsAsFactors = FALSE
    )

    step1_rest <- run_pipeline(pipeline_step1, "Step 1")
    step1_log  <- rbind(vre_log_row, step1_rest)
    all_logs[["Step 1: Main Manuscript Figures & Tables"]] <- step1_log
  }

  if (run_step2) {
    cat("\n", strrep("=", 50), "\n")
    cat("STEP 2: Supplementary Material Figures\n")
    cat(strrep("=", 50), "\n")
    all_logs[["Step 2: Supplementary Figures"]] <- run_pipeline(pipeline_step2, "Step 2")
  }

}

# =============================================================================
# 7. FINAL STATUS REPORT
# =============================================================================

cat("\n", strrep("=", 50), "\n")
if (display_mode) {
  cat("OUTPUT PRESENCE REPORT\n")
} else {
  cat("FINAL PIPELINE REPORT\n")
}
cat(strrep("=", 50), "\n")

for (label in names(all_logs)) {
  cat(sprintf("\n%s\n", label))
  print(all_logs[[label]], row.names = FALSE)
}

end_time <- Sys.time()
cat(sprintf("\nTotal Runtime: %0.2f minutes\n",
            as.numeric(difftime(end_time, start_time, units = "mins"))))
if (!display_mode) cat("All outputs are available in the /results folder.\n")
cat(strrep("=", 50), "\n")

# =============================================================================
# 8. SESSION INFO (for reproducibility)
# =============================================================================

cat("\nSESSION INFO\n")
cat(strrep("=", 50), "\n")
print(sessionInfo())
