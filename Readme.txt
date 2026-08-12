Supplementary Materials: Biodiversity and Energy System Planning - Queensland 2025
See also: README.md in this folder for the formatted Markdown version of these instructions.

Overview
--------
This repository contains analysis scripts and project configurations for the study
"Biodiversity and Energy System Planning in Queensland: Balancing Conservation and
Infrastructure Development". The materials include energy system modelling results,
conservation priority analyses, and spatial mapping tools.

Note on Data: Due to size constraints, the raw spatial data (approx. 7.8 GB) is
hosted separately on Figshare: https://doi.org/10.26188/29604590. This repository
provides the code to process that data into the final results and figures.


Data
----
Data used for this study can be found on Figshare:
https://figshare.unimelb.edu.au/articles/dataset/Supplementary_Data_Biodiversity_and_Energy_System_Planning_-_Queensland_2025/29604590

Recommended citation:
Rogers, Andrew (2025). Supplementary Data: Biodiversity and Energy System Planning -
Queensland 2025. The University of Melbourne. Dataset.
https://doi.org/10.26188/29604590.v1


What Figshare Delivers
----------------------
The Figshare deposit is TWO separate downloads. Both are placed inside the cloned
GitHub repository, at the same level as _RUN_ALL.R:

  BESP_data_qld_2025  -> place at <repo>/BESP_data_qld_2025/ (same relative
                          location as in this repo). Contains the raw input data:
                          Energy_system_model_outputs/, Zonation_analysis/.

  results             -> place at <repo>/results/ (repo root, alongside
                          BESP_data_qld_2025/). SHIPS PRE-POPULATED with every
                          figure and table already generated. Running
                          source("_RUN_ALL.R") will detect these and redraw them
                          (~2 min) instead of recomputing from scratch. Delete or
                          empty results/ to force a full recompute (60+ minutes
                          for the spatial steps).

Wrapper directories: depending on how each item downloads and unzips, you may end up
with an extra nested folder. If so, move the INNER folder up one level so that
BESP_data_qld_2025/ and results/ each sit directly in the repository root -- verify
against the tree below before running anything.

The code folders (Biodiversity_analysis/, Figure_code/, Energy system and
transmission analysis/) are NOT part of the Figshare deposit -- they come from the
GitHub repository itself.

Repository Structure
--------------------
The project is organised as an R Project. Open the .Rproj file to automatically set
the correct working directory — no need to manually set paths.

Biodiversity_and_energy_system_planning_2024/
├── Biodiversity_and_energy_system_planning_2024.Rproj  # <-- Open this first
├── _RUN_ALL.R                          # Master runner — executes full pipeline
├── _paths.R                            # Central path management (sourced by scripts)
├── BESP_data_qld_2025/                 # <-- Figshare download 1 goes here
│   ├── Energy_system_model_outputs/    # Processed CSV/XLSX files, GDBs
│   └── Zonation_analysis/              # Zonation run files, weights, and outputs
├── Figure_code/                        # R analysis and figure scripts
├── Biodiversity_analysis/              # Biodiversity analysis scripts
├── Energy system and transmission analysis/  # Transmission pipeline scripts
├── results/                            # <-- Figshare download 2 goes here (pre-populated)
│   ├── figures/                        # PNG outputs
│   ├── tables/                         # Summary CSV outputs
│   ├── zonation_figures/               # Zonation performance curve outputs
│   ├── zero_coverage/                  # Supp. Fig 2 outputs
│   ├── transmission_processing/        # TX pipeline intermediates
│   └── transmission_scenario_comparison/  # Supp. Table 2 outputs
└── Readme.txt


Data Files Description
----------------------

Energy System Data
- QLD_v202412_eplus_tx1.gdb: Energy infrastructure scenarios, transmission option 1
- QLD_v202412_eplus_tx2.gdb: Energy infrastructure scenarios, transmission option 2
- cost_increase_results.csv: Cost increases under different biodiversity protection levels
- eplus_Domestic_NPV_2025.xlsx: Net present value analysis for domestic energy projects

Conservation Data
- feature_curves.csv: Zonation performance curves for 545 conservation features
- Species_files_weights_table.xlsx: Weighting schemes for species in conservation planning
- Supplementary table 9. EPBC listed species and ecological communities and their associated threat status.xlsx: national threat classifications for species and ecological communities

Spatial Exclusions
- Suplementary table_other spatial exclusions.xlsx: Non-biodiversity spatial constraints


Zip Archives in the Deposit
----------------------------
Every .zip file in BESP_data_qld_2025/ and how it gets extracted:

  QLD_v202412_eplus_tx1.gdb.zip
    Location:   Energy_system_model_outputs/Energy_system_analysis_scenarios/
    Extraction: AUTOMATIC. Energy system and transmission analysis/
                domestic_export_map_iterations.R extracts it on first run if
                QLD_v202412_eplus_tx1.gdb/ is not already present. No manual
                action needed.

  QLD_v202412_eplus_tx2.gdb.zip
    Location:   same folder
    Extraction: AUTOMATIC, same script and mechanism.

  Zonation_QLD_biodiversity_feature_rasters.zip
    Location:   Zonation_analysis/
    Extraction: MANUAL. No script extracts this -- it is only needed if you
                re-run Zonation from scratch (see "Re-running the Zonation
                Analysis" below). Unzip it yourself before attempting a
                Zonation re-run; the R pipeline (_RUN_ALL.R) never reads it.

Expected disk space after extraction (zips are kept after extracting, not
deleted -- extracted size is IN ADDITION TO the zip):
  QLD_v202412_eplus_tx1.gdb.zip                    1.10 GB zipped -> ~1.1 GB extracted
  QLD_v202412_eplus_tx2.gdb.zip                    2.85 GB zipped -> ~0.96 GB extracted
  Zonation_QLD_biodiversity_feature_rasters.zip    81 MB zipped   -> ~730 MB extracted
                                                    (only if manually unzipped)

Allow roughly an extra 2 GB free for the two GDBs once auto-extracted, plus ~730 MB
more if you unzip the Zonation rasters for a from-scratch Zonation run.


Analysis Scripts
----------------
All scripts are in the Figure_code/ folder. The recommended way to run them is via
_RUN_ALL.R (see Getting Started below). Scripts can also be run individually.

1. Species Coverage (Critically_endangered_mean_coverage_and_line_plot.R)
   Purpose:  Calculates mean coverage for critically endangered and endangered species
             from Zonation outputs at key landscape protection thresholds.
   Input:    Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_curves.csv
             Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/species_weights.csv
   Outputs:  results/tables/CE_EN_mean_coverage_results.csv
             results/figures/CE_EN_mean_coverage_plot.png

2. Zonation Performance Curves (Zonation curves.R)
   Purpose:  Plots performance curves for all 545 conservation features, with national
             mean and 95% confidence interval overlay.
   Input:    Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_curves.csv
   Output:   results/zonation_figures/zonation_performance_curves.png

3. Cost Analysis (percent cost increase_line plot.R)
   Purpose:  Analyses and visualises energy cost increases under different High
             Biodiversity Value Area (HBVA) exclusion scenarios.
   Input:    Energy_system_model_outputs/cost_increase_results.csv
   Output:   results/figures/energy_cost_increase_plot.png

4. NPV Analysis (NPV_bar_plot.R)
   Purpose:  Creates Figure 2 — bar plots showing Net Present Value of energy
             investments across protection thresholds and years.
   Input:    Energy_system_model_outputs/eplus_Domestic_NPV_figure.csv
   Output:   results/figures/npv_analysis_plot.png

5. Spatial Mapping (domestic_export_map_iterations.R)
   Purpose:  Generates Figure 1b-e — cost-optimised VRE siting maps for BAU and
             biodiversity protection scenarios (Top 30/50/70%). Processes TX1/TX2
             GDB files across thresholds and years, producing shapefiles and PNG maps.
   Input:    BESP_data_qld_2025/QLD_v202412_eplus_tx1.gdb (or tx2)
   Output:   results/figures/energy_maps/

6. NPV Analysis (NPV_bar_plot.R)
   Purpose:  Creates Figure 2 — stacked bar plots showing Net Present Value of VRE
             and transmission investments by year (2030/2040/2050) and threshold.
   Input:    BESP_data_qld_2025/Energy_system_model_outputs/eplus_Domestic_NPV_figure.csv
   Output:   results/figures/npv_analysis_plot.png

7. Exclusion Area Barplot (exclusion_overlap_barplot.R)
   Purpose:  Stacked bar chart showing wind and solar PV land area excluded and
             available under each biodiversity protection threshold. Prints a
             summary table of total area and exclusion percentage to the console.
   Input:    Energy_system_model_outputs/BV_exclusion_area_overlap.csv
   Output:   results/figures/Exclusions_stacked_bar_plot.png


Transmission Pipeline Scripts (Energy system and transmission analysis/transmission_mapping/)
---------------------------------------------------------------------------------------------------
These scripts process energy model GDB outputs through a spatial pipeline to produce
the transmission upgrade and length summaries used for Figure 3. Run in order for
each TX scenario (tx1 and tx2). Scripts are not included in _RUN_ALL.R because they
require large intermediate spatial outputs and the existing TX reference shapefiles
(see Spatial Reference Data below).

Pipeline for Figure 3 (run in order):

  Step 1 — Transmission_processing.R
    Purpose:  Reads domestic transmission line layers from the energy model GDB and
              saves per-threshold shapefiles and a combined GeoPackage.
    Input:    BESP_data_qld_2025/QLD_v202412_eplus_tx1.gdb (or tx2)
    Output:   results/transmission_processing/tx1/TX_domestic_layers/
                   transmission_y2050_t{N}.shp

  Step 2 — Transmission_save_layers_as_shapefiles.R
    Purpose:  Extracts individual threshold layers from the GeoPackage as separate
              ESRI Shapefiles.
    Input:    results/transmission_processing/tx1/split_tx.gpkg
    Output:   results/transmission_processing/tx1/TX_domestic_layers/

  Step 3 — QLD_new_tx_processing_summary.R
    Purpose:  Clips modelled TX lines to remove segments overlapping the existing
              network (st_difference), then summarises new-only TX length (km)
              by voltage class per threshold.
    Input:    results/transmission_processing/tx1/TX_domestic_layers/
              BESP_data_qld_2025/Spatial_reference_data/
                   Electricity_Transmission_Lines_1km_buff.shp
    Output:   results/transmission_processing/tx1/QLD_threshold_tx_new_summaries/
                   QLD_threshold_tx_new_summary.csv
    Note:     Copy output CSV to:
              BESP_data_qld_2025/Energy_system_model_outputs/tx1_new_transmission_summary.csv

  Step 4 — Figure_code/tx_length_figure.R (run via _RUN_ALL.R)
    Purpose:  Produces Figure 3 — new transmission build length (km) vs biodiversity
              avoidance scenario for TX1 and TX2, stacked with the existing network.
    Input:    BESP_data_qld_2025/Energy_system_model_outputs/
                   Electricity_Transmission_Lines/Tx_outputs/
                   tx1_domestic_transmission/QLD_threshold_tx_new/
                        QLD_threshold_tx1_new_summary.csv
                   tx2_domestic_transmission/QLD_threshold_tx_new/
                        QLD_threshold_tx2_new_summary.csv
    Output:   results/figures/tx_length_figure.png
              results/tables/tx_new_build_length_tx1_tx2.csv
    Note:     Two other scripts in transmission_mapping/ also produce Figure-3-like
              plots from earlier drafts (tx_length_figure_superseded.R,
              transmission_length_tx1_tx2.R) but are not part of the pipeline --
              Figure_code/tx_length_figure.R is the only one _RUN_ALL.R calls.

Supporting transmission scripts (not required for Figure 3):

  Transmission_clip_join_w_existing.R
    Purpose:  Clips modelled TX to existing TX buffer and spatially joins attributes.
              Used in upgrade area calculations.
    Output:   results/transmission_processing/tx1/tx_overlap_tables/

  Transmission_upgrade_calculation.R
    Purpose:  Calculates transmission easement area difference between modelled
              upgrades and the existing network. Requires ArcGIS-produced join
              tables as input (see script header).
    Output:   results/transmission_processing/{scenario}/QLD_ex_mod_summaries/
                   summarized_transmission_t{N}.xlsx
                   total_area_increase_{scenario}.xlsx

  QLD_existing_model_tx_max_join.R
    Purpose:  Joins modelled TX to simplified existing network, recording
              maximum modelled kV per existing corridor.
    Output:   results/transmission_processing/tx2/existing_model_tx_max_join/

  QLD_modelled_tx_easement_area.R
    Purpose:  Buffers modelled TX centrelines by voltage-based easement width
              and calculates total easement area (km²) per threshold.
    Output:   results/transmission_processing/tx1/tx_simplified_buffered/
                   threshold_areas.csv

  QLD_tranmission_summaries_modelled_existing.R
    Purpose:  Alternative summary — clips modelled TX to existing network and
              reports remaining lengths by voltage class.

  QLD_reproject_tx_lines.R
    Purpose:  One-off utility. Reprojects the existing TX lines shapefile to
              GDA2020 MGA Zone 55 and calculates line lengths in km.

  QLD_summarize_existing_in_simplified_tx.R
    Purpose:  Utility fragment. Summarises existing TX attributes into a
              simplified corridor network. Requires `simplified_tx` to be
              loaded before running (see script header).

Spatial Reference Data
  The transmission pipeline scripts require the following shapefiles located in:
    BESP_data_qld_2025/Energy_system_model_outputs/Electricity_Transmission_Lines/
  - Electricity_Transmission_Lines.shp
  - Electricity_Transmission_Lines_1km_buff.shp
  - QLD_existing_tx_simplified_buff_no_overlap2.shp
  Source: QLD electricity transmission network (state government spatial data).
  Configure paths via paths$existing_tx, paths$existing_tx_buff, paths$existing_tx_simp
  in _paths.R.

Note on duplicate scripts: The root-level Energy system and transmission analysis/
folder contains older versions of these scripts. The canonical current versions are in
the transmission_mapping/ subfolder.


Supporting Scripts (Biodiversity_analysis/)
---------------------------------------------
retrieve_spp_details.R is not included in _RUN_ALL.R and requires access to raw
species distribution shapefiles. zero_coverage_species.R IS included in _RUN_ALL.R
(Supp. Fig 2).

retrieve_spp_details.R
  Purpose:  Extracts species attributes (scientific name, common name, EPBC threat
            status) from individual species distribution shapefiles and compiles
            them into a single lookup CSV. Not run by default -- its output
            (species_attributes.csv) already ships pre-computed in the deposit.
  Input:    BESP_data_qld_2025/Zonation_analysis/Zonation_MNES_shapefiles/shapefiles/
            (individual .shp files per species)
  Output:   BESP_data_qld_2025/Zonation_analysis/Zonation_MNES_shapefiles/
            species_attributes.csv

zero_coverage_species.R
  Purpose:  Identifies species with zero coverage in the Zonation output and maps
            their distributions with CAPAD protected area boundaries overlaid.
            Used to investigate which species receive no representation at any
            priority threshold. Run automatically by _RUN_ALL.R (Supp. Fig 2).
  Input:    BESP_data_qld_2025/Zonation_analysis/Zonation_MNES_shapefiles/shapefiles/
            (selected species .shp files)
            BESP_data_qld_2025/Zonation_analysis/QLD_CAPAD/CAPAD_QLD.shp
  Output:   results/zero_coverage/species_distribution_map.png
            results/zero_coverage/map_number_lookup.csv

RZ_area_outside_exclusions_and_ECNES.R
  STATUS:   Not in _RUN_ALL.R. Superseded by land_use_competition_QLD.R as the data
            source for Supp. Fig 4 (see Version History, v1.2). Requires proprietary
            inputs not included in the Figshare deposit -- kept for reference only.
  Purpose:  Calculates the area of biodiversity red zones remaining outside
            renewable energy exclusion areas (PV and wind separately) using spatial
            difference operations. Supports optional parallel processing.
  Input:    data/AUS_area_outside_exclusion_zones.gdb (not in deposit)
            data/Red_zones_QLD.shp (not in deposit)
  Output:   Printed summary table; optional shapefiles via save_results()


Pre-computed LCOE Analysis Outputs
------------------------------------
The following files in BESP_data_qld_2025/Energy_system_model_outputs/ are
pre-computed outputs from Figure_code/LCOE_BV_exclusion_summary.R. They are
included in the Figshare data so that LCOE results can be reviewed without
re-running the full spatial analysis (which requires the raw GDB files and a
Zonation exclusion threshold shapefile not distributed separately).

LCOE maps and histograms:
  LCOE_exclusion_map_pv.jpg     Spatial map of PV Levelized Cost of Electricity
                                  overlaid with biodiversity exclusion zones
  LCOE_exclusion_map_wind.jpg   As above for wind energy
  LCOE_histograms_pv.jpg        LCOE distribution histograms by exclusion zone (PV)
  LCOE_histograms_wind.jpg      As above for wind energy

LCOE summary statistics:
  LCOE_statistics_pv.csv        Mean, median, SD, and range of LCOE by exclusion
                                  zone for solar PV
  LCOE_statistics_wind.csv      As above for wind energy
  LCOE_statistics_combined.csv  Combined statistics across both technologies

Note: These outputs are not reproduced by _RUN_ALL.R. To regenerate them, run
Figure_code/LCOE_BV_exclusion_summary.R directly after updating its file paths.


Manuscript Figure Reference
----------------------------
Script → Manuscript figure mapping:
  Biodiversity_value_map.R                             → Figure 1a (R rendition; ArcGIS
                                                          Pro produces the manuscript figure)
  domestic_export_map_iterations.R                     → Figure 1b-e (VRE siting maps)
  NPV_bar_plot.R                                       → Figure 2 (NPV bar chart)
  Transmission_processing.R  }
  Transmission_save_layers_as_shapefiles.R  }          → Figure 3 (transmission length)
  QLD_new_tx_processing_summary.R  }                      (pipeline steps 1-3, via tx_run_all.R)
  Figure_code/tx_length_figure.R  }                       (step 4, via _RUN_ALL.R)
  percent cost increase_line plot.R                    → Figure 4 (cost increase)
  Critically_endangered_mean_coverage_and_line_plot.R  → Table 1 / line plot
  2050_domestic_CPA_comparison.R                       → Supplementary Table 2
                                                          (TX1 vs TX2 spatial comparison)
  Zonation curves.R                                    → Supplementary Fig 1D
  zero_coverage_species.R                              → Supplementary Fig 2
  exclusion_overlap_barplot.R                          → Supplementary Fig 4
  LCOE_BV_exclusion_summary.R                          → Supplementary (LCOE maps)


Getting Started
---------------

Prerequisites — R packages (automatically installed on first run via pacman):
  sf, dplyr, ggplot2, readr, readxl, tidyr, furrr, data.table, progress,
  ozmaps, purrr, ggpattern, gridExtra, here, scales, magick

This list covers the main pipeline (_RUN_ALL.R). The full dependency tree,
with exact versions verified to work together, is captured in renv.lock --
see "Reproducible environment (renv)" below.

System requirements:
  16 GB RAM recommended (required for the full spatial comparison script,
  Energy system and transmission analysis/2050_domestic_CPA_comparison.R)

Setup Instructions

1. Download both Figshare items (doi:10.26188/29604590) -- BESP_data_qld_2025 and
   results -- unzip each, and place them directly in the repository root, alongside
   _RUN_ALL.R (check for an extra wrapper folder after unzipping -- see "What
   Figshare Delivers" above). The GDB archives (QLD_v202412_eplus_tx1.gdb.zip and
   tx2.gdb.zip) auto-extract on first run; no manual unzipping needed for those.
   results/ ships pre-populated with every figure and table already generated.

2. Open Biodiversity_and_energy_system_planning_2024.Rproj in RStudio.
   This sets the working directory automatically via the here package.

3. Run the full pipeline:
     source("_RUN_ALL.R")

   Or run individual scripts:
     source(here::here("Figure_code", "Critically_endangered_mean_coverage_and_line_plot.R"))
     source(here::here("Figure_code", "Zonation curves.R"))
     source(here::here("Figure_code", "percent cost increase_line plot.R"))
     source(here::here("Figure_code", "NPV_bar_plot.R"))


Reproducing this Analysis
--------------------------

Quick start
1. Download both Figshare items (doi:10.26188/29604590) -- BESP_data_qld_2025 and
   results -- unzip each, and place them directly in the repository root, alongside
   _RUN_ALL.R (check for a wrapper folder after unzipping -- see "What Figshare
   Delivers" above).
2. Open Biodiversity_and_energy_system_planning_2024.Rproj in RStudio -- this sets
   the working directory automatically via the here package.
3. Run the full pipeline:
     source("_RUN_ALL.R")
4. Expected runtime: as downloaded, results/ already contains every output, so this
   run mostly skips recomputation and finishes in well under a minute -- each
   script's overwrite_mode guard sees the existing file and prints "already exists"
   rather than recalculating. If you delete or empty results/ to force a full
   recompute, expect approximately 2 minutes on a modern laptop (verified
   2026-08-03 on R 4.4.2 / Windows 10 x64).

Reproducible environment (renv)
renv.lock records the exact version of R and every package used anywhere in
this repository, generated by renv::snapshot() against the environment this
pipeline was verified in (R 4.4.2). To restore that exact environment:
  install.packages("renv")   # if not already installed
  renv::restore()            # run from the repository root; installs pinned versions
This is optional -- _RUN_ALL.R auto-installs whatever it needs via pacman
regardless, so renv::restore() is only necessary if you specifically want the
verified package versions rather than whatever is current on CRAN.

Platform requirements by step
  All _RUN_ALL.R steps (main figures and tables)
    R >= 4.4, Windows 10 x64

  Figure 1a -- biodiversity priority map
    ArcGIS Pro; no script provided; source raster is rankmap.tif

  Figure 3 -- transmission pipeline
    R + existing TX shapefiles (included in Figshare data)

  Re-running Zonation from scratch
    Zonation 5 (Windows)

  Regenerating GDB model outputs from scratch
    netzero_navigate external codebase (not distributed)

Long install paths (Windows): 2050_domestic_CPA_comparison.R reads shapefiles nested
several folders deep under BESP_data_qld_2025/. If your repository clone sits at a long
path (this happens most often inside a deeply-nested OneDrive folder), the combined path
can exceed Windows' classic 260-character MAX_PATH limit -- R's file.exists() then
silently returns FALSE for a file that actually exists. If Supp. Table 2 reports files
missing that you can see in Explorer, clone or extract the repository closer to a drive
root (e.g. C:\BESP\) rather than several folders deep.

Figure and table reference

  Table 1 (all MNES)
    Script:  Biodiversity_analysis/Mean_spp_scenario_coverage.R
    Input:   feature_curves.csv
    Tool:    R
    Note:    "Species with >=99% coverage" counts a species as covered if its
             modelled distribution reaches at least 99% (not exactly 100%) at
             the scenario's threshold rank -- same definition used in the
             manuscript body text and abstract.

  Table 1 (CE/EN) + line plot
    Script:  Figure_code/Critically_endangered_mean_coverage_and_line_plot.R
    Input:   feature_curves.csv
    Tool:    R

  Figure 1a (biodiversity priority map)
    Script:  none (produced in ArcGIS Pro)
    Input:   rankmap.tif
    Tool:    ArcGIS Pro

  Figure 1b-e (VRE siting maps)
    Script:  Energy system and transmission analysis/domestic_export_map_iterations.R
    Input:   tx1.gdb, tx2.gdb
    Tool:    R

  Figure 2 (NPV)
    Script:  Figure_code/NPV_bar_plot.R
    Input:   eplus_Domestic_NPV_figure.csv
    Tool:    R

  Figure 3 (transmission length)
    Script:  Transmission pipeline -- 4 scripts (see Transmission Pipeline Scripts above)
    Input:   tx1.gdb, tx2.gdb, existing TX shapefiles
    Tool:    R

  Figure 4 (cost increase)
    Script:  Figure_code/percent cost increase_line plot.R
    Input:   cost_increase_results.csv
    Tool:    R

  Supplementary Table 2 (TX1 vs TX2 spatial comparison)
    Script:  Energy system and transmission analysis/2050_domestic_CPA_comparison.R
    Input:   Tx_outputs/domestic_tx1_shapefiles/, domestic_tx2_shapefiles/
    Tool:    R
    Note:    Overall spatial overlap (Jaccard index), technology-specific
             overlap (wind/solar_pv/offshore), and wind-solar co-occurrence
             between the TX1 and TX2 scenarios.

  Supplementary Fig 1D (Zonation performance curves)
    Script:  Figure_code/Zonation curves.R
    Input:   feature_curves.csv
    Tool:    R

  Supplementary Fig 2 (zero coverage species map)
    Script:  Biodiversity_analysis/zero_coverage_species.R
    Input:   species shapefiles
    Tool:    R

  Supplementary Fig 4 (exclusion area barplot)
    Script:  Biodiversity_analysis/land_use_competition_QLD.R
             Figure_code/exclusion_overlap_barplot.R
    Input:   BV_exclusion_area_overlap.csv
    Tool:    R

The overwrite_mode flag
Each script sets overwrite_mode <- FALSE near the top. With this default, if an
output file already exists in results/, the script prints a message and skips
recomputation -- this is what makes the default run take approximately 2 minutes.
Set overwrite_mode <- TRUE to force recalculation and overwrite existing outputs.
Two scripts -- Mean_spp_scenario_coverage.R and NPV_bar_plot.R -- always recompute
regardless of this flag.


Re-running the Zonation Analysis
---------------------------------
The Zonation output files included in the Figshare data were produced using Zonation 5.
To re-run the analysis from scratch:

1. Download and install Zonation 5: https://zonationteam.github.io/Zonation5/

2. Unzip BESP_data_qld_2025/Zonation_analysis/Zonation_QLD_biodiversity_feature_rasters.zip
   yourself -- no script does this. After extracting, the biodiversity feature
   rasters should be at:
     BESP_data_qld_2025/Zonation_analysis/Zonation_QLD_biodiversity_feature_rasters/QLD_250m_500spp/

3. Update file paths in the run files located at:
     BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/
   Replace "User_directory" in features_example1.txt and minimal_settings.z5 with the
   full path to your BESP_data_qld_2025/Zonation_analysis/ folder.

4. Run the analysis by executing z5_example1.cmd. If paths are correct this will
   overwrite the outputs in the Zonation_output folder.


Technical Notes
---------------

Coordinate Reference System
All spatial data uses GDA2020 / MGA Zone 56 (EPSG:7856).

overwrite_mode flag
Each script includes an overwrite_mode setting at the top:
  overwrite_mode <- FALSE (default): If the output file already exists in results/,
    the script skips recalculation and displays the existing result. This saves
    processing time (spatial scripts can take 60+ minutes).
  overwrite_mode <- TRUE: Forces all calculations to re-run and overwrites outputs.

Package Management
Scripts use the pacman package manager. Missing packages are installed automatically
on first run.

Session Information
Running _RUN_ALL.R prints a full session info block at the end of each run,
recording the R version, OS, and all loaded package versions for reproducibility.

File Size Information
  BESP_data_qld_2025/ on disk (as extracted):  ~11.2 GB
  results/ on disk (pre-populated deposit):    ~12 MB
  QLD_v202412_eplus_tx1.gdb.zip:                1.10 GB (extracts to ~1.1 GB)
  QLD_v202412_eplus_tx2.gdb.zip:                2.85 GB (extracts to ~0.96 GB)
  Zonation_QLD_biodiversity_feature_rasters.zip: 81 MB (extracts to ~730 MB --
                                                 manual unzip only)
  feature_curves.csv:                           17.7 MB
  Zonation output TIF files:                    45-66 MB each


Version History
---------------
v1.7 (Aug 2026): Added renv.lock, generated via renv::snapshot() against the
  verified R 4.4.2 environment -- records exact versions of every package
  used anywhere in the repository. Fixed 2050_domestic_CPA_comparison.R's
  create_summary_report(), which printed the absolute local output_path
  (including the author's username) into analysis_summary.txt, a file that
  ships in the public deposit -- now prints a fixed repo-relative string.
  Swept both deposit folders for personal paths/usernames beyond the
  originally-flagged stats.txt: found 1,696 affected files, all inside
  BESP_data_qld_2025/. 1,690 are ArcGIS .shp.xml/.tif.xml metadata sidecars
  (not read by any script) -- left untouched per instruction. The remaining
  6 (stats.txt, analysis.log, features_info.csv, species_files_weights.csv,
  species_files_weights-6600L-219128-W.csv, species_weights.csv) had the
  personal path scrubbed to the User_directory placeholder already used
  elsewhere in the deposit; species_weights.csv is an active pipeline input
  and was verified to still parse correctly and produce byte-identical
  output after scrubbing. Also fixed two stale package prerequisites
  (forcats, cowplot -- listed but never actually used) discovered while
  cross-checking the lockfile.

v1.6 (Aug 2026): Recovered a working copy of 2050_domestic_CPA_comparison.R
  (Supplementary Table 2 -- TX1 vs TX2 spatial overlap, technology-specific
  overlap, and wind-solar co-occurrence analysis) from
  Z:/NetZero_scenarios_outputs/Code/2050_domestic_CPA_comparison_revised.R, a
  pre-repo working copy; the version committed to this repo in Jan 2026 was an
  incomplete placeholder stub (see v1.5). Moved it to Energy system and
  transmission analysis/, adapted it to read via _paths.R (paths$tx_outputs)
  and write to results/transmission_scenario_comparison/ instead of hardcoded
  Z: paths, disabled sf's s2 geometry engine to fix a degenerate-vertex crash
  in st_union() (same fix already used in NZAU2_QLD_mapping.R), and added a
  defensive check so a no-data case reports a warning instead of crashing.
  Wired into _RUN_ALL.R as a Step 2 pipeline entry. Verified by actually
  running it end-to-end (R 4.4.2): output matches the original Oct 2025 run to
  within floating-point rounding (e.g. TX1/TX2 overlap area at threshold 0%:
  13729.10 km2 then vs 13729.11 km2 now).

v1.5 (Aug 2026): Moved 2050_domestic_CPA_comparison.R from Figure_code/ to
  Energy system and transmission analysis/ (produces Supplementary Table 2).
  STATUS UNRESOLVED: the script's body is still a placeholder stub with no
  working logic -- git history confirms it has never contained more than this.
  It cannot currently reproduce Supplementary Table 2; see open items. Not
  listed as a runnable step until this is fixed. Redirected
  Biodiversity_value_map.R's classified-raster cache (reclass_path) from
  BESP_data_qld_2025/ to results/zonation_figures/ -- the pipeline must never
  write into the data folder, since a clean-room junction would hit the real
  deposit and a downloading user would have data they were told is read-only
  silently modified.

v1.4 (Aug 2026): Final pre-upload audit. Deposit is now exactly two Figshare
  downloads (BESP_data_qld_2025, results), both placed at the repository root;
  code stays in GitHub only. Removed the pre-extracted
  Zonation_QLD_biodiversity_feature_rasters/ copy from the deposit (kept the
  .zip; unzip manually before a from-scratch Zonation run -- no script does this
  automatically). Wired Biodiversity_value_map.R into _RUN_ALL.R as an
  R-generated rendition of Figure 1a. Fixed a stale species-shapefile path
  reference (retrieve_spp_details.R) and a folder-name error (species_code/ ->
  Biodiversity_analysis/). Fixed Figure 3 script references throughout this
  document (Figure_code/tx_length_figure.R is canonical, not
  transmission_length_tx1_tx2.R). Documented that results/ ships pre-populated.

v1.3 (Aug 2026): Reproducibility fixes -- corrected script path references in
  _paths.R, land_use_competition_QLD.R, zero_coverage_species.R, tx_run_all.R,
  and domestic_export_map_iterations.R; fixed minimal_settings.z5 Zonation path;
  renamed two misspelled scripts; added wind/PV exclusion rasters to deposit;
  added Reproducing this Analysis section.

v1.2 (Mar 2026): Updated data folder structure (BESP_data_qld_2025/); updated Zonation
  output path to 250m_QLD_2024; restructured _RUN_ALL.R into Step 1 (paper figures)
  and Step 2 (maps/barplots) with independent run flags; added session info output;
  added exclusion_overlap_barplot to pipeline; documented species_code/ scripts, LCOE
  pre-computed outputs, and manuscript figure reference placeholder in README.

v1.1 (Jan 2026): Updated to full R Project structure; implemented here for relative
  pathing; added automated unzipping logic and LLM-assisted code optimisation.

v1.0 (2025): Initial release for peer review.


Author: Andrew Rogers
LLMs used: Claude AI and Gemini
Last Updated: August 2026
