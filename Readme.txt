Supplementary Materials: Biodiversity and Energy System Planning - Queensland 2025

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


Repository Structure
--------------------
The project is organised as an R Project. Open the .Rproj file to automatically set
the correct working directory — no need to manually set paths.

Biodiversity_and_energy_system_planning_2024/
├── Biodiversity_and_energy_system_planning_2024.Rproj  # <-- Open this first
├── _RUN_ALL.R                          # Master runner — executes full pipeline
├── _paths.R                            # Central path management (sourced by scripts)
├── BESP_data_qld_2025/                 # Place unzipped Figshare data here
│   ├── Energy_system_model_outputs/    # Processed CSV/XLSX files
│   ├── Zonation_analysis/              # Zonation run files, weights, and outputs
│   └── [.gdb files from Figshare]      # Large spatial databases (unzip before use)
├── Figure_code/                        # R analysis and figure scripts
├── results/                            # Created automatically by scripts
│   ├── figures/                        # PNG outputs
│   ├── tables/                         # Summary CSV outputs
│   ├── zonation_figures/               # Zonation performance curve outputs
│   └── transmission_scenario_comparison/  # Spatial overlap statistics
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
- Supplementary table 9. EPBC listed species...: IUCN and national threat classifications

Spatial Exclusions
- Suplementary table_other spatial exclusions.xlsx: Non-biodiversity spatial constraints


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


Transmission Pipeline Scripts (Energy system and transmission mapping_code/transmission_mapping/)
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

  Step 4 — transmission_length_tx1_tx2.R
    Purpose:  Produces Figure 3 — total new TX length (km) vs biodiversity avoidance
              scenario for TX1 and TX2, with existing network reference line.
    Input:    BESP_data_qld_2025/Energy_system_model_outputs/
                   tx1_new_transmission_summary.csv
                   tx2_new_transmission_summary.csv
    Output:   results/figures/transmission_length_tx1_tx2.png
              results/tables/tx_length_summary.csv

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

  QLD_exisitng_model_tx_max_join.R
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

  QLD_summarize_exisiting_in_simiplified_tx.R
    Purpose:  Utility fragment. Summarises existing TX attributes into a
              simplified corridor network. Requires `simplified_tx` to be
              loaded before running (see script header).

Spatial Reference Data
  The transmission pipeline scripts require the following shapefiles to be placed in:
    BESP_data_qld_2025/Spatial_reference_data/
  - Electricity_Transmission_Lines.shp
  - Electricity_Transmission_Lines_1km_buff.shp
  - QLD_existing_tx_simplified_buff_no_overlap2.shp
  Source: QLD electricity transmission network (state government spatial data).
  Configure paths via paths$existing_tx, paths$existing_tx_buff, paths$existing_tx_simp
  in _paths.R.

Note on duplicate scripts: The root-level Energy system and transmission mapping_code/
folder contains older versions of these scripts. The canonical current versions are in
the transmission_mapping/ subfolder.


Supporting Scripts (species_code/)
-----------------------------------
These scripts were used in data preparation and supporting analyses upstream of the
main pipeline. They are not included in _RUN_ALL.R and require access to raw species
distribution shapefiles not included in the Figshare data due to file size.

retrieve_spp_details.R
  Purpose:  Extracts species attributes (scientific name, common name, EPBC threat
            status) from individual species distribution shapefiles and compiles
            them into a single lookup CSV. Used to build the species metadata table.
  Input:    data/QLD_100m_SNES_500spp/shapefiles/ (individual .shp files per species)
  Output:   data/QLD_100m_SNES_500spp/species_attributes.csv

zero_coverage_species.R
  Purpose:  Identifies species with zero coverage in the Zonation output and maps
            their distributions with CAPAD protected area boundaries overlaid.
            Used to investigate which species receive no representation at any
            priority threshold.
  Input:    data/QLD_100m_SNES_500spp/shapefiles/ (selected species .shp files)
            data/CAPAD_QLD.shp
  Output:   results/zero_coverage/species_distribution_map.png
            results/zero_coverage/map_number_lookup.csv

RZ_area_outside_exclusions_and_ECNES.R
  Purpose:  Calculates the area of biodiversity red zones remaining outside
            renewable energy exclusion areas (PV and wind separately) using spatial
            difference operations. Supports optional parallel processing.
  Input:    data/AUS_area_outside_exclusion_zones.gdb
            data/Red_zones_QLD.shp
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
  domestic_export_map_iterations.R                     → Figure 1b-e (VRE siting maps)
  NPV_bar_plot.R                                       → Figure 2 (NPV bar chart)
  Transmission_processing.R  }
  Transmission_save_layers_as_shapefiles.R  }          → Figure 3 (transmission length)
  QLD_new_tx_processing_summary.R  }                      (pipeline steps 1-3)
  transmission_length_tx1_tx2.R  }
  percent cost increase_line plot.R                    → Figure 4 (cost increase)
  Critically_endangered_mean_coverage_and_line_plot.R  → Table 1 / line plot
  Zonation curves.R                                    → Supplementary Fig 1D
  zero_coverage_species.R                              → Supplementary Fig 2
  exclusion_overlap_barplot.R                          → Supplementary Fig 6
  LCOE_BV_exclusion_summary.R                          → Supplementary (LCOE maps)


Getting Started
---------------

Prerequisites — R packages (automatically installed on first run via pacman):
  sf, dplyr, ggplot2, readr, readxl, tidyr, furrr, data.table, progress,
  ozmaps, purrr, forcats, cowplot, gridExtra, here, scales, magick

System requirements:
  16 GB RAM recommended (required for the full spatial comparison script)

Setup Instructions

1. Download the Figshare data and unzip its contents into the BESP_data_qld_2025/
   folder inside the repository root. The folder structure should match the tree above.

2. Unzip the GDB files before running the spatial mapping script:
     BESP_data_qld_2025/QLD_v202412_eplus_tx1.gdb.zip
     BESP_data_qld_2025/QLD_v202412_eplus_tx2.gdb.zip

3. Open Biodiversity_and_energy_system_planning_2024.Rproj in RStudio.
   This sets the working directory automatically via the here package.

4. Run the full pipeline:
     source("_RUN_ALL.R")

   Or run individual scripts from Figure_code/:
     source(here::here("Figure_code", "Critically_endangered_mean_coverage_and_line_plot.R"))
     source(here::here("Figure_code", "Zonation curves.R"))
     source(here::here("Figure_code", "percent cost increase_line plot.R"))
     source(here::here("Figure_code", "NPV_bar_plot.R"))
     source(here::here("Figure_code", "2050_domestic_CPA_comparison.R"))


Re-running the Zonation Analysis
---------------------------------
The Zonation output files included in the Figshare data were produced using Zonation 5.
To re-run the analysis from scratch:

1. Download and install Zonation 5: https://zonationteam.github.io/Zonation5/

2. Unzip the feature rasters:
     BESP_data_qld_2025/Zonation_analysis/Zonation_QLD_biodiversity_feature_rasters.zip

3. Update file paths in the run files located at:
     BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/
   Replace "User_directory" in features_example1.txt and minimal_settings.z5 with the
   full path to your BESP_data_qld_2025/Zonation_analysis/ folder.

4. Run the analysis by executing z5_example1.cmd. If paths are correct and feature
   rasters are unzipped, this will overwrite the outputs in the Zonation_output folder.


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
  Total Figshare data:            ~7.8 GB
  QLD_v202412_eplus_tx1.gdb.zip:   1.0 GB
  QLD_v202412_eplus_tx2.gdb.zip:   2.7 GB
  feature_curves.csv:              17.7 MB
  Zonation output TIF files:       45-66 MB each


Version History
---------------
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
Last Updated: March 2026
