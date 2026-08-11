Supplementary Materials: Biodiversity and Energy System Planning - Queensland 2025
> See also: `Readme.txt` in this folder for the plain-text version of these instructions.

Overview
This repository contains analysis scripts and project configurations for the study "Biodiversity and Energy System Planning in Queensland: Balancing Conservation and Infrastructure Development".

Note on Data: Due to size constraints, the raw spatial data (approx. 7.8 GB) is hosted separately on Figshare: https://doi.org/10.26188/29604590. This repository provides the code to process that data into the final results and figures.

#Data
Data used for this study can be found through figshare here:
https://figshare.unimelb.edu.au/articles/dataset/Supplementary_Data_Biodiversity_and_Energy_System_Planning_-_Queensland_2025/29604590

Recommended citation: Rogers, Andrew (2025). Supplementary Data: Biodiversity and Energy System Planning - Queensland 2025. The University of Melbourne. Dataset. https://doi.org/10.26188/29604590.v1

## What Figshare Delivers

The Figshare deposit is **two separate downloads**. Both are placed inside the cloned
GitHub repository, at the same level as `_RUN_ALL.R`:

| Download | Placement | Contents |
|---|---|---|
| `BESP_data_qld_2025` | `<repo>/BESP_data_qld_2025/` (same relative location as in this repo) | Raw input data: `Energy_system_model_outputs/`, `Zonation_analysis/` |
| `results` | `<repo>/results/` (repo root, alongside `BESP_data_qld_2025/`) | **Ships pre-populated** with every figure and table already generated. `source("_RUN_ALL.R")` will detect these and redraw them (~2 min) instead of recomputing from scratch. Delete or empty `results/` to force a full recompute (60+ minutes for the spatial steps). |

**Wrapper directories:** depending on how each item downloads and unzips, you may end
up with an extra nested folder (e.g. `BESP_data_qld_2025/BESP_data_qld_2025/...` or a
folder named after the archive). If so, move the *inner* folder up one level so that
`BESP_data_qld_2025/` and `results/` each sit directly in the repository root — verify
against the tree below before running anything.

The code folders (`Biodiversity_analysis/`, `Figure_code/`, `Energy system and
transmission analysis/`) are **not** part of the Figshare deposit — they come from the
GitHub repository itself.

## Repository Structure

Biodiversity_and_energy_system_planning_2024/
├── Biodiversity_and_energy_system_planning_2024.Rproj  # <-- Start here
├── _RUN_ALL.R                             # Master runner
├── _paths.R                               # Central path management
├── BESP_data_qld_2025/                    # <-- Figshare download 1 goes here
│   ├── Energy_system_model_outputs/       # Processed CSV/XLSX files, GDBs
│   └── Zonation_analysis/                 # Zonation run files and curves
├── Figure_code/                           # Figure scripts
├── Biodiversity_analysis/                 # Biodiversity analysis scripts
├── Energy system and transmission analysis/ # Transmission pipeline scripts
├── results/                               # <-- Figshare download 2 goes here (pre-populated)
│   ├── figures/                           # PNG/PDF outputs
│   ├── tables/                            # Summary CSV outputs
│   ├── zonation_figures/                  # Zonation curve/rankmap outputs
│   ├── zero_coverage/                     # Supp. Fig 2 outputs
│   ├── transmission_processing/           # TX pipeline intermediates
│   └── transmission_scenario_comparison/  # Supp. Table 2 outputs
└── README.md

## Data Files Description

### Energy System Data
- **QLD_v202412_eplus_tx1.gdb**: Energy infrastructure scenarios under transmission option 1 (ESRI File Geodatabase folder; `.gdb.zip` archive also included)
- **QLD_v202412_eplus_tx2.gdb**: Energy infrastructure scenarios under transmission option 2 (ESRI File Geodatabase folder; `.gdb.zip` archive also included)
- **cost_increase_results.csv**: Cost increases under different biodiversity protection levels
- **eplus_Domestic_NPV_figure.csv**: Net present value data used directly by `NPV_bar_plot.R`
- **eplus_Domestic_NPV_2025.xlsx**: Full NPV dataset (source for the figure CSV above)

### Conservation Data
- **feature_curves.csv**: Zonation performance curves for 545 conservation features
- **Species_files_weights_table.xlsx**: Weighting schemes for species in conservation planning
- **Species threat status tables**: IUCN and national threat classifications

### Spatial Exclusions
- **Suplementary table_other spatial exclusions.xlsx** (sic — matches the actual filename in the deposit): Non-biodiversity spatial constraints

## Zip Archives in the Deposit

Every `.zip` file in `BESP_data_qld_2025/` and how it gets extracted:

| Archive | Location | Extraction |
|---|---|---|
| `QLD_v202412_eplus_tx1.gdb.zip` | `Energy_system_model_outputs/Energy_system_analysis_scenarios/` | **Automatic.** `Energy system and transmission analysis/domestic_export_map_iterations.R` extracts it on first run if `QLD_v202412_eplus_tx1.gdb/` is not already present. No manual action needed. |
| `QLD_v202412_eplus_tx2.gdb.zip` | same folder | **Automatic**, same script, same mechanism. |
| `Zonation_QLD_biodiversity_feature_rasters.zip` | `Zonation_analysis/` | **Manual.** No script extracts this — it is only needed if you re-run Zonation from scratch (see "Re-running the Zonation analysis" below). Unzip it yourself before attempting a Zonation re-run; the R pipeline (`_RUN_ALL.R`) never reads it. |

Expected disk space after extraction (zips are kept after extracting, not deleted —
extracted size is *in addition to* the zip):

| Archive | Zipped | Extracted |
|---|---|---|
| `QLD_v202412_eplus_tx1.gdb.zip` | 1.10 GB | ~1.1 GB |
| `QLD_v202412_eplus_tx2.gdb.zip` | 2.85 GB | ~0.96 GB |
| `Zonation_QLD_biodiversity_feature_rasters.zip` (only if manually unzipped) | 81 MB | ~730 MB |

Allow roughly an extra 2 GB free for the two GDBs once auto-extracted, plus ~730 MB
more if you unzip the Zonation rasters for a from-scratch Zonation run.

### Re-running the Zonation analysis
1. Download and install Zonation 5: https://zonationteam.github.io/Zonation5/
2. Update `features_example1.txt` and `minimal_settings.z5` in `BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/`: replace `User_directory` with the full path to your `BESP_data_qld_2025/Zonation_analysis/` folder.
3. Unzip `BESP_data_qld_2025/Zonation_analysis/Zonation_QLD_biodiversity_feature_rasters.zip` yourself (no script does this). After extracting, the biodiversity feature rasters should be at `BESP_data_qld_2025/Zonation_analysis/Zonation_QLD_biodiversity_feature_rasters/QLD_250m_500spp/`.
4. Run `z5_example1.cmd`. If paths are correct this will overwrite the outputs in the `Zonation_output/` folder.


## Analysis Scripts

### 1. Species Coverage — All MNES (`Biodiversity_analysis/Mean_spp_scenario_coverage.R`)
**Purpose**: Calculates mean (+ 95% CI), min, and max distribution coverage across **all** MNES (species and ecological communities in `feature_curves.csv`) at each biodiversity protection scenario threshold. Produces the primary data for Table 1 in the main manuscript.

**Input**: `BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_curves.csv`
**Output**: `results/tables/scenario_coverage_results.csv`

**Scenarios**: BAU (14% of Qld), Top 30%, Top 50%, Top 70%, Top 90%

**Note**: The "Species with >=99% coverage" column counts a species as covered
if its modelled distribution reaches at least 99% (not exactly 100%) at the
scenario's threshold rank. This is the same definition used for the
manuscript body text and abstract.

**Usage**:
```r
source("Biodiversity_analysis/Mean_spp_scenario_coverage.R")
```

### 2. Cost Analysis (`percent cost increase_line plot.R`)
**Purpose**: Analyzes and visualizes energy cost increases under different High Biodiversity Value Area (HBVA) exclusion scenarios.

**Input**: `Energy_system_model_outputs/cost_increase_results.csv`
**Output**: `figures/energy_cost_increase_plot.png`

**Usage**:
```r
# Ensure you're in the supplementary data folder
source("percent cost increase_line plot.R")
```

### 2. Conservation Priority Analysis (`Zonation curves.R`)
**Purpose**: Creates performance curves for conservation features and analyzes coverage at different priority thresholds.

**Input**: `Zonation_output/250m_QLD_2024/out_example1/feature_curves.csv`
**Outputs**: 
- `figures/zonation_performance_curves.png`
- `Zonation_output/250m_QLD_2024/out_example1/feature_coverage_summary_with_CI.csv`

**Usage**:
```r
source("Zonation curves.R")
```

### 3. Economic Analysis (`Figure_code/NPV_bar_plot.R`)
**Purpose**: Creates Figure 2 — stacked bar plots showing Net Present Value of VRE and transmission infrastructure investments by year (2030/2040/2050) and biodiversity avoidance threshold, faceted by TX scenario.

**Input**: `BESP_data_qld_2025/Energy_system_model_outputs/eplus_Domestic_NPV_figure.csv`
**Output**: `results/figures/npv_analysis_plot.png`

**Note**: `eplus_Domestic_NPV_2025.xlsx` contains the full dataset; `eplus_Domestic_NPV_figure.csv` is the processed version used directly by this script.

**Usage**:
```r
source("Figure_code/NPV_bar_plot.R")
```

### 4. Exclusion Area Analysis — Supp. Fig 6 (`Biodiversity_analysis/land_use_competition_QLD.R` → `Figure_code/exclusion_overlap_barplot.R`)
**Purpose**: Quantifies wind and PV exclusion areas within each Zonation priority band using raster cell counting (no polygonisation). Produces the CSV consumed by `exclusion_overlap_barplot.R` to generate Supp. Fig 6.

**Inputs**:
- Zonation rankmap: `BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/rankmap.tif`
- Wind exclusion raster: `NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters/combined_wind.tif`
- PV exclusion raster: `NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters/combined_pv.tif`

**Output**: `BESP_data_qld_2025/Energy_system_model_outputs/BV_exclusion_area_overlap.csv`

**Bands**: Top 30%, Top 30–50%, Bottom 50–70%, Bottom 70–90% (Zonation 0–1 scale, 250 m cells = 6.25 ha each)

**Usage**:
```r
# Run data script first, then figure script
source("Biodiversity_analysis/land_use_competition_QLD.R")
source("Figure_code/exclusion_overlap_barplot.R")
# Or use _RUN_ALL.R — both steps are included in pipeline_step2
```

### 5. Spatial Mapping — Figure 1b–e (`Energy system and transmission analysis/domestic_export_map_iterations.R`)
**Purpose**: Generates Figure 1b–e — cost-optimised VRE siting maps for BAU and biodiversity protection scenarios (Top 30/50/70%). Processes TX1/TX2 GDB files across thresholds (0/10/30/50/70/90%) and years (2030/2040/2050), producing shapefiles and PNG maps.

**Note**: Figure 1a (biodiversity prioritisation map) was produced in ArcGIS Pro using the Zonation output raster as its source. The source file is `BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/rankmap.tif` (accessible via `paths$rankmap` in `_paths.R`).

**Input**: `BESP_data_qld_2025/Energy_system_model_outputs/Energy_system_analysis_scenarios/QLD_v202412_eplus_tx1.gdb` (and `tx2.gdb`)
**Outputs**:
- `results/figures/energy_maps/shapefiles_tx1/combined_renewables_2050_threshold_N.shp`
- `results/figures/energy_maps/domestic_maps_tx1/domestic_layer_map_[threshold]_[year].png`
- `results/figures/energy_maps/export_maps_tx1/export_layer_map_[threshold]_[year].png`

**Usage**:
```r
source("Energy system and transmission analysis/domestic_export_map_iterations.R")
```

## Getting Started

### Prerequisites
Required R packages (automatically installed by scripts):
- sf, dplyr, ggplot2, readr, readxl, tidyr
- furrr, data.table, progress, ozmaps, purrr
- ggpattern, forcats, cowplot, gridExtra
- here, scales, extrafont (optional)

### Setup Instructions

1. **Download and place both Figshare downloads**: Download both items from the Figshare deposit (doi:10.26188/29604590) — `BESP_data_qld_2025` and `results`. Unzip each and place them directly inside the repository root, alongside `_RUN_ALL.R` (check for an extra wrapper folder after unzipping — see "What Figshare Delivers" above). `results/` ships pre-populated with every figure and table already generated, so you don't need to run anything to see the outputs.

2. **Open the R Project**: Open `Biodiversity_and_energy_system_planning_2024.Rproj` in RStudio. This sets the working directory automatically.

3. **Run Analysis Scripts**:
   ```r
   # Recommended: run the full pipeline via the master runner
   source("_RUN_ALL.R")

   # Or run individual scripts:
   source("Biodiversity_analysis/Mean_spp_scenario_coverage.R")   # Table 1 — all MNES coverage
   source("Figure_code/Critically_endangered_mean_coverage_and_line_plot.R")  # Table 1 CE/EN + plot
   source("Figure_code/Zonation curves.R")
   source("Figure_code/percent cost increase_line plot.R")
   source("Figure_code/NPV_bar_plot.R")
   source("Energy system and transmission analysis/domestic_export_map_iterations.R")  # Figure 1b-e
   ```

### Expected Outputs
All scripts write into the `results/` directory:
- `results/figures/` — PNG outputs (Figures 1b–e, 2, 4, Supp. Fig 1D, 6)
- `results/tables/` — Summary CSV outputs (Table 1 data)
- `results/zonation_figures/` — Zonation performance curves
- `results/figures/energy_maps/` — VRE siting maps (Figure 1b–e)

## Reproducing this Analysis

### Quick start
1. Download both Figshare items (doi:10.26188/29604590) — `BESP_data_qld_2025` and `results` — unzip each, and place them directly in the repository root, alongside `_RUN_ALL.R` (check for a wrapper folder after unzipping — see "What Figshare Delivers" above).
2. Open `Biodiversity_and_energy_system_planning_2024.Rproj` in RStudio — this sets the working directory automatically via the `here` package.
3. Run the full pipeline:
   ```r
   source("_RUN_ALL.R")
   ```
4. Expected runtime: as downloaded, `results/` already contains every output, so this run mostly skips recomputation and finishes in well under a minute — each script's `overwrite_mode` guard sees the existing file and prints "already exists" rather than recalculating. If you delete or empty `results/` to force a full recompute, expect approximately 2 minutes on a modern laptop (verified 2026-08-03 on R 4.4.2 / Windows 10 x64).

### Platform requirements
| Step | Tool required |
|------|---------------|
| All `_RUN_ALL.R` steps | R ≥ 4.4, Windows 10 x64 |
| Figure 1a — biodiversity priority map | ArcGIS Pro; no script provided; source raster is `rankmap.tif` |
| Figure 3 — transmission pipeline | R + existing TX shapefiles (included in Figshare data) |
| Re-running Zonation from scratch | Zonation 5 (Windows) |
| Regenerating GDB model outputs | `netzero_navigate` external codebase (not distributed) |

**Long install paths (Windows):** `2050_domestic_CPA_comparison.R` reads shapefiles nested
several folders deep under `BESP_data_qld_2025/`. If your repository clone sits at a long
path (this happens most often inside a deeply-nested OneDrive folder), the combined path
can exceed Windows' classic 260-character `MAX_PATH` limit — R's `file.exists()` then
silently returns `FALSE` for a file that actually exists. If Supp. Table 2 reports files
missing that you can see in Explorer, clone or extract the repository closer to a drive
root (e.g. `C:\BESP\`) rather than several folders deep.

### Figure and table reference
| Output | Script | Key input | Tool |
|--------|--------|-----------|------|
| Table 1 (all MNES) | `Biodiversity_analysis/Mean_spp_scenario_coverage.R` | `feature_curves.csv` | R |
| Table 1 (CE/EN) + line plot | `Figure_code/Critically_endangered_mean_coverage_and_line_plot.R` | `feature_curves.csv` | R |
| Figure 1a (priority map) | — | `rankmap.tif` | ArcGIS Pro |
| Figure 1b–e (VRE maps) | `Energy system and transmission analysis/domestic_export_map_iterations.R` | `tx1.gdb`, `tx2.gdb` | R |
| Figure 2 (NPV) | `Figure_code/NPV_bar_plot.R` | `eplus_Domestic_NPV_figure.csv` | R |
| Figure 3 (TX length) | Transmission pipeline — 4 scripts | `tx1.gdb`, `tx2.gdb`, TX shapefiles | R |
| Figure 4 (cost increase) | `Figure_code/percent cost increase_line plot.R` | `cost_increase_results.csv` | R |
| Supp. Table 2 (TX1 vs TX2 spatial comparison) | `Energy system and transmission analysis/2050_domestic_CPA_comparison.R` | `Tx_outputs/domestic_tx1_shapefiles/`, `domestic_tx2_shapefiles/` | R |
| Supp. Fig 1D (Zonation curves) | `Figure_code/Zonation curves.R` | `feature_curves.csv` | R |
| Supp. Fig 2 (zero coverage map) | `Biodiversity_analysis/zero_coverage_species.R` | species shapefiles | R |
| Supp. Fig 6 (exclusion barplot) | `Biodiversity_analysis/land_use_competition_QLD.R` → `Figure_code/exclusion_overlap_barplot.R` | `BV_exclusion_area_overlap.csv` | R |

### The `overwrite_mode` flag
Each script sets `overwrite_mode <- FALSE` near the top. With this default, if an output file already exists in `results/`, the script prints a message and skips recomputation — this is what makes the default run take approximately 2 minutes. Set `overwrite_mode <- TRUE` to force recalculation and overwrite existing outputs. Two scripts — `Mean_spp_scenario_coverage.R` and `NPV_bar_plot.R` — always recompute regardless of this flag.

## File Size Information
- **`BESP_data_qld_2025/` on disk (as extracted)**: ~11.2 GB
- **`results/` on disk (pre-populated deposit)**: ~12 MB
- **Large files**:
  - QLD_v202412_eplus_tx1.gdb.zip: 1.10 GB (extracts to ~1.1 GB)
  - QLD_v202412_eplus_tx2.gdb.zip: 2.85 GB (extracts to ~0.96 GB)
  - Zonation_QLD_biodiversity_feature_rasters.zip: 81 MB (extracts to ~730 MB — manual unzip only, see "Zip Archives in the Deposit" above)
  - feature_curves.csv: 17.7 MB
  - Various output TIF files: 45-66 MB each

## Technical Notes

### Coordinate Reference System
All spatial data uses GDA2020 / MGA Zone 56 (EPSG:7856) coordinate reference system.

To save processing time (especially for spatial tasks that can take 60+ minutes), each script includes a "Smart Check" at the top:

overwrite_mode <- FALSE (Default): If the output figure or table already exists in the results/ folder, the script will skip the calculation and simply display the existing file.

overwrite_mode <- TRUE: Force the script to re-run all calculations and overwrite existing files.

📦 Package Management
The scripts use the pacman manager. Missing libraries (e.g., sf, here, ggpattern) will be installed automatically on your first run.

🗺️ Spatial Metadata
CRS: GDA2020 / MGA Zone 56 (EPSG:7856).

Version History
v1.6 (Aug 2026): Recovered a working copy of `2050_domestic_CPA_comparison.R`
  (Supplementary Table 2 — TX1 vs TX2 spatial overlap, technology-specific
  overlap, and wind-solar co-occurrence analysis) from
  `Z:/NetZero_scenarios_outputs/Code/2050_domestic_CPA_comparison_revised.R`,
  a pre-repo working copy; the version committed to this repo in Jan 2026 was
  an incomplete placeholder stub (see v1.5). Moved it to `Energy system and
  transmission analysis/`, adapted it to read via `_paths.R`
  (`paths$tx_outputs`) and write to `results/transmission_scenario_comparison/`
  instead of hardcoded `Z:` paths, disabled `sf`'s s2 geometry engine to fix a
  degenerate-vertex crash in `st_union()` (same fix already used in
  `NZAU2_QLD_mapping.R`), and added a defensive check so a no-data case
  reports a warning instead of crashing. Wired into `_RUN_ALL.R` as a
  Step 2 pipeline entry. Verified by actually running it end-to-end (R 4.4.2):
  output matches the original Oct 2025 run to within floating-point rounding
  (e.g. TX1/TX2 overlap area at threshold 0%: 13729.10 km² then vs 13729.11 km²
  now).

v1.5 (Aug 2026): Moved `2050_domestic_CPA_comparison.R` from `Figure_code/` to
  `Energy system and transmission analysis/` (produces Supplementary Table 2).
  **STATUS UNRESOLVED:** the script's body is still a placeholder stub with no
  working logic — git history confirms it has never contained more than this.
  It cannot currently reproduce Supplementary Table 2; see open items. Not
  listed as a runnable step until this is fixed. Redirected
  `Biodiversity_value_map.R`'s classified-raster cache (`reclass_path`) from
  `BESP_data_qld_2025/` to `results/zonation_figures/` — the pipeline must never
  write into the data folder, since a clean-room junction would hit the real
  deposit and a downloading user would have data they were told is read-only
  silently modified.

v1.4 (Aug 2026): Final pre-upload audit. Deposit is now exactly two Figshare downloads
  (`BESP_data_qld_2025`, `results`), both placed at the repository root; code stays in
  GitHub only. Removed the pre-extracted `Zonation_QLD_biodiversity_feature_rasters/`
  copy from the deposit (kept the `.zip`; unzip manually before a from-scratch
  Zonation run — no script does this automatically). Wired `Biodiversity_value_map.R`
  into `_RUN_ALL.R` as an R-generated rendition of Figure 1a. Fixed a stale path
  reference in `retrieve_spp_details.R` and resolved a filename/output-path collision
  between two Figure 3 scripts. Documented that `results/` ships pre-populated.

v1.3 (Aug 2026): Reproducibility fixes — corrected script path references in `_paths.R`, `land_use_competition_QLD.R`, `zero_coverage_species.R`, `tx_run_all.R`, and `domestic_export_map_iterations.R`; fixed `minimal_settings.z5` Zonation path; renamed two misspelled scripts; added wind/PV exclusion rasters to deposit; added "Reproducing this Analysis" section.

v1.2 (May 2026): Replaced `RZ_area_outside_exclusions_and_ECNES.R` with `land_use_competition_QLD.R` as the data source for Supp. Fig 6. New script uses raster cell counting (terra) instead of vector intersection (sf) for faster, reproducible exclusion area summaries. Pipeline updated in `_RUN_ALL.R`.

v1.1 (Jan 2026): Updated to full R Project structure; implemented here for relative pathing; added automated unzipping logic and LLM-assisted code optimization.

v1.0 (2025): Initial release for peer review.

Author: Andrew Rogers

LLMs used: Claude AI and Gemini

Last Updated: August 2026

