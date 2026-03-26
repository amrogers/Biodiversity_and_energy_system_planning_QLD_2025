Supplementary Materials: Biodiversity and Energy System Planning - Queensland 2025
Overview
This repository contains analysis scripts and project configurations for the study "Biodiversity and Energy System Planning in Queensland: Balancing Conservation and Infrastructure Development".

Note on Data: Due to size constraints, the raw spatial data (approx. 7.8 GB) is hosted separately on Figshare: https://doi.org/10.26188/29604590. This repository provides the code to process that data into the final results and figures.

Repository Structure
The project is organized as an R Project. Opening the .Rproj file automatically sets the correct working directory.scripts for the study "Biodiversity and Energy System Planning in Queensland: Balancing Conservation and Infrastructure Development". The materials include energy system modeling results, conservation priority analyses, and spatial mapping tools.

#Data
Data used for this study can be found through figshare here:
https://figshare.unimelb.edu.au/articles/dataset/Supplementary_Data_Biodiversity_and_Energy_System_Planning_-_Queensland_2025/29604590

reccomended citation: Rogers, Andrew (2025). Supplementary Data: Biodiversity and Energy System Planning - Queensland 2025. The University of Melbourne. Dataset. https://doi.org/10.26188/29604590.v1

## Repository Structure

Biodiversity_and_energy_system_planning_2024/
├── Biodiversity_and_energy_system_planning_2024.Rproj  # <-- Start here
├── data/                                  # Place Figshare ZIP contents here
│   ├── Energy_system_model_outputs/       # Processed CSV/XLSX files
│   ├── Zonation_analysis/                 # Zonation run files and curves
│   └── [.gdb.zip files from Figshare]     # Large spatial databases
├── scripts/                               # R Analysis scripts
├── results/                               # Created automatically by scripts
│   ├── figures/                           # PNG/PDF outputs
│   ├── tables/                            # Summary CSV outputs
│   └── transmission_scenario_comparison/  # Spatial overlap stats
└── README.md

## Data Files Description

### Energy System Data
- **QLD_v202412_eplus_tx1.gdb.zip**: Energy infrastructure scenarios under transmission option 1
- **QLD_v202412_eplus_tx2.gdb.zip**: Energy infrastructure scenarios under transmission option 2
- **cost_increase_results.csv**: Cost increases under different biodiversity protection levels
- **eplus_Domestic_NPV_2025.xlsx**: Net present value analysis for domestic energy projects

### Conservation Data
- **feature_curves.csv**: Zonation performance curves for 524+ conservation features
- **Species_files_weights_table.xlsx**: Weighting schemes for species in conservation planning
- **Species threat status tables**: IUCN and national threat classifications

### Spatial Exclusions
- **Supplementary table_other spatial exclusions.xlsx**: Non-biodiversity spatial constraints

###Zonation analysis
### 1. Download and install Zonation 5: https://zonationteam.github.io/Zonation5/
### 2. Update the features_example1.txt and minimal_settings.Z5 files. 
	- Change the file paths: replace User_directory in the file paths to the download location. this should be in the "download_location"\Biodiversity_and_energy_system_planning_2024\data
### 3. Unzip the Zonation_QLD_biodiversity_feature_rasters.zip file in the Zonation analysis folder
### 4. Run the z5_example1 file. if the files paths are correct and the feature rasters are unzipped this will run the zonation analysis and overwrite the outputs in the zonation output folder. 


## Analysis Scripts

### 1. Species Coverage — All MNES (`species_code/Mean_spp_scenario_coverage.R`)
**Purpose**: Calculates mean (+ 95% CI), min, and max distribution coverage across **all** MNES (species and ecological communities in `feature_curves.csv`) at each biodiversity protection scenario threshold. Produces the primary data for Table 1 in the main manuscript.

**Input**: `BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_curves.csv`
**Output**: `results/tables/scenario_coverage_results.csv`

**Scenarios**: BAU (14% of Qld), Top 30%, Top 50%, Top 70%, Top 90%

**Usage**:
```r
source("species_code/Mean_spp_scenario_coverage.R")
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

**Input**: `Zonation_output/250m_SNES_ECNES_red_zones_weighted_QLD/out_example1/feature_curves.csv`
**Outputs**: 
- `figures/zonation_performance_curves.png`
- `Zonation_output/250m_SNES_ECNES_red_zones_weighted_QLD/out_example1/feature_coverage_summary_with_CI.csv`

**Usage**:
```r
source("Zonation curves.R")
```

### 3. Economic Analysis (`NPV_bar_plot.R`)
**Purpose**: Creates bar plots showing Net Present Value of energy investments with build components.

**Input**: `Energy_system_model_outputs/eplus_Domestic_NPV_2025.xlsx`
**Output**: `figures/npv_analysis_plot.png`

**Usage**:
```r
source("NPV_bar_plot.R")
```

### 4. Spatial Mapping (`domestic_export_map_iterations.R`)
**Purpose**: Creates detailed spatial maps of renewable energy infrastructure for domestic and export scenarios.

**Input**: Extracted GDB files from `Energy_system_analysis_scenarios/`
**Outputs**: 
- `figures/energy_maps/domestic_maps_tx1/domestic_layer_map_[threshold]_[year].png`
- `figures/energy_maps/export_maps_tx1/export_layer_map_[threshold]_[year].png`
- Processed shapefiles and summary statistics

**Usage**:
```r
# First extract GDB files from ZIP archives
source("domestic_export_map_iterations.R")
```

## Getting Started

### Prerequisites
Required R packages (automatically installed by scripts):
- sf, dplyr, ggplot2, readr, readxl, tidyr
- furrr, data.table, progress, ozmaps, purrr
- ggpattern, forcats, cowplot, gridExtra
- here, scales, extrafont (optional)

### Setup Instructions

1. **Download and Extract**:
   ```
   # Download all files to a local directory
   # Extract GDB files from ZIP archives:
   unzip Energy_system_analysis_scenarios/QLD_v202412_eplus_tx1.gdb.zip
   unzip Energy_system_analysis_scenarios/QLD_v202412_eplus_tx2.gdb.zip
   ```

2. **Set Working Directory**:
   ```r
   # In R, navigate to the supplementary data folder
   setwd("path/to/supplementary_data")
   ```

3. **Run Analysis Scripts**:
   ```r
   # Recommended: run the full pipeline via the master runner
   source("_RUN_ALL.R")

   # Or run individual scripts:
   source("species_code/Mean_spp_scenario_coverage.R")   # Table 1 — all MNES coverage
   source("Figure_code/Critically_endangered_mean_coverage_and_line_plot.R")  # Table 1 CE/EN + plot
   source("Figure_code/Zonation curves.R")
   source("Figure_code/percent cost increase_line plot.R")
   source("Figure_code/NPV_bar_plot.R")
   source("Figure_code/domestic_export_map_iterations.R")
   ```

### Expected Outputs
All scripts create a `figures/` directory with organized outputs:
- `figures/energy_cost_increase_plot.png`
- `figures/zonation_performance_curves.png` 
- `figures/npv_analysis_plot.png`
- `figures/energy_maps/` (multiple subdirectories)

## File Size Information
- **Total repository size**: ~7.8 GB
- **Large files**:
  - QLD_v202412_eplus_tx1.gdb.zip: 1.0 GB
  - QLD_v202412_eplus_tx2.gdb.zip: 2.7 GB
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
v1.1 (Jan 2026): Updated to full R Project structure; implemented here for relative pathing; added automated unzipping logic and LLM-assisted code optimization.

v1.0 (2025): Initial release for peer review.

Author: Andrew Rogers

LLMs used: Claude AI and Gemini

Last Updated: March 2026

