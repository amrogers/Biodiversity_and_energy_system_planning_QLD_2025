# Supplementary Materials: Biodiversity and Energy System Planning - Queensland 2025

## Overview
This repository contains supplementary data and analysis scripts for the study "Biodiversity and Energy System Planning in Queensland: Balancing Conservation and Infrastructure Development". The materials include energy system modeling results, conservation priority analyses, and spatial mapping tools.

## Repository Structure

```
supplementary_data/
├── README.txt                                    # This file
├── Energy_system_analysis_scenarios/            # Raw energy modeling data
│   ├── QLD_v202412_eplus_tx1.gdb.zip           # Transmission scenario 1 (3.8 GB)
│   └── QLD_v202412_eplus_tx2.gdb.zip           # Transmission scenario 2 (3.9 GB)
├── Energy_system_model_outputs/                 # Processed energy results
│   ├── cost_increase_results.csv               # Cost analysis by scenario
│   └── eplus_Domestic_NPV_2025.xlsx           # Net present value calculations
├── Zonation_output/                            # Conservation priority analysis
│   └── 250m_SNES_ECNES_red_zones_weighted_QLD/
│       ├── out_example1/
│       │   ├── feature_curves.csv              # Performance curves (17.7 MB)
│       │   ├── feature_coverage_summary_with_CI.csv
│       │   ├── features_info.csv
│       │   └── [other Zonation outputs]
│       └── [configuration files]
├── Species_files_weights_table.xlsx            # Species weighting data
├── Table 8_The 524 species.xls                # Species threat status
├── Table 9_The 22 ecological communities.xlsx  # Community threat status
├── Supplementary table_other spatial exclusions.xlsx
└── [R analysis scripts - see below]
```

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

## Analysis Scripts

### 1. Cost Analysis (`percent cost increase_line plot.R`)
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
   # Run individual scripts or all analyses
   source("percent cost increase_line plot.R")
   source("Zonation curves.R") 
   source("NPV_bar_plot.R")
   source("domestic_export_map_iterations.R")
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

### Processing Requirements
- **RAM**: Minimum 8GB recommended (16GB for full spatial analysis)
- **Storage**: 10GB free space for temporary files and outputs
- **Processing time**: 
  - Cost/NPV analyses: 1-2 minutes
  - Zonation analysis: 2-5 minutes  
  - Spatial mapping: 30-60 minutes per transmission scenario

### Troubleshooting

**Common Issues**:
1. **GDB files not found**: Ensure ZIP files are extracted in the correct location
2. **Missing packages**: Scripts will auto-install, but manual installation may be needed
3. **Path errors**: Ensure working directory is set to supplementary data folder


## Version History
- v1.0: Initial release with peer review materials
- [Future versions as needed]

---
Last updated: [Date]
Repository DOI: [DOI if applicable]