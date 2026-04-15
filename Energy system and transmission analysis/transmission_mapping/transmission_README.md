# Transmission Analysis Pipeline — README

**Author:** Andrew Rogers
**Date:** March 2026
**Project:** Biodiversity and Energy System Planning QLD 2025

---

## Overview

This pipeline processes Queensland energy system model outputs (GDB files from the
eplus model) to quantify new transmission infrastructure required under increasing
levels of biodiversity avoidance. The primary output is **Figure 3**: total new
transmission build length (km) versus biodiversity avoidance scenario, comparing
two transmission cost assumptions (TX1 and TX2).

The pipeline is orchestrated by `tx_run_all.R`.

---

## Data Inputs

| File | Location | Description |
|------|----------|-------------|
| `QLD_v202412_eplus_tx1.gdb` | `BESP_data_qld_2025/Energy_system_model_outputs/Energy_system_analysis_scenarios/` | Energy model GDB — TX1 (reference transmission cost) |
| `QLD_v202412_eplus_tx2.gdb` | `BESP_data_qld_2025/Energy_system_model_outputs/Energy_system_analysis_scenarios/` | Energy model GDB — TX2 (doubled transmission cost) |
| `Electricity_Transmission_Lines.shp` | `BESP_data_qld_2025/Energy_system_model_outputs/Electricity_Transmission_Lines/` | Existing QLD transmission network (state govt. source) |
| `Electricity_Transmission_Lines_1km_buff.shp` | same folder | 1 km buffer around existing network |
| `QLD_existing_tx_simplified_buff_no_overlap2.shp` | same folder | Simplified, non-overlapping corridor buffers (supporting analyses only) |

All paths are managed centrally in `_paths.R` (project root).

---

## Biodiversity Threshold Mapping

The energy model GDB contains layers for six biodiversity avoidance thresholds.
These correspond to the proportion of the QLD landscape excluded from energy
development based on Zonation biodiversity priority rankings:

| GDB / layer suffix | `layer_name` in CSV | Paper label | Interpretation |
|--------------------|---------------------|-------------|----------------|
| `_0_` | `threshold_0` | **BAU** | No biodiversity constraint |
| `_10_` | `threshold_10` | **Top 90%** | Top 10% of biodiversity value excluded |
| `_30_` | `threshold_30` | **Top 70%** | Top 30% excluded |
| `_50_` | `threshold_50` | **Top 50%** | Top 50% excluded |
| `_70_` | `threshold_70` | **Top 30%** | Top 70% excluded |
| `_90_` | `threshold_90` | **Top 10%** | Top 90% excluded — most constrained scenario |

---

## Pipeline Scripts

### Prerequisite (one-off)

#### `QLD_reproject_tx_lines.R`
- **Purpose:** Reprojects the existing QLD transmission shapefile to GDA2020
  MGA Zone 55 (EPSG:7855) and calculates line lengths in km.
- **Input:** `paths$existing_tx` → `Electricity_Transmission_Lines.shp`
- **Output:** `Electricity_Transmission_Lines/QLD_existing_tx_projected.shp`
- **TX scenarios:** N/A (reference data only)
- **Notes:** Skipped automatically by `tx_run_all.R` if output already exists.

---

### Primary Pipeline — runs for each scenario (TX1 and TX2)

#### Step 1 — `Transmission_processing.R`
- **Purpose:** Reads all domestic transmission layers from the energy model GDB
  and saves them to a GeoPackage and per-threshold shapefiles.
- **Input:** `paths$gdb_tx1` or `paths$gdb_tx2`
- **Output:**
  - `results/transmission_processing/{tx}/split_tx.gpkg`
  - `results/transmission_processing/{tx}/TX_domestic_layers/transmission_y2050_t{N}.shp`
    (one shapefile per threshold: 0, 10, 30, 50, 70, 90)
- **TX scenarios:** Processes one scenario at a time; called in a loop by
  `tx_run_all.R` for both TX1 and TX2.
- **Layer filter:** Year 2050; technologies: `off`, `pv`, `wind`, `interTX`;
  line types: `bulk`, `spur`, `sink`, `export`.

#### Step 2 — `Transmission_save_layers_as_shapefiles.R`
- **Purpose:** Fallback step — extracts individual shapefiles from the GeoPackage
  if Step 1 did not already populate `TX_domestic_layers/`.
- **Input:** `results/transmission_processing/{tx}/split_tx.gpkg`
- **Output:** `results/transmission_processing/{tx}/TX_domestic_layers/{layer}.shp`
- **TX scenarios:** Both TX1 and TX2 (via loop in `tx_run_all.R`).
- **Notes:** Skipped by `tx_run_all.R` when `TX_domestic_layers/` already
  contains shapefiles from Step 1.

#### Step 3 — `QLD_new_tx_processing_summary.R`
- **Purpose:** The key processing step for Figure 3. For each biodiversity
  threshold, clips the modelled transmission lines using `st_difference` against
  the existing QLD network buffer (`Electricity_Transmission_Lines_1km_buff.shp`)
  to isolate **new-build-only** segments. Summarises total new-build length (km)
  by voltage class.
- **Input:**
  - `results/transmission_processing/{tx}/TX_domestic_layers/transmission_y2050_t{N}.shp`
  - `paths$existing_tx_buff` → `Electricity_Transmission_Lines_1km_buff.shp`
- **Output:**
  - `results/transmission_processing/{tx}/QLD_threshold_tx_new/threshold_{N}_tx_new.shp`
  - `results/transmission_processing/{tx}/QLD_threshold_tx_new/threshold_{N}_summary.csv`
  - `results/transmission_processing/{tx}/QLD_threshold_tx_new_summaries/QLD_threshold_tx_new_summary.csv`
    (**key output — combined across all thresholds**)
- **TX scenarios:** Both TX1 and TX2 (via loop in `tx_run_all.R`).
- **CRS:** Reprojects to GDA2020 MGA Zone 56 (EPSG:7856) for accurate length
  calculation.

#### Step 3b — Copy CSV to `Tx_outputs/` (in `tx_run_all.R`)
- Copies `QLD_threshold_tx_new_summary.csv` from the processing folder to:
  - `Tx_outputs/tx1_domestic_transmission/QLD_threshold_tx_new/QLD_threshold_tx1_new_summary.csv`
  - `Tx_outputs/tx2_domestic_transmission/QLD_threshold_tx_new/QLD_threshold_tx2_new_summary.csv`
- These are the files read by Step 4.

#### Step 4 — `tx_length_figure.R`
- **Purpose:** Reads the new-build summary CSVs for both TX1 and TX2, sums
  length across voltage classes per threshold, maps threshold codes to paper
  labels, saves the combined wide-format table, and generates Figure 3.
- **Input:**
  - `paths$tx1_new_summary` → `QLD_threshold_tx1_new_summary.csv`
  - `paths$tx2_new_summary` → `QLD_threshold_tx2_new_summary.csv`
- **Output:**
  - `results/tables/tx_new_build_length_tx1_tx2.csv` (wide format: bv_scenario | TX1_total_length_km | TX2_total_length_km)
  - `results/figures/tx_length_figure.png` (**Figure 3**)
- **TX scenarios:** Both TX1 and TX2 processed together (reads both CSVs).
- **Notes:** Runs once after both scenario loops complete.

---

### Supporting Analyses — `run_supporting = TRUE` in `tx_run_all.R`

These are not required for Figure 3 but provide additional spatial context.
Set `run_supporting <- TRUE` in `tx_run_all.R` to run them.

#### Supp 1 — `Transmission_clip_join_w_existing.R`
- Clips modelled TX to the existing network 1 km buffer and spatially joins
  clipped segments back to existing lines. Used in upgrade area calculations.
- **Output:** `results/transmission_processing/{tx}/tx_overlap_tables/T{N}_existing_join.csv`
- **TX scenarios:** Both, via loop.

#### Supp 2 — `QLD_exisitng_model_tx_max_join.R`
- Joins modelled TX to the simplified existing corridor network, recording
  maximum modelled kV per corridor. Default scenario: TX2.
- **Input:** `paths$existing_tx_simp` → `QLD_existing_tx_simplified_buff_no_overlap2.shp`
- **Output:** `results/transmission_processing/{tx}/existing_model_tx_max_join/`
- **TX scenarios:** Both, via loop.

#### Supp 3 — `QLD_modelled_tx_easement_area.R`
- Buffers modelled transmission centrelines by a voltage-based easement width
  and calculates total easement area (km²) per threshold.
- **Output:** `results/transmission_processing/{tx}/tx_simplified_buffered/threshold_areas.csv`
- **TX scenarios:** Both, via loop.

#### Supp 4 — `QLD_tranmission_summaries_modelled_existing.R`
- Alternative summary: clips ALL modelled TX (not just new-build) to the
  existing network buffer and summarises by voltage class. Complements Step 3.
- **Output:** `results/transmission_processing/{tx}/model_existing_summaries/QLD_model_existing_combined_summary.csv`
- **TX scenarios:** Both, via loop.

#### Supp 5 — `Transmission_upgrade_calculation.R`
- Calculates the difference in easement area between modelled upgrades and the
  existing network, using ArcGIS-produced join tables as input.
- **Input:** `results/transmission_processing/{tx}/ex_mod_join_tables/transmission_y2050_t{N}.xlsx`
- **Output:** `results/transmission_processing/{tx}/QLD_ex_mod_summaries/total_area_increase_{tx}.xlsx`
- **TX scenarios:** Processes TX1 and TX2 internally (not via the runner loop).

#### Utility — `QLD_summarize_exisiting_in_simiplified_tx.R`
- Fragment script. Summarises existing network attributes into the simplified
  corridor network. **Requires `simplified_tx` to be loaded manually before
  sourcing** — not included in `tx_run_all.R`.

---

## Pre-computed Data in `Tx_outputs/`

The `Tx_outputs/` folder contains the results of a previous complete pipeline run,
including intermediate shapefiles and summary files. Key files:

| File | Description |
|------|-------------|
| `tx1_domestic_transmission/QLD_threshold_tx_new/QLD_threshold_tx1_new_summary.csv` | TX1 new-build summary — direct input to `tx_length_figure.R` |
| `tx2_domestic_transmission/QLD_threshold_tx_new/QLD_threshold_tx2_new_summary.csv` | TX2 new-build summary — direct input to `tx_length_figure.R` |
| `tx1_domestic_transmission/QLD_ex_mod_summaries/total_area_increase_tx1.xlsx` | TX1 upgrade area increase by threshold |
| `Tx_upgrade_summary.xlsx` | Combined upgrade area summary across scenarios |

---

## Key Outputs for the Paper

| Output file | Script | Figure / Table |
|-------------|--------|---------------|
| `results/figures/tx_length_figure.png` | `tx_length_figure.R` | Figure 3 |
| `results/tables/tx_new_build_length_tx1_tx2.csv` | `tx_length_figure.R` | Table basis for Figure 3 |

---

## Running the Pipeline

Open `Biodiversity_and_energy_system_planning_2024.Rproj` in RStudio, then:

```r
source(here::here("Energy system and transmission mapping_code",
                  "transmission_mapping", "tx_run_all.R"))
```

**To regenerate outputs from scratch:** set `overwrite_mode <- TRUE` in
`tx_run_all.R`.

**To run Figure 3 only** (using pre-computed CSVs in `Tx_outputs/`):

```r
source(here::here("Energy system and transmission mapping_code",
                  "transmission_mapping", "tx_length_figure.R"))
```
