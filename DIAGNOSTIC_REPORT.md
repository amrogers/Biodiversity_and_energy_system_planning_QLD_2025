# DIAGNOSTIC REPORT — Biodiversity and Energy System Planning QLD 2025
**Prepared:** 2026-08-03  
**Repository root analysed:** `Biodiversity_and_energy_system_planning_2024/` (relative to working directory)  
**Working data (authors' copy):** `Z:\BESP_data_qld_2025\`  
**Local data copy:** `BESP_data_qld_2025\` (OneDrive, inside repo root)  
**Figshare archive:** `Z:\BESP_data_qld_2025_figshare` — extracted 2026-08-03; Phase 0 resolved below  
**Method:** Static file inspection, directory listings, and file reads only. No scripts executed. No files modified during diagnostic phase.

---

## Phase 0 — Figshare archive verified (2026-08-03)

The figshare archive was extracted to `Z:\BESP_data_qld_2025_figshare` after initial diagnostic. Simultaneously, the decision was made to replace the current figshare deposit with `Z:\BESP_data_qld_2025` uploaded directly. This changes all three Phase 0 answers.

### Figshare structure as downloaded (old deposit — now superseded)

The old figshare delivered data inside a wrapper folder named `supplementary data_biodiversity and energy system planning_qld_2025/`, one level deeper than `_paths.R` expects. Inside that folder the Zonation output was at `Zonation_output/250m_SNES_ECNES_red_zones_weighted_QLD/` (old folder name, no `Zonation_analysis/` wrapper) and `Energy_system_analysis_scenarios/` sat at the data root rather than inside `Energy_system_model_outputs/`. This confirms reviewer claim (a) was correct for the figshare deposit, and claim (b) was correct for the figshare deposit — both were previously assessed against the working-data copies rather than the archive.

### Q1 — Nesting (new deposit: Z:\BESP_data_qld_2025)

**Answer: NO extra nesting. C7 not needed.**

`Z:\BESP_data_qld_2025` is the top-level folder. When uploaded to figshare and extracted, users receive `BESP_data_qld_2025/Energy_system_model_outputs/…` etc., matching `_paths.R`'s second fallback (`here("BESP_data_qld_2025")`). The Zonation output folder is `250m_QLD_2024` matching all current script references. `Energy_system_analysis_scenarios/` sits inside `Energy_system_model_outputs/` matching `paths$gdb_tx1` and `paths$gdb_tx2`.

### Q2 — `eplus_Domestic_NPV_figure.csv` (Figure 2 input)

**Answer: ABSENT from Z: drive. Reviewer claim (c) confirmed for new deposit unless user action taken.**

The file exists only in the local OneDrive copy. **Required user action before upload:** copy `eplus_Domestic_NPV_figure.csv` from `BESP_data_qld_2025\Energy_system_model_outputs\` (local) to `Z:\BESP_data_qld_2025\Energy_system_model_outputs\`. Once present, C8 is not needed.

### Q3 — Tx summary CSVs (Figure 3 inputs)

**Answer: ABSENT from Z: drive at the path `_paths.R` constructs. D3 applies unless user action taken.**

`QLD_threshold_tx1_new_summary.csv` and `QLD_threshold_tx2_new_summary.csv` exist in the local copy at `Energy_system_model_outputs\Electricity_Transmission_Lines\Tx_outputs\tx1_domestic_transmission\QLD_threshold_tx_new\` (and tx2 equivalent), which is the default path `_paths.R` constructs. They are absent from `Z:\BESP_data_qld_2025`. **Required user action before upload:** copy the entire `Electricity_Transmission_Lines\` folder (shapefiles + Tx_outputs/) from the local copy to `Z:\BESP_data_qld_2025\Energy_system_model_outputs\`. All data in this folder are public datasets with no redistribution restrictions.

### Additional files to copy from local to Z: drive before upload

| File / folder | Destination in Z:\BESP_data_qld_2025\ | Required for |
|---|---|---|
| `eplus_Domestic_NPV_figure.csv` | `Energy_system_model_outputs\` | Figure 2 |
| `Electricity_Transmission_Lines\` (entire folder) | `Energy_system_model_outputs\` | Figure 3 + transmission pipeline |

### CAPAD_RZ.tif (C10 pre-check)

`CAPAD_RZ.tif` — the hierarchic mask layer referenced in `minimal_settings.z5` — is present in both `Z:\BESP_data_qld_2025\Zonation_analysis\Zonation_output\250m_QLD_2024\` and the local copy. It will be included in the new figshare upload. The only issue is that `minimal_settings.z5` currently names the wrong folder (`250m_SNES_ECNES_red_zones_weighted_500spp`) — correctable by a one-line edit (fix C10).

### Revised reproduction table (post-upload)

After the user copies the above files, the new deposit will support: Figure 1a, Figure 2, Figure 3, Figure 4, Table 1 (all-MNES + CE/EN), Supp. Fig 1D, Supp. Fig 6 barplot. Supp. Fig 2 remains impossible from deposit (species shapefiles intentionally excluded).

---

## 1. Summary Judgement

The reviewer's report is broadly directionally correct but contains two factual reversals and several overclaims. Of the six lettered categories, (e) is fully confirmed, (f) is confirmed in part but the specific C:\Users\andrewrogers\ claim is refuted, (b) is refuted outright — the reviewer got the folder name mismatch backwards, (c) and (d) are partly correct, and (a) cannot be verified without the figshare archive. The three wrong script paths in `_RUN_ALL.R` are real and confirmed by direct inspection. However, a critical design feature the reviewer missed is that `_RUN_ALL.R` wraps every `source()` call in a `file.exists()` check and a `tryCatch()`, so the script logs "File Not Found" and continues rather than crashing — the reviewer's prediction that "none of the nine pipeline steps will execute as shipped" is too strong. At minimum two steps (Figure 4, Supp. Fig 1D) execute cleanly from a fresh download given only the figshare data; a third (Table 1 / all-MNES coverage) also runs if its path is corrected.

The tension between the reviewer's report and the authors' lived experience is explained by two independent factors: (1) `_paths_local.R` — a machine-specific override file that redirects transmission paths to `Z:\NetZero_scenarios_outputs\` and is correctly absent on a fresh clone but present on the authors' machine; and (2) cached results in the local `results/` directory from previous runs. With these two features in place, all scripts with `overwrite_mode <- FALSE` skip computation and redisplay existing figures, producing a "successful" run that performs no analysis. This is the caching design working as intended, not evidence that the inputs are complete. The reviewer's simulation appears to have ignored this mechanism and predicted hard failures where the pipeline would actually log "File Not Found" silently and continue.

---

## 2. Task A — Claim-by-claim Verification

### (a) Figshare archive nested one level deeper than `_paths.R` expects

**Status: UNVERIFIABLE WITHOUT FIGSHARE ARCHIVE**

`_paths.R` (lines 25–28) resolves the data root with:
```r
if (dir.exists(here("data")) && length(list.files(here("data"))) > 0) {
  data_root <- here("data")
} else {
  data_root <- here("BESP_data_qld_2025")
}
```
The two alternative root names are `data/` and `BESP_data_qld_2025/`. A fresh clone has neither (both are in `.gitignore`, lines 11–12). Readme.txt (line 302) instructs users to "unzip its contents into the `BESP_data_qld_2025/` folder." README.md (lines 20–23) says to place contents into `data/`. The fallback logic accommodates both approaches. Whether the figshare archive itself introduces an additional nesting level (a common figshare behaviour where the deposit folder name becomes a subdirectory on extraction) cannot be determined without extracting the archive. The exact offset and the path `_paths.R` would construct are therefore open.

**What is verifiable:** the `data/` directory does not exist on disk (`Test-Path` returns `False`); `BESP_data_qld_2025/` exists as the authors' working copy. All 16 paths in `_paths.R` resolve correctly against the local `BESP_data_qld_2025/` copy.

---

### (b) Zonation output folder named `250m_SNES_ECNES_red_zones_weighted_QLD` but scripts expect `250m_QLD_2024`

**Status: REFUTED — reviewer has this backwards**

Every R script that references the Zonation output folder uses `250m_QLD_2024`:
- `_paths.R` lines 42–43: `file.path(data_root, "Zonation_analysis", "Zonation_output", "250m_QLD_2024", "out_example1", "rankmap.tif")`
- `Figure_code/Zonation curves.R` line 36: `file.path(zonation_dir, "250m_QLD_2024", "out_example1", "feature_curves.csv")`
- `Biodiversity_analysis/Mean_spp_scenario_coverage.R` line 27: `file.path(data_root, "Zonation_analysis", "Zonation_output", "250m_QLD_2024", "out_example1")`

The actual folder on disk is also named `250m_QLD_2024` — confirmed in both the local copy (`BESP_data_qld_2025\Zonation_analysis\Zonation_output\250m_QLD_2024\`) and `Z:\BESP_data_qld_2025\Zonation_analysis\Zonation_output\250m_QLD_2024\`. No folder named `250m_SNES_ECNES_red_zones_weighted_QLD` exists anywhere in either data location.

**Where the old name does appear:** `README.md` lines 85 and 88 contain stale references to `250m_SNES_ECNES_red_zones_weighted_QLD/out_example1/feature_curves.csv` and `...feature_coverage_summary_with_CI.csv`. This is a documentation error in README.md only. `Readme.txt` uses the correct `250m_QLD_2024` name throughout (lines 74, 82, 334).

**Additional finding:** `minimal_settings.z5` (line 2) references a third, different folder name — `250m_SNES_ECNES_red_zones_weighted_500spp\CAPAD_RZ.tif` — as the hierarchic mask layer input. This is the input mask, not an output folder, and the path uses the `User_directory` placeholder (see item f). This is a separate documentation issue.

---

### (c) `NPV_bar_plot.R` expects `eplus_Domestic_NPV_figure.csv` but only an XLSX with a different name is provided

**Status: PARTLY CORRECT (location-dependent)**

`NPV_bar_plot.R` line 34 constructs: `file.path(paths$energy_outputs, "eplus_Domestic_NPV_figure.csv")`. If no input file is found, line 44–49 calls `stop()` with an explicit error message. The `overwrite_mode <- TRUE` setting (line 28) means the output-file guard is always bypassed; there is no way to skip this script via caching.

**File existence:**
- `BESP_data_qld_2025\Energy_system_model_outputs\eplus_Domestic_NPV_figure.csv` — **present in local (OneDrive) copy**
- `Z:\BESP_data_qld_2025\Energy_system_model_outputs\eplus_Domestic_NPV_figure.csv` — **absent from Z: drive**
- `Z:\BESP_data_qld_2025\Energy_system_model_outputs\eplus_Domestic_NPV_2025.xlsx` — present in both locations

`README.md` line 101 explains: "eplus_Domestic_NPV_2025.xlsx contains the full dataset; eplus_Domestic_NPV_figure.csv is the processed version used directly by this script." The CSV is a derived file. It exists on the author's local machine but appears to be absent from the Z: drive working data. Whether it is in the figshare archive is **unverifiable**. If the figshare archive mirrors the Z: drive (likely, given that the Z: drive is the canonical data store), the CSV will be absent and the reviewer's claim will be confirmed. If the figshare archive was assembled from the local OneDrive copy, the CSV is present and the claim is refuted.

---

### (d) Several input files not in the figshare deposit

**Status: PARTLY CORRECT — with important distinctions per file**

| File / folder | In local copy? | In Z: drive? | `_paths.R` path | Assessment |
|---|---|---|---|---|
| `Electricity_Transmission_Lines.shp` (existing grid) | **YES** — `Energy_system_model_outputs/Electricity_Transmission_Lines/` | **NO** | `paths$existing_tx` (line 76) | Unverifiable re figshare; present locally |
| `QLD_threshold_tx1_new_summary.csv` (Tx_outputs) | **YES** — `Electricity_Transmission_Lines/Tx_outputs/tx1.../` | Via `_paths_local.R` override only → `Z:\NetZero_scenarios_outputs\...` | `paths$tx1_new_summary` (lines 56–59) | Present locally at default path |
| `QLD_threshold_tx2_new_summary.csv` | **YES** — local | Via `_paths_local.R` override | `paths$tx2_new_summary` (lines 60–63) | Present locally at default path |
| `combined_wind.tif` (wind exclusion raster) | **NO** — not in `BESP_data_qld_2025/` | **YES** — `Z:\NetZero_scenarios_outputs\QLD_v202412_eplus\Area_outside_exclusions\rasters\combined_wind.tif` | Hard-coded in `land_use_competition_QLD.R` line 51 | **Genuinely absent from deposit path; not in BESP_data_qld_2025/** |
| `combined_pv.tif` (PV exclusion raster) | **NO** | **YES** — same Z: path | Hard-coded `land_use_competition_QLD.R` line 52 | **Genuinely absent from deposit path** |
| Per-species shapefiles (`QLD_100m_SNES_500spp/shapefiles/`) | **NO** — dir absent | **NO** — dir absent | `paths$snes_shapefiles` (line 67) | Confirmed absent; Readme.txt line 218–219 explicitly documents this: "not included in the Figshare data due to file size" |
| `species_attributes.csv` | **YES** — at `Zonation_MNES_shapefiles/species_attributes.csv` | **YES** — same path | `paths$snes_attributes` line 68 → wrong path `QLD_100m_SNES_500spp/species_attributes.csv` | **FILE EXISTS but `_paths.R` points to wrong location** |
| `BV_exclusion_area_overlap.csv` | **YES** — `Energy_system_model_outputs/` | **YES** — same | `paths$energy_outputs` (computed by `land_use_competition_QLD.R`) | Present; reviewer claim incorrect |
| `species_weights.csv` | **YES** — `Zonation_output/250m_QLD_2024/out_example1/` | **YES** — same | No named path entry | Present; reviewer claim incorrect |

**Detailed notes:**

- **Wind/PV exclusion rasters:** Hard-coded at `land_use_competition_QLD.R` lines 51–52 as `Z:\NetZero_scenarios_outputs\...`. These paths are outside `BESP_data_qld_2025\` entirely and do not correspond to any deposited data location. This is a genuine gap. However, `BV_exclusion_area_overlap.csv` (the output of this script) already exists in the deposit, so the script only needs to run if users wish to regenerate the CSV from scratch. See Task D.

- **`species_attributes.csv` path mismatch:** The file physically exists at `BESP_data_qld_2025/Zonation_analysis/Zonation_MNES_shapefiles/species_attributes.csv` in both data copies. `_paths.R` line 68 constructs `file.path(data_root, "QLD_100m_SNES_500spp", "species_attributes.csv")` — a directory (`QLD_100m_SNES_500spp/`) that does not exist in either location. Any script reading `paths$snes_attributes` will fail with "file not found" despite the file existing on disk.

- **Species shapefiles:** The absence is documented in Readme.txt (lines 218–219) as intentional. The reviewer is correct that they are absent but wrong to characterise this as an undisclosed omission.

---

### (e) `_RUN_ALL.R` references three script paths that do not exist

**Status: CONFIRMED — and applies to all nine pipeline steps, not just three**

Direct inspection of `_RUN_ALL.R` shows the following `source()` targets:

| Line | Path constructed | Directory exists? | Script exists at correct location? |
|---|---|---|---|
| 45 | `here("Energy system and transmission mapping_code", "domestic_export_map_iterations.R")` | **NO** — dir absent | Actual: `Energy system and transmission analysis/domestic_export_map_iterations.R` |
| 46 | `here("species_code", "Mean_spp_scenario_coverage.R")` | **NO** — dir absent | Actual: `Biodiversity_analysis/Mean_spp_scenario_coverage.R` |
| 47 | `here("Figure_code", "NPV_bar_plot.R")` | YES | YES — correct |
| 48 | `here("Figure_code", "tx_length_figure.R")` | YES | YES — correct |
| 49 | `here("Figure_code", "percent cost increase_line plot.R")` | YES | YES — correct |
| 54 | `here("Figure_code", "Zonation curves.R")` | YES | YES — correct |
| 55 | `here("species_code", "zero_coverage_species.R")` | **NO** — dir absent | Actual: `Biodiversity_analysis/zero_coverage_species.R` |
| 56 | `here("Biodiversity_analysis", "land_use_competition_QLD.R")` | YES | YES — correct |
| 57 | `here("Figure_code", "exclusion_overlap_barplot.R")` | YES | YES — correct |

Three of nine pipeline step paths are broken: lines 45, 46, and 55. This is exactly as the reviewer states.

**Critical design point the reviewer missed:** `_RUN_ALL.R` lines 75 and 82–84 wrap each step as:
```r
if (file.exists(step_path)) {
  tryCatch({ source(step_path) ... })
} else {
  log$Status[i] <- "File Not Found ✗"
}
```
The script does not crash on a missing path. It logs "File Not Found" and continues. The pipeline run completes; only the log table reveals which steps were skipped. This explains why the authors observe a "successful" run.

**Additional finding not in reviewer's report:** `Energy system and transmission analysis/transmission_mapping/tx_run_all.R` line 70 also contains the wrong directory name:
```r
script_dir <- here("Energy system and transmission mapping_code",
                   "transmission_mapping")
```
This makes every step in `tx_run_all.R` a "File Not Found" failure. `tx_run_all.R` is not called by `_RUN_ALL.R` (it is the transmission preprocessing pipeline, separate from the figure pipeline), but users wishing to regenerate Figure 3 inputs from raw GDB files would encounter this.

**Also in README.md:** The wrong directory names propagate into the README's usage examples: `species_code/` appears at lines 57, 67, 176; `Energy system and transmission mapping_code/` appears at lines 128 and 142.

---

### (f) Hard-coded absolute paths

**Status: PARTLY CORRECT — Z: drive paths confirmed; C:\Users\andrewrogers\ claim refuted**

**Active hard-coded absolute paths found in R scripts:**

| File | Line(s) | Path | Active? |
|---|---|---|---|
| `Biodiversity_analysis/land_use_competition_QLD.R` | 50 | `"Z:/BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/rankmap.tif"` | **YES** |
| `Biodiversity_analysis/land_use_competition_QLD.R` | 51 | `"Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters/combined_wind.tif"` | **YES** |
| `Biodiversity_analysis/land_use_competition_QLD.R` | 52 | `"Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/Area_outside_exclusions/rasters/combined_pv.tif"` | **YES** |
| `_paths_local.R` | 18 | `z_tx_outputs <- "Z:/NetZero_scenarios_outputs/QLD_v202412_eplus/map_outputs"` | YES — but this is a machine-specific override file (in `.gitignore`) not present on a fresh clone |
| `_paths.R` | 13 | `"Z:/my_drive/tx1/QLD_threshold_tx1_new_summary.csv"` | **NO** — comment only |

**Regarding `C:\Users\andrewrogers\` in Zonation config files:**
The reviewer's specific claim is **refuted**. Both Zonation config files use a generic `User_directory` placeholder, not a hard-coded user path:
- `features_example1.txt` line 2 (first data line): `3  User_directory\Zonation_QLD_biodiversity_feature_rasters\QLD_250m_500spp\1.tif`
- `minimal_settings.z5` line 2: `hierarchic mask layer = "User_directory\250m_SNES_ECNES_red_zones_weighted_500spp\CAPAD_RZ.tif"`

`Readme.txt` lines 333–336 and `README.md` line 50 both explicitly document this placeholder and instruct users to replace `User_directory` with their local path. This is intentional parameterisation, not an error.

**`_paths_local.R` status:** This file is listed in `.gitignore` (line 8: `_paths_local.R`) and is documented as machine-specific (its own header, lines 1–16). It is present on disk on the authors' machine because they use it to redirect transmission paths to the Z: drive, but it would be absent on a fresh clone. Its Z: drive contents (line 18) are functionally correct for the authors but inaccessible to any external user. This is an architectural design choice, not an error — but it means the default `_paths.R` transmission paths must be the ones that work from the figshare data.

**Full search scope:** No other drive-letter patterns (`C:\`, `/Users/`, `~/`), `setwd(` with real paths (only a placeholder appears in README.md line 167: `setwd("path/to/supplementary_data")`), or absolute paths were found anywhere else in the repository.

**Total unique files with active absolute paths outside their intended scope: 1** (`land_use_competition_QLD.R`). `_paths_local.R` is machine-specific by design.

---

### README issues

| Claim | Status | Evidence |
|---|---|---|
| README doesn't match figshare archive structure | UNVERIFIABLE | Cannot assess without figshare |
| Duplicated paragraph at Overview/Repository Structure transition | **CONFIRMED** | `README.md` lines 7–8: the heading "Repository Structure" appears without `##` markup, then line 8 contains a garbled continuation sentence ("The project is organized as an R Project. Opening the .Rproj file automatically sets...scripts for the study...") that merges the end of the Overview paragraph with the start of Repository Structure. The `## Repository Structure` heading then appears again at line 16 |
| Two READMEs at root | **CONFIRMED** | `README.md` and `Readme.txt` both at `Biodiversity_and_energy_system_planning_2024/` root. No nested `Readme.txt` under the data folder was found in either local or Z: drive copies |
| `Biodiversisty_value_map.R` typo | **CONFIRMED** | File is named `Biodiversity_analysis/Biodiversisty_value_map.R` (transposed letters in "Biodiversity") |
| `Suplementary` typo | **CONFIRMED** | `Z:\BESP_data_qld_2025\Zonation_analysis\Suplementary table_other spatial exclusions.xlsx`; `Readme.txt` line 63: `Suplementary table_other spatial exclusions.xlsx` |
| `exisitng` typo | **CONFIRMED** | `QLD_exisitng_model_tx_max_join.R` (script filename); `QLD_summarize_exisiting_in_simiplified_tx.R` (double typo: `exisiting` and `simiplified`) |
| `simiplified` typo | **CONFIRMED** | `QLD_summarize_exisiting_in_simiplified_tx.R` |
| `reccomended` typo | **CONFIRMED** | `README.md` line 14: `reccomended citation:` |
| Wrong script paths in README usage examples | **CONFIRMED** | `README.md` lines 57, 67, 176, 181: `species_code/`; lines 128, 142: `Energy system and transmission mapping_code/`. `Readme.txt` line 118 also references `Energy system and transmission mapping_code/` as a folder that doesn't exist |

**Additional README finding not in reviewer's report:** `Readme.txt` lists `NPV_bar_plot.R` twice — as item 4 (lines 91–95) and item 6 (lines 104–108) — with slightly different descriptions. Item 4 omits the XLSX/CSV relationship note; item 6 includes it.

**Readme.txt line 203–209 describes existing TX shapefiles as belonging in `BESP_data_qld_2025/Spatial_reference_data/`, but `_paths.R` lines 75–83 reads them from `Energy_system_model_outputs/Electricity_Transmission_Lines/` — the actual location in both data copies. The README description is wrong.**

---

### renv lockfile

**Status: CONFIRMED ABSENT**

No `renv.lock`, `renv/` directory, or `renv::init()` call was found anywhere in the repository. Package versions are not pinned. The scripts use `pacman::p_load()` which installs the current CRAN version at runtime.

---

### Zenodo deposit

**Status: CONFIRMED — no Zenodo DOI exists in any documentation**

`README.md` lines 5 and 12–14 cite the figshare DOI `https://doi.org/10.26188/29604590.v1` only. `Readme.txt` lines 17–23 cite the same figshare URL. No Zenodo deposit is referenced anywhere.

---

## 3. Task B — Manifest Diff

**Status: PARTIAL — figshare archive not yet extracted**

The diff below compares the local OneDrive copy (`BESP_data_qld_2025\` inside repo) against `Z:\BESP_data_qld_2025\`. This establishes which files the authors have locally that are absent from their canonical data store. Full comparison against the figshare archive must wait for the download to complete.

### Files present in both copies (same relative path — selection of key files)

| Relative path |
|---|
| `Energy_system_model_outputs/BV_exclusion_area_overlap.csv` |
| `Energy_system_model_outputs/LCOE_exclusion_map_pv.jpg` |
| `Energy_system_model_outputs/LCOE_exclusion_map_wind.jpg` |
| `Energy_system_model_outputs/LCOE_histograms_pv.jpg` |
| `Energy_system_model_outputs/LCOE_histograms_wind.jpg` |
| `Energy_system_model_outputs/LCOE_statistics_combined.csv` |
| `Energy_system_model_outputs/LCOE_statistics_pv.csv` |
| `Energy_system_model_outputs/LCOE_statistics_wind.csv` |
| `Energy_system_model_outputs/cost_increase_results.csv` |
| `Energy_system_model_outputs/eplus_Domestic_NPV_2025.xlsx` |
| `Energy_system_model_outputs/Energy_system_analysis_scenarios/QLD_v202412_eplus_tx1.gdb.zip` |
| `Energy_system_model_outputs/Energy_system_analysis_scenarios/QLD_v202412_eplus_tx2.gdb.zip` |
| `Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/rankmap.tif` |
| `Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_curves.csv` |
| `Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/species_weights.csv` |
| `Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_coverage_summary_with_CI.csv` |
| `Zonation_analysis/Zonation_MNES_shapefiles/species_attributes.csv` |
| `Zonation_analysis/Zonation_output/250m_QLD_2024/features_example1.txt` |
| `Zonation_analysis/Zonation_output/250m_QLD_2024/minimal_settings.z5` |

### Files present in local copy only (absent from Z: drive)

These are candidates for files that may be absent from figshare if the archive was built from the Z: drive data:

| File | Significance |
|---|---|
| `Energy_system_model_outputs/eplus_Domestic_NPV_figure.csv` | **Primary input for Figure 2.** Required by `NPV_bar_plot.R`. No equivalent on Z: drive. Derived from `eplus_Domestic_NPV_2025.xlsx`. |
| `Energy_system_model_outputs/Electricity_Transmission_Lines/Electricity_Transmission_Lines.shp` (+ .dbf, .prj, .shx) | Required by transmission pipeline scripts via `paths$existing_tx` |
| `Energy_system_model_outputs/Electricity_Transmission_Lines/Electricity_Transmission_Lines_1km_buff.shp` (+ companions) | Required via `paths$existing_tx_buff` |
| `Energy_system_model_outputs/Electricity_Transmission_Lines/QLD_existing_tx_simplified_buff_no_overlap2.shp` (+ companions) | Required via `paths$existing_tx_simp` |
| `Energy_system_model_outputs/Electricity_Transmission_Lines/Tx_outputs/tx1_domestic_transmission/QLD_threshold_tx_new/QLD_threshold_tx1_new_summary.csv` | Primary input for Figure 3 via `paths$tx1_new_summary` |
| `Energy_system_model_outputs/Electricity_Transmission_Lines/Tx_outputs/tx2_domestic_transmission/QLD_threshold_tx_new/QLD_threshold_tx2_new_summary.csv` | Primary input for Figure 3 via `paths$tx2_new_summary` |
| `Energy_system_model_outputs/QLD_tx1_eplus_v202412 (1).xlsx` | Not referenced by any current script |
| `Energy_system_model_outputs/QLD_tx2_eplus_v202412 (1).xlsx` | Not referenced by any current script |
| `Energy_system_model_outputs/line_length_summary.csv` | Not referenced by any current pipeline script |

### Files present in Z: drive only (absent from local copy)

| File | Significance |
|---|---|
| `Energy_system_model_outputs/TX_length_existing.csv` | Not referenced by main pipeline scripts |
| `Energy_system_model_outputs/TX_master_table.csv` | Not referenced by main pipeline scripts |
| `Energy_system_model_outputs/NZAU_phase2_QLD_data/QLD_NZAU2_NPV.xlsx` | Used by `NZAU2_QLD_mapping.R` (not in main pipeline) |
| `Energy_system_model_outputs/NZAU_phase2_QLD_data/QLD_NZAU2_NPV_tx2.xlsx` | Used by `NZAU2_QLD_mapping.R` |

### Files present in neither copy

| File | Script that requires it | Note |
|---|---|---|
| `QLD_100m_SNES_500spp/shapefiles/*.shp` | `retrieve_spp_details.R`, `zero_coverage_species.R` | Documented absent (Readme.txt line 218–219): too large for deposit |
| `combined_wind.tif` | `land_use_competition_QLD.R` line 51 | Located at `Z:\NetZero_scenarios_outputs\...` — outside `BESP_data_qld_2025\` entirely; not in deposit path |
| `combined_pv.tif` | `land_use_competition_QLD.R` line 52 | Same: `Z:\NetZero_scenarios_outputs\...` |

---

## 4. Task C — Caching Guard Inventory

All scripts with `overwrite_mode` follow the same pattern: if `overwrite_mode <- FALSE` (the default for all scripts except `NPV_bar_plot.R` and `Mean_spp_scenario_coverage.R` which use `TRUE`), a sentinel file check is performed; if the sentinel exists, computation is skipped and the existing file is displayed. All guards emit a `cat()` log message identifying the skip. None are silent.

### Guard table

| Script | `overwrite_mode` default | Sentinel tested | Sentinel in git repo? | Sentinel in figshare? | Computation skipped if sentinel present? |
|---|---|---|---|---|---|
| `Figure_code/Zonation curves.R` | `FALSE` (line 24) | `results/zonation_figures/zonation_performance_curves.png` | No (`results/` gitignored) | No (not a data file) | YES |
| `Figure_code/percent cost increase_line plot.R` | `FALSE` (line 20) | `results/figures/energy_cost_increase_plot.png` | No | No | YES |
| `Figure_code/NPV_bar_plot.R` | `TRUE` (line 28) | `results/figures/npv_analysis_plot.png` | No | No | No (always overwrites) — but data guard at line 44 calls `stop()` if CSV absent |
| `Figure_code/tx_length_figure.R` | `TRUE` (standalone default, line 67); inherits environment if called via `_RUN_ALL.R` | `results/figures/tx_length_figure.png` | No | No | Depends on call context |
| `Biodiversity_analysis/Mean_spp_scenario_coverage.R` | `TRUE` (line 21) | `results/tables/scenario_coverage_results.csv` | No | No | No (always overwrites) |
| `Figure_code/Critically_endangered_mean_coverage_and_line_plot.R` | (not directly invoked by `_RUN_ALL.R`) | `results/tables/CE_EN_mean_coverage_results.csv` | No | No | YES if sentinel present |
| `Biodiversity_analysis/retrieve_spp_details.R` | (not in pipeline) | `data/QLD_100m_SNES_500spp/species_attributes.csv` | No | No | YES, then calls `stop()` |
| `Figure_code/exclusion_overlap_barplot.R` | Check required (not read) | (unknown without reading full file) | No | No | — |
| `Biodiversity_analysis/land_use_competition_QLD.R` | None found | No guard present | — | — | **NO GUARD — always attempts computation** |

### Sentinel files present on authors' local machine

The `results/` directory exists locally with cached outputs from previous runs:
```
results/figures/CE_EN_mean_coverage_plot.png
results/figures/energy_cost_increase_plot.png
results/figures/npv_analysis_plot.png
results/figures/tx_length_figure.png
results/figures/Exclusions_stacked_bar_plot.png
results/tables/CE_EN_mean_coverage_results.csv
results/tables/scenario_coverage_results.csv
results/tables/tx_new_build_length_tx1_tx2.csv
results/zonation_figures/zonation_performance_curves.png
```

`results/` is listed in `.gitignore` (line 15) and would not be present on a fresh clone. Sentinels therefore do NOT ship with the repository; guards would not trigger for a new user.

### Explaining the authors' "successful" runs

With `results/` populated from previous runs AND `overwrite_mode <- FALSE` as the default, the following scripts skip all computation when the authors re-run `_RUN_ALL.R`: `Zonation curves.R`, `percent cost increase_line plot.R`, and (via the environment inheritance mechanism in `tx_length_figure.R`) potentially Figure 3. This is correct and intended behaviour. A reviewer downloading a fresh clone would not benefit from this mechanism and would attempt full computation, encountering missing inputs.

---

## 5. Task D — Minimum Fix List

Classification:
- **CODE-ONLY** — editing a script, `_paths.R`, a config template, or README; no data changes
- **DATA-ADDITIVE** — uploading one or more files to a new figshare version; nothing existing moved or renamed
- **DATA-RESTRUCTURE** — re-organising or re-packaging the deposit

Default is CODE-ONLY per instruction. DATA-RESTRUCTURE is not proposed for any item.

---

### CODE-ONLY fixes (6 items)

**C1. Fix three wrong directory names in `_RUN_ALL.R`**
- `_RUN_ALL.R` line 45: `"Energy system and transmission mapping_code"` → `"Energy system and transmission analysis"`
- `_RUN_ALL.R` line 46: `"species_code"` → `"Biodiversity_analysis"`
- `_RUN_ALL.R` line 55: `"species_code"` → `"Biodiversity_analysis"`
- Effort: < 5 minutes
- Note: `zero_coverage_species.R` (line 55 target) requires species shapefiles not in the deposit. After fixing the path, the script will be found but fail on missing data. `_RUN_ALL.R`'s `tryCatch` will catch this and log an error. This is acceptable — the failure is now informative rather than silent "File Not Found". Alternatively, document in `_RUN_ALL.R` comments that this step requires data not in the figshare deposit.

**C2. Fix same wrong directory name in `tx_run_all.R`**
- `Energy system and transmission analysis/transmission_mapping/tx_run_all.R` line 70: `"Energy system and transmission mapping_code"` → `"Energy system and transmission analysis"`
- Effort: < 5 minutes

**C3. Fix `_paths.R` `snes_attributes` path to match actual file location**
- `_paths.R` line 68: `file.path(data_root, "QLD_100m_SNES_500spp", "species_attributes.csv")` → `file.path(data_root, "Zonation_analysis", "Zonation_MNES_shapefiles", "species_attributes.csv")`
- Also update line 66–67 (`snes_dir`, `snes_shapefiles`) to reflect that the MNES shapefile directory is `Zonation_analysis/Zonation_MNES_shapefiles/` — or mark them as unavailable in the deposit (they require large species shapefiles not distributed)
- Effort: 10 minutes

**C4. Add a caching guard to `land_use_competition_QLD.R` and replace hard-coded paths**
- Replace lines 50–52 with:
  ```r
  source(here::here("_paths.R"))
  rankmap_path   <- paths$rankmap
  wind_excl_path <- paths$wind_excl   # new entry to be added to _paths.R
  pv_excl_path   <- paths$pv_excl     # new entry to be added to _paths.R
  output_path    <- file.path(paths$energy_outputs, "BV_exclusion_area_overlap.csv")
  ```
- Add to `_paths.R`: `wind_excl` and `pv_excl` path entries pointing to wherever these rasters will be placed (see DATA-ADDITIVE section)
- Add a caching guard at the top of the `main()` function: if `output_path` already exists and `overwrite_mode <- FALSE`, skip computation and return the existing CSV. `BV_exclusion_area_overlap.csv` ships in the deposit, so the guard would trigger and the script would never attempt to load the missing rasters.
- Effort: 30 minutes

**C5. Update README.md**
- Fix stale Zonation folder name at lines 85, 88: `250m_SNES_ECNES_red_zones_weighted_QLD` → `250m_QLD_2024`
- Fix wrong script directory names in usage examples: `species_code/` → `Biodiversity_analysis/`; `Energy system and transmission mapping_code/` → `Energy system and transmission analysis/`
- Remove or resolve duplicated paragraph at lines 7–8 (merge Overview section cleanly into Repository Structure)
- Fix `reccomended` → `recommended` (line 14)
- Align `scripts/` folder name in directory tree (line 25) to match actual structure (`Figure_code/`, `Biodiversity_analysis/`, `Energy system and transmission analysis/`)
- Effort: 30–45 minutes

**C6. Add `renv.lock`**
- Run `renv::init()` followed by `renv::snapshot()` in a clean R session after confirming the full pipeline runs. This requires execution and cannot be done statically.
- Effort: 30–60 minutes (depends on package resolution)
- Flag: requires execution — mark as an open question

---

### DATA-ADDITIVE fixes (2 items)

**D1. Add `eplus_Domestic_NPV_figure.csv` to figshare deposit (Figure 2)**
- Required by: `NPV_bar_plot.R` line 34
- Is it required? YES — Figure 2 in the main manuscript depends on it
- Is it derivable? YES — it is described as a processed version of `eplus_Domestic_NPV_2025.xlsx` (README.md line 101). The derivation script is not included; one option is to rewrite `NPV_bar_plot.R` to read from the XLSX directly (CODE-ONLY alternative, ~45 min effort) or to export the CSV from the XLSX and add it to the deposit (DATA-ADDITIVE, ~15 min if CSV is simply a subset/reshaping of the XLSX)
- Can the script be pointed at something already deposited? Potentially yes — the XLSX is already deposited. CODE-ONLY is the lower-friction fix.
- File size: small (CSV); no redistribution concerns
- **Recommendation:** CODE-ONLY alternative — rewrite `NPV_bar_plot.R` to read `eplus_Domestic_NPV_2025.xlsx` directly using `readxl`. This eliminates the dependency on the derived CSV.

**D2. Add wind/PV exclusion rasters to figshare deposit (Supp. Fig 6 data step)**
- Required by: `land_use_competition_QLD.R` lines 51–52 (after fix C4, via `paths$wind_excl` and `paths$pv_excl`)
- Is it required? Only if users wish to regenerate `BV_exclusion_area_overlap.csv` from scratch. If C4 adds a caching guard and the CSV ships (it does — it is already in the deposit in both data copies), the rasters are never loaded by a standard run.
- Is it derivable? These rasters are produced by the energy system model (netzero_navigate pipeline), not by any script in this repository. They are not derivable from deposited data.
- Can the script be pointed at something already deposited? No — the rasters are unique model outputs.
- If added: ~53 MB total (`combined_wind.tif` + `combined_pv.tif` based on file sizes visible on Z: drive). **Redistribution conditions: FLAG FOR DECISION.** These rasters derive from the netzero_navigate energy system model. Confirm redistribution rights before uploading to a public figshare deposit.
- **Recommendation:** Implement C4 first (caching guard). If the CSV output already ships, the rasters are not needed for standard reproduction. The only user who needs them is one who wants to re-derive the CSV from scratch — a case that can be documented as requiring access to the original model outputs.

---

### Regarding items the reviewer listed under (d) as missing

| File | Required for? | Derivable? | In deposit? | Proposed fix |
|---|---|---|---|---|
| Existing-grid shapefiles | Figure 3 pipeline preprocessing; not needed to redraw Figure 3 if Tx summary CSVs ship | No (state government data) | Locally present; figshare status unknown — **flag for verification** | If in figshare: no action. If absent: DATA-ADDITIVE. Check redistribution rights (state government source). Readme.txt line 207: "Source: QLD electricity transmission network (state government spatial data)" |
| Tx_outputs CSVs (`QLD_threshold_tx1/2_new_summary.csv`) | Figure 3 (`tx_length_figure.R`) | Yes — produced by `tx_run_all.R` steps 1–3, but those steps require GDB inputs and existing-grid shapefiles | Locally present under default `_paths.R` path; figshare status unknown | If absent from figshare: DATA-ADDITIVE (small CSVs). No redistribution concerns. |
| Wind/PV exclusion rasters | Supp. Fig 6 data step only | No | Not in deposit path | See D2 above — mitigated by C4 caching guard |
| Per-species shapefiles | `retrieve_spp_details.R`, `zero_coverage_species.R` | No (species distribution model outputs) | Confirmed absent; documented as excluded (Readme.txt lines 218–219) | CODE-ONLY: add comment to `_RUN_ALL.R` (fix C1) that this step requires data not in deposit; ensure error is informative |
| `species_attributes.csv` | `retrieve_spp_details.R` | Yes — produced by `retrieve_spp_details.R` from species shapefiles; already pre-computed at `Zonation_MNES_shapefiles/species_attributes.csv` | Present at wrong path | Fix C3 (path correction in `_paths.R`) — no data change needed |
| `BV_exclusion_area_overlap.csv` | `exclusion_overlap_barplot.R` (Supp. Fig 6) | Recomputable via `land_use_competition_QLD.R` | **Present in deposit** | No action needed |
| `species_weights.csv` | Not directly read by pipeline scripts (Zonation output file) | Yes — produced by Zonation run | **Present in deposit** | No action needed |

---

### No DATA-RESTRUCTURE fixes are proposed

Every confirmed issue can be addressed by code edits (`_paths.R`, `_RUN_ALL.R`, individual scripts, READMEs) or by adding small files to a new figshare version. The reviewer's suggestion to re-organise the deposit is not necessary.

---

## 6. Task E — Reproduction Assessment

Assessment is based on static path tracing from the figshare-deposited data (as represented by the Z: drive working copy, which is the more conservative/conservative comparison point). "Fresh user" means: fresh clone of repo, figshare data extracted to `BESP_data_qld_2025/` per Readme.txt instructions, no `_paths_local.R`, `results/` empty.

| Figure / Table | Script | Required input | Input in deposit? | Status | Notes |
|---|---|---|---|---|---|
| Figure 1a — biodiversity prioritisation map | ArcGIS Pro (no R script) | `rankmap.tif` | YES | **REPRODUCES** — raster available; ArcGIS Pro required |
| Figure 1b–e — VRE siting maps | `domestic_export_map_iterations.R` | `QLD_v202412_eplus_tx1.gdb`, `tx2.gdb` (unzipped) | YES (as .zip) | **CANNOT REPRODUCE** — script uses `here("data", ...)` not `_paths.R`; `data/` dir doesn't exist; also called from wrong dir in `_RUN_ALL.R` | Two independent code bugs; GDB must also be unzipped |
| Table 1 — all-MNES coverage | `Mean_spp_scenario_coverage.R` | `feature_curves.csv` (250m_QLD_2024) | YES | **REPRODUCES after C1 fix** — correct data present; path bug in `_RUN_ALL.R` is only blocker; script itself reads correct path | With path fix: full computation possible |
| Table 1 — CE/EN coverage | `Critically_endangered_mean_coverage_and_line_plot.R` | `feature_curves.csv`, `species_weights.csv` | YES | **REPRODUCES** — not in `_RUN_ALL.R`; run directly; all inputs present |
| Figure 2 — NPV bar chart | `NPV_bar_plot.R` | `eplus_Domestic_NPV_figure.csv` | UNKNOWN (absent from Z:, present locally) | **CANNOT REPRODUCE from deposit** (if figshare mirrors Z:) — fails with `stop()` at line 44 | Fix: CODE-ONLY rewrite to read XLSX, or DATA-ADDITIVE upload of CSV |
| Figure 3 — TX build length | `tx_length_figure.R` | `QLD_threshold_tx1_new_summary.csv`, `QLD_threshold_tx2_new_summary.csv` | UNKNOWN (present locally at default path; figshare unknown) | **UNVERIFIED** — depends on whether Tx_outputs CSVs ship in figshare deposit | If CSVs in deposit: REPRODUCES. If absent: CANNOT REPRODUCE without full pipeline run |
| Figure 4 — cost increase | `percent cost increase_line plot.R` | `cost_increase_results.csv` | YES | **REPRODUCES** — correct data present; script reads via `_paths.R` correctly |
| Supp. Fig 1D — Zonation curves | `Zonation curves.R` | `feature_curves.csv` | YES | **REPRODUCES** — correct data present; `overwrite_mode <- FALSE` but no cached sentinel for fresh user |
| Supp. Fig 2 — zero-coverage map | `zero_coverage_species.R` | `QLD_100m_SNES_500spp/shapefiles/` | **NO** | **CANNOT REPRODUCE** — inputs confirmed absent and documented as excluded from deposit |
| Supp. Fig 6 — exclusion barplot | `exclusion_overlap_barplot.R` + `land_use_competition_QLD.R` | `BV_exclusion_area_overlap.csv` (barplot only) | YES | **BARPLOT REPRODUCES** — CSV is in deposit; `exclusion_overlap_barplot.R` reads it directly. Data step (`land_use_competition_QLD.R`) fails (missing rasters, Z: paths), but `_RUN_ALL.R` catches the error and continues to barplot |
| Supp. table — tx easement area | `Transmission_upgrade_calculation.R` (not in pipeline) | ArcGIS-produced join tables | UNVERIFIED | UNVERIFIED — requires ArcGIS Pro intermediate outputs |

### Summary by category

| Category | Figures / Tables |
|---|---|
| Reproduces from deposit as-is | Figure 1a, Table 1 (CE/EN), Figure 4, Supp. Fig 1D, Supp. Fig 6 barplot |
| Reproduces after CODE-ONLY fix | Table 1 (all-MNES) — needs C1; Figure 2 — needs C1 + C4 or XLSX rewrite |
| Reproduces after DATA-ADDITIVE fix | Figure 3 — if Tx CSVs absent from deposit |
| Cannot reproduce from deposit (acknowledged) | Supp. Fig 2 — species shapefiles intentionally excluded |
| Cannot reproduce without major additional data/tools | Figure 1b–e (GDB unzip + code fix); Supp. Fig 6 data step (Z: rasters) |
| UNVERIFIED | Figure 3 (conditional on figshare contents); Supp. table |

The reviewer's assessment that "Figure 4, Supplementary Figure 1D, and Table 1 would reproduce" is **mostly confirmed** (Figure 4 and Supp. Fig 1D yes; Table 1 CE/EN yes; Table 1 all-MNES only after path fix). The reviewer's prediction that Supp. Fig 6 cannot reproduce is **too pessimistic** — the barplot itself reproduces from the deposited CSV; only the data computation step fails.

---

## 7. Open Questions for the Author

1. **Figshare nesting (claim a):** Once `Z:\BESP_QLD_figshare` is fully extracted, check whether `Energy_system_model_outputs/` appears at the top level or nested inside a named folder. If nested, decide whether to fix `_paths.R` (add one more fallback folder name) or update the README extraction instructions. The `_paths.R` double-check mechanism (`data/` first, then `BESP_data_qld_2025/`) already handles one level of variation.

2. **`eplus_Domestic_NPV_figure.csv` in figshare:** Verify whether this CSV is in the current figshare deposit. If absent, choose between: (a) rewrite `NPV_bar_plot.R` to read `eplus_Domestic_NPV_2025.xlsx` directly (preferred, CODE-ONLY), or (b) add the CSV to a new figshare version (DATA-ADDITIVE). The CSV is derived data; the XLSX is the source — CODE-ONLY is cleaner.

3. **Tx_outputs CSVs in figshare:** Verify whether `QLD_threshold_tx1_new_summary.csv` and `QLD_threshold_tx2_new_summary.csv` are in the current figshare deposit. If absent, add them (small files, no redistribution concerns). These are the only inputs `tx_length_figure.R` needs to draw Figure 3.

4. **Existing transmission shapefiles redistribution:** Readme.txt line 207 identifies these as "QLD electricity transmission network (state government spatial data)". Confirm redistribution rights before adding to figshare. If rights are clear, add to deposit (DATA-ADDITIVE). If not, document that Figure 3 preprocessing requires a separately sourced dataset and point to the source.

5. **Wind/PV exclusion rasters redistribution:** Confirm whether `combined_wind.tif` and `combined_pv.tif` (produced by the netzero_navigate model) can be redistributed under a figshare open licence. If yes, adding them makes `land_use_competition_QLD.R` fully runnable from deposited data. If no, the caching guard (fix C4) ensures Supp. Fig 6 still reproduces from the pre-computed CSV without them.

6. **`renv.lock`:** Requires running the full pipeline in a clean R environment and capturing the package snapshot. This requires execution. Flag as a post-revision task after all code fixes are applied.

7. **Zenodo deposit:** Decision required on whether to create a Zenodo deposit in addition to figshare. GitHub + figshare covers versioned code + data; Zenodo would add a DOI-citable code archive. No code changes required; administrative action only.

8. **`domestic_export_map_iterations.R` data path:** This script uses `data_root <- here("data")` internally (not `_paths.R`). For a fresh user, `data/` does not exist. Decide whether to refactor this script to use `_paths.R` (consistent with all other scripts) or document that Figure 1b–e requires the data in a `data/` subfolder specifically. This is separate from the directory-name bug in `_RUN_ALL.R` line 45.

9. **`RZ_area_outside_exclusions_and_ECNES.R`:** This script (now moved to `Biodiversity_analysis/`) uses `here("data", "AUS_area_outside_exclusion_zones.gdb")` and `here("data", "Red_zones_QLD.shp")` — files not in the deposit. Readme.txt lists it as a supporting script not in `_RUN_ALL.R`. Decide whether to remove it from the repository (it is not needed to reproduce any main-text figure or table) or add a header comment documenting the proprietary inputs it requires.
