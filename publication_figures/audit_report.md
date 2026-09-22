# Publication Figure Audit — Phase 1 (Static Inspection)

Method: read-only. No R code was sourced or executed. Vector/bitmap classification
follows the correction on Figure 1: **data source** (sf/vector vs terra/raster)
determines the classification, not the file the pipeline currently happens to write.
Journal context (from `review_instructions.md`): manuscript under revision at
*Nature Ecology & Evolution* — flagged assumptions below should be checked against
that journal's current figure guidelines before Phase 2 overrides are finalised.

Scripts audited are the ones actually wired into `_RUN_ALL.R`'s `pipeline_step1`.
Three of the four `Figure_code/` scripts in scope have newer, unwired sibling files
(`*_katherine_edit.R`) — see [Open items](#open-items-for-you), item 5.

---

## Summary table

| Figure/panel | Vector or bitmap (evidence) | Stated specs from code | Compliance gap | Fix complexity |
|---|---|---|---|---|
| **1a** (R rendition) | **Bitmap**, correctly — data is a `terra` raster (`rank_reclass`, continuous grid reclassified to 5 classes, `rast()`/`classify()`) rendered via `geom_tile()`. `Biodiversity_analysis/Biodiversity_value_map.R:107,159,197-198` | `ggsave(width=7, height=8, dpi=300, bg="white")` — units default to **inches**, no `device=`. No `family=` in `theme_void()`. Colours: manual hex map, no colour-space stated. L228 `print(p)`, plot object `p` assigned (capturable). | No mm dimensions; no explicit font family (defaults to system sans, likely not the house font); no stated RGB/CMYK colour profile. | **Low** — object is a plain variable; add `units="mm"`, set `family=` in theme, confirm hex colours render as RGB. |
| **1b–e** (VRE siting maps) | **Bitmap file, but source data is vector** — `sf` polygons from shapefiles (`st_read`, `geom_sf`) rasterised straight to PNG. `Energy system and transmission analysis/domestic_export_map_iterations.R:266,278,296-301,342-352`. Under the Fig-1 correction this is a **vector-eligible chart currently exported only as bitmap**. | `ggsave(width=15, height=15, units="in", dpi=300, bg="white")` → 4500×4500 px, matches `_RUN_ALL.R` header comment. No `device=`, no `family=`. Title/subtitle/caption (`labs(title=…, subtitle=…, caption=…)`, sizes 16/12) are baked into the image. | (1) Vector source rendered bitmap-only — no PDF/EPS alternative exists. (2) Baked-in title/subtitle/caption — journals normally want these in the caption text, not on the figure. (3) `create_plot()` returns `NULL` (not the plot object) whenever the target PNG already exists (`domestic_export_map_iterations.R:258-261`) — **the ggplot object is never returned when cached**, so it cannot be captured/recaptured as-is. (4) Named R colours `"lightblue"`/`"blue"` alongside hex — inconsistent colour definition, no confirmed RGB values. | **High** — `energy_maps_figure.R` (the script actually sourced by `_RUN_ALL.R`) does not build a plot at all, it only verifies file presence (see detail section). Phase 2 will need to reuse `domestic_export_map_iterations.R`'s data-loading/`geom_sf` logic in new plotting code per the audit instructions, not source-and-capture the original. |
| **2** (NPV) | **Vector chart data** — CSV-derived stacked-rect chart (`geom_rect_pattern`), no spatial component. `Figure_code/NPV_bar_plot.R:53-103,143-198`. Currently exported bitmap-only. | `ggsave(width=180, height=90, units="mm", dpi=300, bg="white")` — **mm already used** ✅. No `device=` (raster PNG only). No `family=` in `theme_minimal(base_size=12)`. Object `final_plot` assigned (`L143`), capturable. | Chart data is vector-eligible but only a raster PNG is produced — no vector device (`cairo_pdf`/`svglite`) alternative. No font family set. | **Low** — object already assigned to a variable; add a vector-device `ggsave()` call alongside/instead of the PNG one. |
| **3** (TX length) | **Vector chart data** — CSV-derived stacked bar chart (`geom_col`), no spatial component. `Figure_code/tx_length_figure.R:125-133,290-345`. Currently exported bitmap-only. | `ggsave(width=10, height=6, dpi=300, bg="white")` — **units default to inches**, not mm (inconsistent with Figure 2). `theme_minimal(base_size=12, base_family=plot_font)` where `plot_font` is `"Arial"` only if `extrafont::fonts()` already has it registered, else silently falls back to `""` (default device font) — `L273,334`. Object `p` assigned (`L290`), capturable. | No mm dimensions; font choice is conditional/fragile (silent fallback changes the rendered font without erroring); no vector device. | **Low–Medium** — object assigned and capturable; needs mm conversion, a vector-device export, and a guaranteed (not conditional) font. |
| **4** (cost increase) | **Vector chart data** — CSV-derived line/point chart (`geom_line`/`geom_point`), no spatial component. `Figure_code/percent cost increase_line plot.R:66-94`. Currently exported bitmap-only. | `ggsave(width=12, height=6, dpi=300, bg="white")` — units default to inches, not mm. No `family=`. `axis.title`/`axis.text` sizes hard-coded to 18/16 (`L91-92`) — noticeably larger than Figures 2/3's `base_size=12`. Colour: `scale_color_brewer(palette="Set1")` (`L79-83`) — **not** the colour-blind-safe hex palette used in Figs 1a/2/3. Object `cost_plot` assigned (`L66`), capturable. | No mm dimensions; no vector device; inconsistent type scale vs. other figures; `Set1` is not colour-blind safe and breaks the otherwise-consistent custom palette used elsewhere in the manuscript. | **Low–Medium** — object assigned and capturable; needs mm conversion, vector-device export, type-scale reconciliation with Figs 2–3, and a palette swap for consistency/accessibility. |

---

## Detail sections

### Figure 1a — Biodiversity prioritisation map (R rendition)

- **Script:** `Biodiversity_analysis/Biodiversity_value_map.R` (sourced directly by `_RUN_ALL.R`, line 55).
- **Data source:** `terra::rast()` loads `rankmap.tif` (continuous Zonation output), reclassified into 5 discrete categories with `classify()`, then converted to a data frame and drawn with `geom_tile()` (lines 107–184). This is a **raster/continuous-grid source** → bitmap classification is correct here (unlike 1b–e).
- **Plot object:** assigned to `p` (line 197), printed (`print(p)`, line 228) before `ggsave`. Fully capturable via `last_plot()` or the `p` variable.
- **ggsave call** (line 233): `ggsave(final_plot_path, plot = p, width = 7, height = 8, dpi = 300, bg = "white")`
  - Units default to inches (7×8in = 2100×2400px at 300dpi).
  - No `device=` argument — inferred from the `.png` extension in `final_plot_path`.
  - No `units = "mm"`.
- **Theme/font:** `theme_void(base_size = 11)`; no `element_text(family = ...)` anywhere — font is whatever the default graphics device supplies (typically the system sans-serif, e.g. Arial on Windows via the `png()` device, but this is not pinned in code).
- **Colour scale:** `manual_colour_map`, 5 named hex values (lines 42-48), applied via `scale_fill_manual()`. Hex values are well-defined; no explicit colour-space (RGB/CMYK) statement anywhere, but hex + PNG defaults to RGB.
- **Existing output on disk:** `results/zonation_figures/zonation_rankmap.png`, 420,090 bytes, last modified 2026-08-11. Pixel dimensions/DPI/colour mode of the rendered file **cannot be confirmed from a listing alone** — flagged as an open item below.
- Also on disk: `results/figures/Figure 1_protection scenario map and 2050 VRE mapping.tif` (321,452 bytes, modified 2026-04-02) — this is the **ArcGIS Pro-produced manuscript figure** (per `_RUN_ALL.R` lines 76-81), not an R output, and is explicitly noted in the pipeline as "not reproducible in R." Same caveat: pixel dims/DPI/colour mode not confirmable from a filesystem listing.

### Figure 1b–e — VRE siting maps

Two scripts are involved and it's important to distinguish them:

1. **`Figure_code/energy_maps_figure.R`** — this is what `_RUN_ALL.R` actually sources (line 56). It does **not build a plot**. It only checks that the 6 expected PNGs (`domestic_layer_map_{0,10,30,50,70,90}_2050.png`) exist on disk (in `paths$tx_outputs/domestic_maps_tx1/` or the `results/` fallback), prints their paths, and `stop()`s if any are missing. There is no ggplot object here at all — nothing to capture.
2. **`Energy system and transmission analysis/domestic_export_map_iterations.R`** — this is the script that actually produces the PNGs (only run if `regenerate_fig1 <- TRUE`, a 30+ minute rebuild from GDBs). This is where the real plotting logic lives:
   - **Data source:** `sf` polygons (`st_read()` on shapefiles derived from the GDBs, `geom_sf()`, lines 279-301) — **vector data**, not raster. Per the Figure-1 correction this makes 1b–e vector-eligible in principle, even though the only output is a rasterised PNG.
   - **Plot construction:** `create_plot()` (lines 242-317) builds and *returns* a ggplot object — but only when the target PNG does not already exist (lines 258-261 return `NULL` early if `file.exists(png_path)`). Since the 6 maps already exist on disk, running this script today would return `NULL` for all of them and produce nothing to capture. **Phase 2 will need to adapt the data-loading + `geom_sf` logic into new code**, per the audit brief's instruction for scripts with no capturable object — it cannot rely on source-and-recapture as written.
   - **ggsave call** (lines 342-343, 351-352): `ggsave(plot, filename = out_path, width = 15, height = 15, units = "in", dpi = 300, bg = "white")` → 4500×4500px @ 300dpi, matching the `_RUN_ALL.R` header comment exactly.
   - **Theme/font:** `theme_minimal()`, `plot.title = element_text(size = 16, face = "bold")`, `plot.subtitle = element_text(size = 12)` — no `family=` set anywhere.
   - **Baked-in text:** `labs(title=, subtitle=, caption=)` (lines 305-310) puts the panel title, threshold/year subtitle, and a data-source caption directly on the image. If the manuscript figure caption already conveys this, the on-image text is redundant and non-standard for a journal figure panel.
   - **Colour scale:** `tech_colors` (lines 289-294) mixes hex (`"#FFA500"`, `"#808080"`) with named R colours (`"lightblue"`, `"blue"`) — inconsistent definition style, and named colours don't have an explicit RGB value pinned in the script.
- **Existing outputs on disk** (`results/figures/energy_maps/domestic_maps_tx1/`): all 6 PNGs present, ~498–550 KB each, dated 2024-12-17 (note: predates every other figure output in this audit — these have not been regenerated since). Pixel dimensions/DPI/colour mode not confirmable from a listing — flagged below. A second copy of the same directory structure exists under `BESP_data_qld_2025/Energy_system_model_outputs/.../Tx_outputs/domestic_maps_tx1/` (the primary path `energy_maps_figure.R` checks first); I did not diff file contents between the two locations — flagged below.

### Figure 2 — NPV bar chart

- **Script:** `Figure_code/NPV_bar_plot.R` (sourced by `_RUN_ALL.R`, line 58).
- **Data source:** `eplus_Domestic_NPV_figure.csv` (confirmed present at `BESP_data_qld_2025/Energy_system_model_outputs/`) — plain tabular data, no spatial component. Plotted as a stacked-rectangle chart (`geom_rect_pattern`, `ggpattern` package) — **vector-eligible**.
- **Plot object:** assigned to `final_plot` (line 143), printed at line 211. Capturable directly.
- **ggsave call** (line 205): `ggsave(output_filename, plot = final_plot, width = 180, height = 90, units = "mm", dpi = 300, bg = "white")`
  - **Already in mm** — this is the one figure in scope that states its dimensions in mm as-is.
  - No `device=` — PNG only, no vector alternative.
- **Theme/font:** `theme_minimal(base_size = 12)`; no `family=` set.
- **Colour scale:** reuses the same 5-colour hex palette as Figure 1a (`threshold_colours`, lines 124-130) — consistent across figures. A commented-out `RColorBrewer::brewer.pal(5, "PuOr")` alternative is present but inactive (lines 132-135).
- **Existing output:** `results/figures/npv_analysis_plot.png`, 138,036 bytes, modified 2026-08-05.

### Figure 3 — Transmission build length

- **Script:** `Figure_code/tx_length_figure.R` (sourced by `_RUN_ALL.R`, line 59). Note: this is the newest file in `Figure_code/` (2026-09-16), superseding two older `*_katherine_edit.R` variants (2026-03-31, 2026-04-10) — see open item 5.
- **Data source:** two pre-computed CSVs (`paths$tx1_new_summary`, `paths$tx2_new_summary`) plus `paths$existing_tx_csv` — plain tabular data, no spatial component. Stacked bar chart (`geom_col`) — **vector-eligible**.
- **Plot object:** assigned to `p` (line 290), printed at line 358. Capturable directly.
- **ggsave call** (line 352): `ggsave(out_plot, plot = p, width = 10, height = 6, dpi = 300, bg = "white")`
  - Units default to inches — **not mm**, inconsistent with Figure 2.
  - No `device=` — PNG only.
- **Theme/font:** `theme_minimal(base_size = 12, base_family = plot_font)` where `plot_font <- if ("Arial" %in% fonts()) "Arial" else ""` (line 273). This is a **conditional, silent fallback** — if Arial isn't registered with `extrafont` on the machine that runs Phase 2, the figure renders in the device default font with no warning beyond a `message()` at script start (lines 48-56).
- **Colour scale:** hex palette matching Figures 1a/2 for the biodiversity-scenario fill (`NEWBUILD_COLOURS`, lines 103-109), plus `"grey70"` for the existing-network segment — consistent style.
- **Existing output:** `results/figures/tx_length_figure.png`, 175,442 bytes, modified 2026-08-18 (predates the current script's 2026-09-16 modification — **the on-disk PNG may not reflect the current script version**, flagged below).

### Figure 4 — Energy cost increase

- **Script:** `Figure_code/percent cost increase_line plot.R` (sourced by `_RUN_ALL.R`, line 60). A newer, unwired sibling `percent cost increase_line plot_katherine_edit.R` exists (2026-04-10) — see open item 5.
- **Data source:** `cost_increase_results.csv` (confirmed present) — plain tabular data, no spatial component. Line/point chart (`geom_line`/`geom_point`) — **vector-eligible**.
- **Plot object:** assigned to `cost_plot` (line 66), printed at line 107. Capturable directly.
- **ggsave call** (line 101): `ggsave(output_file, plot = cost_plot, width = 12, height = 6, dpi = 300, bg = "white")`
  - Units default to inches — not mm.
  - No `device=` — PNG only.
- **Theme/font:** `theme_minimal()` with `axis.title = element_text(size = 18)`, `axis.text = element_text(size = 16)` (lines 90-93) — no `family=` set, and these sizes are markedly larger than the `base_size = 12` used in Figures 2 and 3.
- **Colour scale:** `scale_color_brewer(palette = "Set1")` (lines 79-83) — a stock ColorBrewer qualitative palette, **not** the custom colour-blind-safe hex palette (built with the `colorBlindness` package, per Figure 1a) used consistently in Figures 1a/2/3. `Set1`'s red/green pair is a known colour-vision-deficiency risk.
- **Existing output:** `results/figures/energy_cost_increase_plot.png`, 232,772 bytes, modified 2026-03-25.

---

## Open items for you

1. **Pixel dimensions / DPI / colour mode of already-rendered images** — cannot be confirmed from filesystem listings alone. Affects:
   - `results/zonation_figures/zonation_rankmap.png` (Fig 1a)
   - `results/figures/Figure 1_protection scenario map and 2050 VRE mapping.tif` (Fig 1a, ArcGIS Pro manuscript version)
   - the 6 files in `results/figures/energy_maps/domestic_maps_tx1/` (Fig 1b–e)
   - `results/figures/npv_analysis_plot.png` (Fig 2)
   - `results/figures/tx_length_figure.png` (Fig 3)
   - `results/figures/energy_cost_increase_plot.png` (Fig 4)

   I have `file` available in this environment (metadata-only, no code execution) but have **not** run it — the instructions ask you to explicitly confirm you're fine with that before I use it. Say the word and I'll run it and fold the results back into this table; otherwise these stay open for you to check in Positron.

2. **Two on-disk PNGs may be stale relative to their generating script:**
   - `tx_length_figure.png` (Fig 3, disk: 2026-08-18) vs. `tx_length_figure.R` (2026-09-16 — newer).
   - The 6 Fig 1b–e maps (disk: 2024-12-17) are far older than every other output in this audit and were not regenerated when `energy_maps_figure.R` last ran (that script only verifies presence).
   Worth confirming these still match what's in the manuscript before Phase 2 treats them as ground truth.

3. **Duplicate Fig 1b–e map locations** — `energy_maps_figure.R` checks `paths$tx_outputs/domestic_maps_tx1/` first, falling back to `results/figures/energy_maps/domestic_maps_tx1/`. Both exist on disk. I did not diff their contents (would require reading binary file contents, out of scope for a static-only audit) — worth a manual check that they're identical before picking one as canonical for Phase 2.

4. **Target compliance spec** — this report assumes the *Nature Ecology & Evolution* house style implied by `review_instructions.md` (mm dimensions, RGB, 300dpi bitmap / vector chart, a pinned font). I don't have the journal's current figure-preparation guidelines in this repo to confirm exact values (point sizes, exact column widths in mm, permitted font list). Worth pulling those up before Phase 2 locks in overrides.

5. **Unwired newer script variants** — `Figure_code/` contains `NPV_bar_plot_katherine_edit.R` (2026-04-10, newer than the wired `NPV_bar_plot.R` from 2026-04-02) and `percent cost increase_line plot_katherine_edit.R` (2026-04-10, newer than the wired 2026-03-31 version), neither referenced by `_RUN_ALL.R`. I audited only the wired versions since those are what the pipeline actually runs, but flagging in case one of the edited variants is actually the intended source for the manuscript figure and the pipeline just hasn't been repointed at it yet.

6. **OneDrive sync** — no files needed for this audit were unavailable or still syncing; everything referenced above was readable. Nothing to come back to on that front.
