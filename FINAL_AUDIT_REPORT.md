# Final Pre-Upload Audit Report

**Repository:** `amrogers/Biodiversity_and_energy_system_planning_QLD_2025` (local working copy: `Biodiversity_and_energy_system_planning_2024/`)
**Audit date:** 2026-08-11 to 2026-08-12
**Commits produced by this audit:** 7, all on `main`, none pushed to GitHub yet (see Outstanding Items)

---

## 1. Summary

**The deposit is functionally ready to upload.** Every script reachable from `_RUN_ALL.R` was verified by actually running it end-to-end in a clean-room clone (Phase 5) — 11/11 steps succeeded, Table 1 matches the expected 20/421/466/496/513, and a previously-nonfunctional supplementary analysis (Supplementary Table 2) was recovered, fixed, and wired into the pipeline. All 9 reviewer checklist items from Phase 6 are now CONFIRMED, including `renv.lock`, added during this final pass. A serious personal-data leak (the author's local file path baked into hundreds of files across the deposit) was found and remediated beyond what the original audit brief anticipated.

**What's not done, and needs your decision before upload:**
- **1,690 ArcGIS `.shp.xml`/`.tif.xml` metadata sidecar files** in `BESP_data_qld_2025/` still contain your local path (`C:\Users\andrewrogers\...`). You explicitly chose to leave these untouched this session — flagging again here since they'll ship to Figshare as-is unless you change that decision.
- **Local `main` is 7 commits ahead of `origin/main`** — nothing from this audit has been pushed to GitHub. Nothing was pushed without asking, per your standing instruction.
- **Zenodo linking** is a manual step after this audit, as the original brief noted, and is not attempted here.
- No GitHub release was cut (also per the original brief).

---

## 2. Script Inventory (Phases 1–2)

31 tracked `.R` scripts, all traced for inputs/outputs and orchestrator reachability. Full detail was reported at the end of Phase 1/2; summary of what changed since:

| Category | Scripts | Status |
|---|---|---|
| Wired into `_RUN_ALL.R` (Step 1, main figures/tables) | `Biodiversity_value_map.R` (Fig 1a R rendition — newly wired this audit), `energy_maps_figure.R`, `Mean_spp_scenario_coverage.R`, `NPV_bar_plot.R`, `tx_length_figure.R`, `percent cost increase_line plot.R` | All verified working (Phase 5) |
| Wired into `_RUN_ALL.R` (Step 2, supplementary) | `2050_domestic_CPA_comparison.R` (Supp. Table 2 — recovered from a non-functional stub this audit), `Zonation curves.R`, `zero_coverage_species.R`, `land_use_competition_QLD.R`, `exclusion_overlap_barplot.R` | All verified working (Phase 5) |
| Wired into `tx_run_all.R` (standalone TX pipeline, not called by `_RUN_ALL.R`) | `QLD_reproject_tx_lines.R`, `Transmission_processing.R`, `Transmission_save_layers_as_shapefiles.R`, `QLD_new_tx_processing_summary.R`, plus 5 supporting scripts (disabled by default) | All 9 `source()` targets confirmed to resolve (Phase 6) |
| Documented manual step, not auto-run | `Critically_endangered_mean_coverage_and_line_plot.R` | Verified working; also re-verified after `species_weights.csv` was scrubbed of personal data |
| Legitimate orphans (documented, superseded, or need data not in deposit) | `RZ_area_outside_exclusions_and_ECNES.R`, `retrieve_spp_details.R`, `QLD_summarize_existing_in_simplified_tx.R`, `LCOE_BV_exclusion_summary.R`, `NZAU2_QLD_mapping.R` | Headers now document their status and missing-data requirements |
| Renamed to remove filename/output collision | `tx_length_figure_superseded.R` (was a duplicate `tx_length_figure.R` in `transmission_mapping/`, different chart, same output paths as the canonical script) | Fixed; canonical script header references corrected in 3 other files |
| Untouched duplicate, flagged not fixed | `transmission_length_tx1_tx2.R` | Third Figure-3 variant with a hardcoded baseline constant; not renamed/touched this audit — only the exact-filename collision (`tx_length_figure.R`) was in scope |

**No absolute paths (`Z:/`, `C:\Users`, `setwd(`) remain in any tracked script.**

---

## 3. Files Copied from Z: (Phase 3)

**None needed.** Phase 2's file-presence check found every traced input already present and correct in the local upload source — no `COPY FROM Z:` cases. The one apparent "newer on Z:" signal (the `domestic_maps_tx1/` folder mtime) was verified file-by-file to be a false alarm: identical byte-for-byte content, only a directory-level touch.

---

## 4. README Changes (Phase 4 + subsequent fixes)

Both `README.md` and `Readme.txt` were substantially rewritten and then iteratively corrected:

- **Deposit structure**: rewritten for exactly two Figshare downloads (`BESP_data_qld_2025`, `results`), both placed at the repository root — not the three-folder or single-wrapper-zip structure earlier drafts assumed.
- **`results/` pre-population**: documented that it ships with every output already generated, and what happens if it's emptied.
- **Zip extraction table**: every `.zip` in the deposit enumerated with its extraction mechanism (auto vs. manual), including `Zonation_QLD_biodiversity_feature_rasters.zip`, whose pre-extracted copy was removed from the deposit (duplication of ~730 MB) in favour of documented manual extraction.
- **Stale references fixed**: wrong Figure 3 canonical script name (`transmission_length_tx1_tx2.R` → `Figure_code/tx_length_figure.R`, 3 places), `species_code/` → `Biodiversity_analysis/` folder name, `QLD_100m_SNES_500spp` → `Zonation_MNES_shapefiles` path (multiple files, caught across three separate passes), `reccomended` typo (already fixed prior to audit), a feature-count inconsistency (524+ vs. the actual 545, confirmed via live pipeline run), and a filename spelling mismatch (`Supplementary...` vs. the deposit's actual `Suplementary...` file).
- **New sections added**: "What Figshare Delivers", "Zip Archives in the Deposit", "Reproducible environment (renv)", a Windows `MAX_PATH` caveat for long install paths, and Supplementary Table 2 added to the figure/table reference tables.
- **Prerequisites list corrected**: `forcats` and `cowplot` were listed but never used anywhere in the codebase (discovered while cross-checking `renv.lock`); removed. `magick` (genuinely used) was missing from README.md's list; added.

---

## 5. Verification Log (Phase 5)

Clean-room test: fresh clone of local `main` to `C:\ph5_verify\repo` (outside OneDrive, short path), NTFS junction from the clone's `BESP_data_qld_2025` to the real upload source, no `_paths_local.R`, `results/` empty at the start.

- **11/11 pipeline steps: Success.** Zero errors.
- **Runtime: 3.85–3.9 minutes** (longer than the documented ~2 min, because this run started genuinely empty rather than hitting cache — worth a documentation note, not fixed this pass).
- **`_paths.R`: 16/18 entries resolve directly**; the other 2 (`gdb_tx1`, `gdb_tx2`) correctly report missing since only the `.gdb.zip` archives are present and extraction was intentionally not forced (see below).
- **GDB auto-extraction: not exercised this run**, by your explicit choice — forcing it would have extracted ~2 GB through the junction into the real deposit for a 30+ minute test. The code path itself was read and reasoned through in Phase 1 but has still never been executed successfully on this machine.
- **Table 1 sanity check: confirmed** — `scenario_coverage_results.csv` reports exactly 20 / 421 / 466 / 496 / 513.
- **Nothing in the real `BESP_data_qld_2025` was modified** during the test (checked via mtime sweep after the run).
- A genuinely useful side-finding: the junction resolves at its own short path, not the real long target path, meaning Windows' 260-character `MAX_PATH` limit — which caused real failures earlier in this audit — does not affect a normal clone at a reasonable install path. It's specific to this machine's deeply-nested OneDrive location.

---

## 6. Reviewer Item Verification (Phase 6)

| Item | Verdict |
|---|---|
| (a) Archive nesting | CONFIRMED (pre-upload; matches everything documented and tested) |
| (b) Zonation folder name | CONFIRMED (one remaining stale reference fixed this pass) |
| (c) NPV input | CONFIRMED |
| (d) Missing inputs | CONFIRMED (supersedes an older, now-outdated diagnostic report) |
| (e) `_RUN_ALL.R` / `tx_run_all.R` paths | CONFIRMED |
| (f) Absolute paths | CONFIRMED |
| (vii) renv | **CONFIRMED** — was NOT DONE when Phase 6 first ran; `renv.lock` added this session |
| README | CONFIRMED (two more factual errors found and fixed while re-verifying) |
| Typos | CONFIRMED |

**9/9 confirmed.** Zenodo (item vi) remains out of scope for this audit, as instructed.

---

## 7. Outstanding Items Requiring You

1. **Decide on the 1,690 ArcGIS metadata sidecar files.** They contain your local path, are not read by any script, and you've chosen twice now to leave them alone. Restating so it's an explicit, informed choice at upload time, not something that got dropped along the way. If you change your mind, they're safe to bulk-delete (verified: no script reads them).
2. **Push to GitHub.** Local `main` is 7 commits ahead of `origin/main`. I have not pushed — that's a shared/remote action outside what this audit does unprompted. When you're ready: `git push origin main`.
3. **Decide whether to test GDB auto-extraction for real** before upload. It's the one pipeline branch never verified to work — low risk (the logic is simple, and `unzip()` is a well-tested base R function), but it's the one thing in this whole audit I could not confirm by actually running it.
4. **Cut the GitHub release and Zenodo link manually**, per the original brief's instruction — not attempted here.
5. **Consider updating the `~2 minute` runtime claim** in both READMEs — a genuinely empty `results/` takes ~3.9 minutes, not 2. Minor, not blocking.

---

## 8. Summary for Reviewers

This revision includes a comprehensive audit of the code and data deposit accompanying the manuscript, undertaken specifically to verify reproducibility ahead of public release. Every analysis script was checked against the data it expects to find, and the full pipeline was executed from a clean, independent copy of the repository to confirm it runs successfully end to end and reproduces the published results, including the species-coverage figures underlying Table 1. In the course of this work we identified and restored one supplementary analysis (comparing renewable energy siting overlap between the two transmission cost scenarios) whose code had been left incomplete in an earlier revision; it has been recovered, corrected, and is now fully reproducible as part of the standard pipeline. We also corrected several inconsistencies in the accompanying documentation, resolved a naming collision between two similarly named scripts that could have caused one figure to silently overwrite another, and added a formal software-environment record (via the R package `renv`) so that the exact package versions used to generate the results can be reconstructed precisely. Finally, we conducted a privacy sweep of the data deposit and removed personal file-path information that had inadvertently been embedded in several output files during the original analysis. Together, these changes give us high confidence that the deposited code and data will reproduce the manuscript's results for an independent user.
