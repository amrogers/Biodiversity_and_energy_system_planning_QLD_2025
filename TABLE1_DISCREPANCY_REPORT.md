# Table 1 Discrepancy Report

Read-only investigation. No analysis script was executed. Two small standalone
inspection scripts (not part of the pipeline) were run, solely to read
`feature_curves.csv` and count values at five fixed rows — see Task 3.
**Revision note:** Section 1's verdict and Section 3 were corrected after a
follow-up test (below) showed the first pass tested the wrong tolerance value.
The original Section 3 test used `>= 0.9999` as a stand-in for the historic
script's floating-point tolerance; the historic script's actual threshold was
`0.99`, a full percentage point of slack, not a noise-sized one. Re-testing
with the correct value reverses the original conclusion about cause. The
original (superseded) reasoning is left in place below the corrected verdict,
struck through in spirit but not deleted, so the audit trail is visible.

## 1. Verdict (corrected)

**Both sets of numbers are computed correctly from data — they use different
definitions of "full coverage." Table 1 (408/455/488, BAU 9) uses exact
equality (`== 1.0`) at a single rank row. The manuscript body text and
abstract (421/466/496, BAU 20, and the 77% claim) match exactly what you get
applying a `>= 0.99` threshold to the same current, correct
`feature_curves.csv` — no stale data or different Zonation run is required to
reproduce them.**

Verified directly (Task 3 rerun, below): computing `sum(species_vals >= 0.99)`
on today's `feature_curves.csv`, at the same five rows `Mean_spp_scenario_coverage.R`
uses, gives **20/421/466/496/513** — an exact match to
`feature_coverage_summary_with_CI.csv` and the body text, for all five
scenarios. This is not a rounding-sized effect: it adds 8–13 extra species
per scenario, all of which have coverage genuinely between 0.99 and 1.0 (not
just outside float precision of 1.0).

So the real cause is that two scripts define "100% covered" differently:

- `Mean_spp_scenario_coverage.R` (Table 1, current pipeline): a species counts
  only if its curve value is *exactly* 1.0 at the scenario's threshold rank.
- The historic `Zonation curves.R` / `feature_coverage_summary_with_CI.csv`
  lineage: a species counts if it reaches *at least* 0.99 (99% of its
  distribution) anywhere in the scenario's rank range.

Both are legitimate analytical choices — "exactly complete" vs. "essentially
complete, ≥99%" — but they are not the same metric, and the manuscript
currently reports numbers from one definition in the body/abstract and a
different definition in Table 1 without saying so.

**This does not fully rule out the stale-run finding from the original pass**
(the actual `feature_coverage_summary_with_CI.csv` file on disk is still
provably from an older Zonation run — see Section 4). But since the disputed
numbers are now independently reproducible on the *current, undisputed* data
using the historic threshold, the run difference is no longer *necessary* to
explain them — the threshold difference is sufficient on its own. Which of
the two effects (or both) is truly what produced the specific file on disk
cannot be fully resolved without the March-2025 run's own `feature_curves.csv`,
which does not appear to exist in this repository.

**Recommendation:** this is now an editorial/methodological decision, not a
data-integrity bug — flag it to the authors as "Table 1 uses exact-coverage,
body text uses ≥99%-coverage; pick one definition and make both consistent,"
rather than "Table 1 is right, fix the text." If exact coverage is the
intended metric, correct the body text and abstract to 408/455/488, BAU 9,
and recompute the percentage from 408/545 ≈ 74.9%. If ≥99% coverage is the
intended metric, Table 1 should be regenerated with that threshold instead
of `== 1.0`, and 421/545 ≈ 77.2% would be the correct abstract percentage
(546 is still an off-by-one on the denominator either way — see Section 5).

---

## 2. Every coverage-counting script found

Repository-wide search (current tree, all `.R` files, plus full git history of
every script found) for full-coverage-counting logic:

| Script | Called by `_RUN_ALL.R`? | Input | Output | Comparison used |
|---|---|---|---|---|
| `Biodiversity_analysis/Mean_spp_scenario_coverage.R` (line 91) | **Yes** — `pipeline_step1`, "Table 1 (all MNES)" | `feature_curves.csv` (250m_QLD_2024) | `results/tables/scenario_coverage_results.csv` | `sum(species_vals == 1.0, na.rm = TRUE)` — exact equality, at a single row (nearest rank to each scenario's `rank_min`) |
| `Figure_code/Zonation curves.R`, **historic only** (commit `2a4321f`, 2025-07-19, and `feb4ef0`, 2025-07-31 — removed by commit `a413a0b`, 2026-01-14) | No — not in current tree at all | `feature_curves.csv` (originally a **Victoria** dataset, `Vic_100m_SNES_ECNES/out_example1/`) | `feature_coverage_summary_with_CI.csv` | `count_full_coverage(data, rank_min, rank_max=1.0, threshold=0.99)`: filters the **entire rank range** `[rank_min, 1.0]`, then flags a species if `max(value, na.rm=TRUE) >= 0.99` **anywhere in that range** |
| `Figure_code/Critically_endangered_mean_coverage_and_line_plot.R` | No — not wired into `_RUN_ALL.R`'s pipeline list; `_RUN_ALL.R` treats "Table 1 (CE/EN)" as pre-computed and only prints the path | `feature_curves.csv`, `species_weights.csv` | `results/tables/CE_EN_mean_coverage_results.csv` | Does not count "full coverage" species at all — computes mean/CI coverage per rank for CE (weight=5) and EN (weight=4) subsets only. Not a source of the disputed counts. |

No other script in the repository (current tree or history) computes a
full-coverage species count. `zero_coverage_species.R`,
`RZ_area_outside_exclusions_and_ECNES.R`, and `land_use_competition_QLD.R`
use `== 1` but for unrelated purposes (raster exclusion-mask cell values, not
species distribution coverage fractions).

**What wrote `feature_coverage_summary_with_CI.csv`:** the historic
`Figure_code/Zonation curves.R`, in the form it had between commits `2a4321f`
(2025-07-19) and `feb4ef0` (2025-07-31). That code was stripped from the
script by the time of commit `a413a0b` (2026-01-14) — the current
`Figure_code/Zonation curves.R` only plots curves and contains no
coverage-counting logic at all. However, the *actual* file on disk predates
even the earliest commit that contains this code (file mtime 2025-03-31,
vs. earliest commit 2025-07-19) and its own `analysis.log` points at a
different feature-set folder than the QLD 2024 run — see Task 4. This means
the version of the script that is in git history is very likely a
**generalised/cleaned-up copy** of whatever one-off script actually produced
the file on disk in March 2025; the exact script that produced this specific
file is not present in this repository.

---

## 3. Count comparison on `feature_curves.csv` (current data)

### 3a. First pass — tested `>= 0.9999` (floating-point-noise-sized tolerance)

Computed directly from `BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_curves.csv`
(mtime 2025-07-31, 546 columns = `rank` + 545 species), at the row nearest
each scenario's threshold rank:

| Scenario | Nearest actual rank | `== 1.0` | `>= 0.9999` | `round(x,4) == 1` | Strictly between 0.9999 and 1.0 |
|---|---|---|---|---|---|
| BAU (0.87) | 0.870010 | **9** | 9 | 9 | 0 |
| Top 30% (0.70) | 0.700030 | **408** | 409 | 408 | 0 |
| Top 50% (0.50) | 0.499950 | **455** | 455 | 455 | 0 |
| Top 70% (0.30) | 0.299970 | **488** | 488 | 488 | 0 |
| Top 90% (0.10) | 0.099990 | **500** | 500 | 500 | 0 |

This tolerance changes almost nothing — at most 1 species (Top 30% only).
**This test was the wrong one to draw a conclusion from**, because it does
not match the historic script's actual tolerance (`threshold = 0.99`, not
`0.9999`). It's retained here to show that noise-sized tolerances genuinely
aren't the story; a full 1%-point tolerance is a different question, tested
next.

### 3b. Second pass — the historic script's actual `threshold = 0.99`, point and whole-range

Re-run with `sum(species_vals >= 0.99)` at the same five rows, plus the count
of values strictly between 0.99 and 1.0, plus the whole-range-max version
(`max` over every row from `rank_min` to `rank_max = 1.0`, replicating the
historic script's actual method rather than a single point):

| Scenario | Nearest actual rank | `== 1.0` (point) | `>= 0.99` (point) | Between 0.99–1.0 (point) | `>= 0.99` (whole-range max) | Species w/ ≥1 decreasing step in-range |
|---|---|---|---|---|---|---|
| BAU (0.87) | 0.870010 | 9 | **20** | 11 | **20** | 524/545 |
| Top 30% (0.70) | 0.700030 | 408 | **421** | 13 | **421** | 545/545 |
| Top 50% (0.50) | 0.499950 | 455 | **466** | 11 | **466** | 545/545 |
| Top 70% (0.30) | 0.299970 | 488 | **496** | 8 | **496** | 545/545 |
| Top 90% (0.10) | 0.099990 | 500 | **513** | 13 | **513** | 545/545 |

The `>= 0.99` point column reproduces the disputed body-text/CSV numbers
exactly (20/421/466/496/513), on the current, undisputed `feature_curves.csv`.
**The comparison operator/threshold is the source of the discrepancy** — the
opposite of what the first-pass test (3a) suggested, because 3a tested too
tight a tolerance to be representative of what the historic script actually
did.

### 3c. Monotonicity

Coverage is **not** strictly monotonic in rank: across the full rank range,
545 of 545 species have at least one decreasing step (672,699 decreasing
steps total out of 545 species × 10,002 row-to-row transitions; the single
largest drop is exactly **-1.0**, a full-unit cliff that looks like a data
artifact — e.g. an NA or sentinel value — rather than genuine curve noise,
and is flagged as unverified below). Despite this, the whole-range-max count
at `threshold = 0.99` is **identical** to the single-point count at every
scenario (see table above) — the non-monotonic dips evidently aren't large
or positioned so as to change which species cross the 0.99 line within these
particular ranges. So for this specific question (99% coverage counts at
these five thresholds), point and whole-range give the same answer on this
data, even though the curves are not monotonic in general.

---

## 4. `feature_coverage_summary_with_CI.csv`

- Full path: `BESP_data_qld_2025/Zonation_analysis/Zonation_output/250m_QLD_2024/out_example1/feature_coverage_summary_with_CI.csv`
- 7 rows (6 data rows + header) x 10 columns
- Columns: `range, min_rank, max_rank, feature_count, zero_coverage, less_than_50pct, more_than_50pct, mean_value, ci_lower, ci_upper`
- mtime: **2025-03-31 15:44:07**, same day as `analysis.log` (12:08) and
  `issues.log` (12:08) in the same folder — all three are artifacts of one
  earlier Zonation run.
- Full-coverage counts per scenario: BAU (range "BAU", min_rank 0.87) = 20,
  "0.7-1.0" = 421, "0.5-1.0" = 466, "0.3-1.0" = 496, "0.1-1.0" = 513 — these
  are exactly the manuscript body-text numbers (20/421/466/496).
- Plausibly derived from the *same underlying feature_curves.csv currently on
  disk*? **No.** `analysis.log` in the same folder records the Zonation run
  that produced these numbers as having used project files under
  `...\250m_SNES_ECNES_red_zones_weighted_500spp\...`, a folder name
  independently documented elsewhere in this repo
  (`DIAGNOSTIC_REPORT.md` lines 46, 95, 197–198; also referenced in
  `minimal_settings.z5` and `features_example1.txt`) as an **older,
  superseded** feature-set configuration, distinct from the current
  `250m_QLD_2024` setup that produced today's `feature_curves.csv`
  (mtime 2025-07-31, four months later).
- Tolerance/rounding rule: the historic script that (per git history) produced
  this file's *structure* used `threshold = 0.99` and a **whole-range max**
  (any row from `rank_min` to `rank_max = 1.0`, not a single point) — a
  materially looser definition of "full coverage" than Table 1's exact-match,
  single-row definition. **Corrected finding (Section 3b):** this threshold
  choice alone, applied to the current `feature_curves.csv`, exactly
  reproduces this file's counts (20/421/466/496/513). The different-run
  finding above is still true as a fact about this specific file's
  provenance, but it is not required to explain the numbers.
- A companion `feature_coverage_summary_with_CI.xlsx` also exists in the same
  folder, mtime 2025-08-04 — later than the CSV, so likely just a manual
  Excel export/reformat of the March CSV, not a new computation. Not
  independently verified further (would require opening the file with a
  spreadsheet reader, not attempted here).

**`results/tables/scenario_coverage_results.csv`** (the current Table 1 output):
mtime **2026-08-05 13:42:42** (today, from the pipeline verification run
earlier in this session). Values: BAU 9, Top 30% 408, Top 50% 455, Top 70%
488, Top 90% 500 — full contents:

```
Protection scenario,Actual rank used,N species,Average distribution coverage,Species with full coverage,Species with no coverage,95 CI (lower),95 CI (upper)
BAU (14% of Qld),0.87001,545,0.458448623853211,9,21,0.43318489620120754,0.48371235150521447
Top 30% of Qld,0.70003,545,0.9058609174311927,408,0,0.887782759984735,0.9239390748776504
Top 50% of Qld,0.49995,545,0.9567935779816513,455,0,0.9456527527403402,0.9679344032229624
Top 70% of Qld,0.29997,545,0.9810840366972478,488,0,0.975087918021149,0.9870801553733465
Top 90% of Qld,0.09999,545,0.9951533944954128,500,0,0.9933556449264797,0.996951144064346
```

---

## 5. The denominator: 500 vs 546

Neither. Both disputed files agree on **545**, not 500 and not 546:

- `feature_curves.csv`: 546 total columns = `rank` + **545** species columns.
- `feature_coverage_summary_with_CI.csv`: `less_than_50pct + more_than_50pct`
  sums to 545 in every row (e.g. BAU: 301+244=545; "0.3-1.0": 0+545=545) —
  545 total features, consistent with `feature_curves.csv`.
- `Readme.txt` line 59 independently documents "545 conservation features."

The abstract's "546" appears to be a simple off-by-one, not evidence of a
500-species CE/EN subset issue. The "Table 1 CE/EN" step in `_RUN_ALL.R`
(`Critically_endangered_mean_coverage_and_line_plot.R`) is a separate,
smaller analysis restricted to weight=4/5 (EN/CE) species only — it does not
feed into, or explain, the all-MNES denominator. Using the correct
denominator does not materially change the headline: 421/546 ≈ 77.1% vs.
421/545 ≈ 77.2% are indistinguishable at the reported precision. If Table 1's
408 is adopted as correct, the honest recomputation is 408/545 ≈ 74.9%, which
**does** materially change the abstract's "77%" claim.

---

## 6. Scenario thresholds

The four non-BAU thresholds are consistent everywhere they appear:
`Mean_spp_scenario_coverage.R` (0.70/0.50/0.30/0.10), `Biodiversity_value_map.R`
(same boundaries in its reclassification matrix, lines 110–117), and the
historic `Zonation curves.R` (`2a4321f`, same `rank_ranges`) all agree.

Two inconsistencies found:

1. **`Critically_endangered_mean_coverage_and_line_plot.R` uses 0.84, not
   0.87**, for its topmost threshold (`target_ranks <- c(0.10, 0.30, 0.50,
   0.70, 0.84)`, line 40) — every other script in the repo that defines this
   boundary uses 0.87. This doesn't affect the all-MNES Table 1 discrepancy
   (different script, different metric), but it means the CE/EN table's
   tightest scenario is not the same rank cutoff as the all-MNES table's BAU
   row, despite both plausibly being read as "the BAU scenario" by a reader.
2. **The "14% of Qld" label is arithmetically inconsistent with its own
   `rank_min = 0.87`.** `1 − 0.87 = 0.13` = 13%, not 14%. The other four
   labels are all exactly consistent (`Top 30%` ↔ 0.70, `Top 50%` ↔ 0.50,
   `Top 70%` ↔ 0.30, `Top 90%` ↔ 0.10 all satisfy `1 − rank_min` = the
   labeled percentage exactly). Only the BAU label is off by one percentage
   point.

Both are minor, cosmetic, and independent of the main 408-vs-421 discrepancy.

---

## Open questions (cannot be resolved without executing something)

- **Which effect actually produced `feature_coverage_summary_with_CI.csv`: the
  0.99 threshold, the older run, or both?** Both independently reproduce
  numbers consistent with what's on disk (Section 3b shows 0.99-on-current-data
  matches exactly; Section 4 shows the file's own `analysis.log` points to a
  different, older run). Disentangling them would need the March-2025 run's
  own `feature_curves.csv`, which isn't in this repository — only its
  aggregated summary survived. Practically this may not matter: regardless of
  which explanation is literally true for this one file, the manuscript's
  actual problem is that Table 1 and the body text use different coverage
  definitions, and that's confirmed directly.
- The single **-1.0** monotonicity anomaly (Section 3c) — worth checking
  whether that's a real Zonation artifact or a data-loading quirk (e.g. an
  `NA` read as a numeric sentinel) before relying on curve smoothness
  elsewhere; not chased further here since it didn't affect the counts in
  question.
- Whether the March 2025 `250m_SNES_ECNES_red_zones_weighted_500spp` run or
  the July 2025 `250m_QLD_2024` run reflects the biodiversity feature set the
  authors actually intended for the final manuscript — this is a modelling
  decision, not something inferable from file inspection. The timestamp and
  naming evidence strongly suggests July 2025 is the intended final run (it's
  the one the current, actively-maintained pipeline reads), but confirming
  that requires the authors' knowledge of the analysis history, not code
  inspection.
- Whether `feature_coverage_summary_with_CI.xlsx` (Aug 2025) contains any
  different numbers than the March 2025 CSV — not opened, since inspecting a
  binary `.xlsx` was outside what could be done with plain-text reads; would
  need to actually load it.
- Whether any copy of the manuscript draft, cover letter, or reviewer
  correspondence explicitly cites `feature_coverage_summary_with_CI.csv` as
  the source for the body text (the prompt states a reviewer claims this) —
  not in this repository, so unverifiable from here.
