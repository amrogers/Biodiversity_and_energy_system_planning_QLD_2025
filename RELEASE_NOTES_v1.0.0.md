# v1.0.0

Code release accompanying the manuscript **"Joint infrastructure and biodiversity optimisation reveals favourable cost-protection trade-offs in a carefully planned renewable energy transition"** (Rogers et al., under revision at *Nature Ecology & Evolution*).

This release pins the exact version of the analysis code used to generate the manuscript's figures and tables. The corresponding input data and pre-computed results are hosted separately on Figshare: https://doi.org/10.26188/29604590 (see `README.md` for how the two are placed together).

## Reproducibility work in this release

This version follows a complete pre-upload reproducibility audit of the repository, checking every script against the code and data as they actually exist rather than as documented. Highlights:

- **Every script reachable from the master pipeline (`_RUN_ALL.R`) was traced end-to-end** — inputs, outputs, and orchestrator wiring — and cross-checked against what actually ships in the data deposit.
- **A previously incomplete supplementary analysis was recovered and fixed.** The script comparing renewable energy siting overlap between the two transmission-cost scenarios (Supplementary Table 2) had been left as a non-functional placeholder in an earlier revision; a working copy was located, adapted to the reproducible pipeline's conventions, and is now wired into `_RUN_ALL.R`.
- **A silent naming collision was resolved.** Two similarly named scripts both generated "Figure 3" to the same output path — sourcing the wrong one would have silently overwritten the correct figure with a different chart. Resolved and documented.
- **All documentation was reconciled against the actual code and data**, correcting stale folder names, incorrect script references, inconsistent package/feature counts, and typos across `README.md` and `Readme.txt`.
- **A formal software environment record was added.** `renv.lock` pins the exact version of R (4.4.2) and every package used anywhere in the repository, generated via `renv::snapshot()` against the verified working environment.
- **A privacy sweep of the data deposit** identified and removed personal file-path information that had been inadvertently embedded in several output files during the original analysis (including a script-level fix so the issue cannot recur on a future run).

## Verified clean-room run

The full pipeline was executed from a **fresh, independent clone** of this exact commit — not the working copy it was developed in — to confirm it reproduces the manuscript's results without any hidden local dependencies:

- **11 of 11 pipeline steps completed successfully**, zero errors.
- **Table 1 (species coverage by protection scenario) confirmed exact**: 20 / 421 / 466 / 496 / 513 species at ≥99% coverage across the BAU, Top 30%, 50%, 70%, and 90% scenarios.
- All 18 data paths declared in `_paths.R` resolve correctly against the deposited data.
- Runtime: ~4 minutes for a full recompute from an empty `results/` directory; under a minute if `results/` is left pre-populated as shipped.

## Getting started

See `README.md` (or `Readme.txt` for a plain-text version) for full setup instructions, including where to place the two Figshare downloads and how to run the pipeline via `source("_RUN_ALL.R")`.

To reproduce the exact package environment this release was verified against:
```r
install.packages("renv")
renv::restore()
```

## Full audit trail

A complete record of the reproducibility audit — including every issue found, how it was fixed, and what remains outstanding — is in `FINAL_AUDIT_REPORT.md` at the repository root.
