# Analysis script template — manifest

Drop these scripts into a project's `R/` folder and `source()` only the ones you
need (delete the rest). Functions are documented with light roxygen (title +
`@param`, plus `@return`/`@export` where useful) so any subset can later be
promoted into a package without rework.

## Source order

`format_utils.R` is the shared helper. If you keep **any** of `model_eval.R`,
`marker_selection.R`, or `correlations.R`, you must also keep `format_utils.R`.
Source it first:

```r
source("R/format_utils.R")   # keep whenever a dependent script is kept
source("R/model_eval.R")     # needs format_utils.R
source("R/marker_selection.R") # needs format_utils.R AND model_eval.R
```

## Files

| File | Purpose | Depends on |
|---|---|---|
| `format_utils.R` | **Global helper.** Number/p-value formatting, name tidying, `htest` → data frame, skewness/winsorising/auto-transform. | — |
| `model_eval.R` | Binomial-GLM evaluation: ROC/AUC, performance tables, optimism (.632/.632+), DHARMa diagnostics, NRI, confusion matrix, all-possible-subsets, standardised coefficients, paired diagnostic comparisons. | `format_utils.R` |
| `marker_selection.R` | Elastic-net biomarker selection (MI + LOO) and the Val-MI bootstrap internal-validation strategy. | `format_utils.R`, `model_eval.R` (uses `fastAUC`) |
| `correlations.R` | Correlation estimation with CIs, GEE correlated performance, paired diagnostic comparison, Efron optimism-adjusted kappa. | `format_utils.R` |
| `survival.R` | Pre-computed OS/PFS/RFS/age and a Kaplan-Meier summary table (log-log CIs). | optional: `data_import.R` date parser (has a built-in fallback) |
| `durations.R` | Durations between messy / partially-known dates with interval-arithmetic classification. | — |
| `data_import.R` | Dictionary-driven Excel import pipeline (typed/labelled import, date detection, dictionary creation, diagnostic accessors). | — |
| `data_screening.R` | Lightweight variable metadata, screening/summaries, stratified subsampling, sample-size for a margin of error, Excel date reader. | — |
| `redcap.R` | Import + recode a REDCap export using its data dictionary (factors, checkbox condensing, labels). | — |
| `proms.R` | Scoring for patient-reported outcomes (WHODAS, SF-36 PF, SDI, GAD-7, PHQ-9, EQ-5D-5L, FACIT-Pal, ESAS, Godin). | — |
| `quarto_tools.R` | Report tooling: build a doc-specific `.bib`, extract figure chunks to a save script, dump a function to file. | — |

## Reconciliation notes (duplicates resolved)

- **`helpers.R`, `functions.R`** — dropped; fully superseded by `format_utils.R`
  (formatting/transforms) and `model_eval.R` (ROC/AUC etc.).
- **`utils.R`** — split into `format_utils.R` (the global helper) and
  `quarto_tools.R` (bibliography + figure extraction).
- **`glm_functions.R`** — split into `model_eval.R` + `marker_selection.R`.
- **`glmnet_marker_selection.R` / `glmnet_marker_selection2.R`** — both dropped.
  The copy inside `glm_functions.R` was the most complete (it includes the
  factor-marker guard that v2 added over v1, and is fully namespaced); it is the
  version now in `marker_selection.R`.
- **`optimsm_corrected_AUCs.R`** — dropped; the `compute_optimism_auc()` in
  `model_eval.R` is the superset (adds .632 / .632+ and `return_optimism`).
- **`data_cleaning.R`** — its date/screening utilities are in `data_screening.R`;
  `write_function_to_file()` moved to `quarto_tools.R`.
- **Overlap kept on purpose:** `data_screening.R::getVarInfo()/clean_names()` and
  `data_import.R::get_var_info()/clean_data()/create_dictionary()` are different
  functions for different uses (quick screening vs. the full dictionary pipeline),
  so both are retained. Prefer the `data_import.R` versions for real imports.

## Documentation depth

Newly documented functions use light roxygen (title + `@param`). The existing,
already-detailed roxygen in `data_import.R` (which documents the required Excel
dictionary format) was kept intact rather than trimmed, since that detail is hard
to reconstruct.

See `BUGFIXES.md` for the list of bugs fixed during this pass.
