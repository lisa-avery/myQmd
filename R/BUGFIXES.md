# Bug fixes applied

Each fix is marked with a `# BUGFIX:` comment at the relevant line in the code.

### model_eval.R

1. **`gg_roc(..., plotOnly = FALSE)` errored.** The function returned
   `list(plot = p, auc = auc)` but `auc` was never assigned in the function's
   scope, so any non-plot call failed with "object 'auc' not found". Now `auc`
   is set before the return: the named AUC vector for a list of models, or the
   formatted AUC for a single model/data frame.

### proms.R

2. **Undefined `recode_values()`.** `score_whodas()` and `score_facitpal()`
   called `recode_values(value, "None" ~ 0, ...)`, which is not defined anywhere
   in the codebase (nor in dplyr/base). Replaced with `dplyr::case_match()`,
   whose `old ~ new` syntax matches exactly what the calls assumed.
3. **Dead line in `score_whodas()`.** A stray
   `cbind(df$whodas_total, rowSums(...))` computed a matrix and discarded it
   (no assignment, no side effect). Removed.

### redcap.R

4. **Spurious `<var>_all2` column / crash.** A block after the checkbox loop
   re-ran `add_column(!!paste0(v, "_all2") := condensed_var, ...)` using the
   final iteration's `v`, `condensed_var` and `lbl_lookup`. This added a junk
   column and, when there were no checkbox variables, referenced undefined
   loop variables. Removed.
5. **Checkbox labels mis-assigned.** `lbl_lookup$label[i]` indexed labels by
   position while `i` iterated over REDCap *code values*, silently mislabelling
   any checkbox group coded from 0 or with non-contiguous codes. Now uses
   `lbl_lookup$label[match(i, lbl_lookup$code)]`.

### survival.R

6. **Hard dependency on a missing file.** The script ran
   `source("deid_functions.R")` on load and `safe_date()` called
   `parse_date_column()` from it; that file was not part of this set, so the
   script failed to source. `safe_date()` now uses `parse_date_column()` or
   `convert_column_to_date()` if either is on the search path, otherwise falls
   back to lubridate then base date parsing — making the script self-contained.

### data_screening.R

7. **Underscore collapsing in `clean_names()`.** `gsub("__", "_", x)` only
   collapsed *doubled* underscores, leaving runs of three or more. Changed to
   `gsub("_+", "_", x)`.

## Side-effects removed (not bugs, but unsafe in a sourced template)

- `survival.R`: removed `require(survival)` / `require(dplyr)`; calls are now
  namespaced (`survival::survfit`, `survival::Surv`, `stats::na.omit`,
  `dplyr::case_when`).
- `durations.R`: removed `library(dplyr)`; `mutate` calls namespaced.
- `redcap.R`: removed `library(tidyverse)`; tidyverse calls namespaced.
- `proms.R`: namespaced bare dplyr/tidyr/stringr/tibble/rlang verbs; replaced
  `1:n()` row indices with `seq_len(dplyr::n())`.
- `correlations.R`: converted magrittr `%>%` to the native pipe `|>`.

## Latent issues noted but not changed (to preserve behaviour)

- `data_screening.R::numericSummary()` selects columns with
  `inherits(x, "numeric")`, which excludes integer columns. Left as-is in case
  that exclusion is intentional; switch to `is.numeric(x)` if integers should
  be summarised.
- `durations.R::compute_trt_start_to_cr()` writes logical `TRUE`/`FALSE` into a
  character result vector (coerced to `"TRUE"`/`"FALSE"`) alongside the
  `"not computable..."` strings. Behaviour is consistent but the column is
  character, not logical.
