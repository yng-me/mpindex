# mpindex 0.3.0

## Breaking changes

* All argument names have dropped the `.` prefix (except `.data`). E.g. `.mpi_specs` → `mpi_specs`, `.weight` → `weight`, `.inference` → `inference`. Passing an old dotted name now triggers an informative error naming the renamed argument and its replacement (ADR-0006).
* `compute_mpi()` now uses a new `deprived()` helper for inline deprivation cutoffs. The previous `.deprivation_profile` list workflow is available via the new `compute_mpi_from_profile()`.
* `define_deprivation()` replaces the boolean `.collapse` argument with `.collapse_fn` — pass a function (e.g. `max`) instead of `TRUE`.
* `save_mpi()` now delegates Excel formatting to the `tsg` package. The `.formatted_output` and `.include_table_summary` arguments have been removed. A new `.include_deprivation_matrix` argument controls whether deprivation matrix sheets are written.
* Default `.names_separator` in `define_mpi_specs()` changed from `">"` to `"__"`.
* `.mpi_specs` is now required in `define_deprivation()`, `compute_mpi_from_profile()`, and `save_mpi()`. The `getOption("mpi_specs")` default has been removed — pass the result of `define_mpi_specs()` or `global_mpi_specs()` explicitly.
* `use_global_mpi_specs()` is deprecated. Use `global_mpi_specs()` instead.
* `.save_as_global_options` in `define_mpi_specs()` is deprecated and no longer has any effect.
* Minimum R version bumped to 4.1.0.

## New features

* `compute_mpi()` — single-call API with inline `deprived()` cutoffs and a `by` argument for disaggregation.
* `compute_mpi_from_profile()` — lower-level entry point for pre-assembled deprivation profiles (replaces the old `compute_mpi()` signature).
* `deprived()` — captures a bare cutoff expression and optional per-indicator settings (`collapse_fn`, `set_na_equal_to`; `.data` is unchanged).
* `global_mpi_specs()` — loads the built-in Global MPI specification. Replaces `use_global_mpi_specs()`.
* `compute_mpi()` returns an object of class `mpi_output`.
* Survey-weighted MPI via `weight`, `strata`, `cluster`, `fpc` or a pre-built `survey::svydesign()` object (`survey_design`) in both `compute_mpi()` and `compute_mpi_from_profile()`. The `survey` package is a soft dependency (`Suggests`).
* `inference = TRUE` appends design-based standard errors and confidence intervals as `*_se`, `*_ci_low`, `*_ci_high` columns. Confidence level configurable via `ci_level` (default 0.95).

## Improvements

* Internal computation refactored to use `dplyr::across()` throughout, removing deprecated `summarise_at()` / `mutate_at()` / `rename_all()` calls.
* `compute_headcount_ratio_adjusted()` correctly preserves `...` grouping variables when `aggregation` is also supplied (bug fix).
* `tsg::rename_label()` replaces manual `attr(..., "label") <-` assignments for column labels.

## Tests

* 51 new unit tests for the survey-weighted path covering both input methods, point-estimate correctness, SE/CI columns, `by` disaggregation, and backward-compatibility.

## Documentation

* Main vignette rewritten with plain-language explanations and a step-by-step narrative.
* New vignette: *Survey-Weighted MPI* — covers both input paths, inference columns, subgroup disaggregation, and a weighted vs. unweighted comparison.

---

# mpindex 0.2.1

* Changed returned value of `define_mpi_specs` from list to data frame with a class of `mpi_specs_df`. By default, it stores a global option named `mpi_specs` which can be accessed using `getOption('mpi_specs')`. Other specifications are now stored as `attributes`.
* Changed default value of `.unit_of_analysis` from `households` to `NULL`.
* `save_mpi` now allows unformatted output (tidy format) when generating an output. 
* Implemented feature to include MPI specification in the output by setting `.include_specs` tot `TRUE` in `save_mpi`.
* Deprecated `.names_separator` argument in `define_mpi_specs`.
* Enhancement of `define_deprivation` based on #17.
* Added `use_global_mpi_specs()` wrapper function to use Global MPI specification.
