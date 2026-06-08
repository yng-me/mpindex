# Migrating to mpindex 0.3.0

``` r

library(mpindex)
```

Version 0.3.0 contains several breaking changes that require updates to
existing code. This vignette walks through each one with before/after
examples so you can migrate quickly.

------------------------------------------------------------------------

## 1. Argument names no longer carry a `.` prefix

Every argument name across all public functions has dropped its leading
`.` — the only exception is `.data`, which stays because it is a special
pronoun in the tidyverse and collides with `base::data()`.

Passing an old dotted name now triggers an informative error:

``` r

# Old (0.2.x) — now errors
define_mpi_specs(
  "path/to/specs.csv",
  .uid            = "uuid",
  .poverty_cutoff = 1/3
)
#> Error in `define_mpi_specs()`:
#> ! Argument(s) renamed in 0.3.0 in `define_mpi_specs()`: `.uid` -> `uid`. Please update your call.
```

**Rename map** — find-and-replace the left column with the right column
in your scripts:

| Old (0.2.x) | New (0.3.0) | Function(s) |
|----|----|----|
| `.mpi_specs_file` | `mpi_specs_file` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) |
| `.uid` | `uid` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) |
| `.poverty_cutoffs` | `poverty_cutoffs` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) |
| `.unit_of_analysis` | `unit_of_analysis` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) |
| `.aggregation` | `aggregation` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) |
| `.source_of_data` | `source_of_data` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) |
| `.names_separator` | `names_separator` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) |
| `.save_as_global_options` | `save_as_global_options` | [`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md) *(deprecated)* |
| `.indicator` | `indicator` | [`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md) |
| `.cutoff` | `cutoff` | [`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md) |
| `.mpi_specs` | `mpi_specs` | [`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md), [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md), [`save_mpi()`](https://yng-me.github.io/mpindex/reference/save_mpi.md) |
| `.collapse_fn` | `collapse_fn` | [`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md), [`deprived()`](https://yng-me.github.io/mpindex/reference/deprived.md) |
| `.collapse_condition` | `collapse_fn` | [`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md) |
| `.set_na_equal_to` | `set_na_equal_to` | [`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md), [`deprived()`](https://yng-me.github.io/mpindex/reference/deprived.md) |
| `.deprivation_profile` | `deprivation_profile` | [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.by` | `by` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md) |
| `.include_deprivation_matrix` | `include_deprivation_matrix` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md), [`save_mpi()`](https://yng-me.github.io/mpindex/reference/save_mpi.md) |
| `.generate_output` | `generate_output` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.mpi_output_filename` | `mpi_output_filename` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.include_specs` | `include_specs` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md), [`save_mpi()`](https://yng-me.github.io/mpindex/reference/save_mpi.md) |
| `.weight` | `weight` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.strata` | `strata` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.cluster` | `cluster` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.fpc` | `fpc` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.survey_design` | `survey_design` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.inference` | `inference` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.ci_level` | `ci_level` | [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md), [`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md) |
| `.filename` | `filename` | [`save_mpi()`](https://yng-me.github.io/mpindex/reference/save_mpi.md) |
| `.aggregation` | `aggregation` | `compute_headcount_ratio()`, `compute_headcount_ratio_adjusted()` |

**Before:**

``` r

specs <- define_mpi_specs(
  "specs.csv",
  .uid            = "uuid",
  .poverty_cutoffs = c(1/3, 1/2),
  .unit_of_analysis = "household"
)

result <- compute_mpi(
  df_household,
  .mpi_specs   = specs,
  .deprivations = deps,
  .by           = region
)
```

**After:**

``` r

specs <- define_mpi_specs(
  "specs.csv",
  uid             = "uuid",
  poverty_cutoffs = c(1/3, 1/2),
  unit_of_analysis = "household"
)

result <- compute_mpi(
  df_household,
  mpi_specs    = specs,
  deprivations = deps,
  by           = region
)
```

------------------------------------------------------------------------

## 2. `mpi_specs` is now required — no more global option

In 0.2.x, calling
[`use_global_mpi_specs()`](https://yng-me.github.io/mpindex/reference/use_global_mpi_specs.md)
stored the spec in `options("mpi_specs")` and functions picked it up
automatically when `mpi_specs` was omitted. That implicit lookup is
gone.

**Before:**

``` r

use_global_mpi_specs()                        # stored in options()

dm <- define_deprivation(df, drinking_water, .cutoff = drinking_water == 2)
                                              # ↑ silently read from options()
```

**After — pass `mpi_specs` explicitly every time:**

``` r

specs <- global_mpi_specs()                   # load once …

dm <- define_deprivation(
  df, drinking_water,
  cutoff    = drinking_water == 2,
  mpi_specs = specs                           # … then pass it
)
```

Omitting `mpi_specs` now raises an error that tells you exactly what to
do:

``` r

define_deprivation(df_household, drinking_water, cutoff = drinking_water == 2)
#> Error in `define_deprivation()`:
#> ! No MPI Specs supplied. Pass the result of `define_mpi_specs()` as `mpi_specs`, or use `global_mpi_specs()` to load the built-in Global MPI spec.
```

------------------------------------------------------------------------

## 3. `use_global_mpi_specs()` is deprecated — use `global_mpi_specs()`

``` r

# Soft-deprecated: raises a warning but still works
specs <- use_global_mpi_specs()
#> Warning: `use_global_mpi_specs()` was deprecated in mpindex 0.3.0.
#> ℹ Please use `global_mpi_specs()` instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

Replace every call with
[`global_mpi_specs()`](https://yng-me.github.io/mpindex/reference/global_mpi_specs.md)
(or
[`define_mpi_specs()`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md)
for a custom spec file):

``` r

# Before
use_global_mpi_specs()

# After
specs <- global_mpi_specs()
```

------------------------------------------------------------------------

## 4. `.save_as_global_options` is deprecated and no-ops

Previously you could persist the spec to
[`options()`](https://rdrr.io/r/base/options.html) via:

``` r

define_mpi_specs("specs.csv", .save_as_global_options = TRUE)
```

The argument still exists so your code doesn’t break immediately, but it
now emits a deprecation warning and has **no effect**. Remove it from
your calls and store the return value instead:

``` r

# Before
define_mpi_specs("specs.csv", .save_as_global_options = TRUE)
# later code called getOption("mpi_specs")

# After
specs <- define_mpi_specs("specs.csv")
# pass specs explicitly to each function
```

------------------------------------------------------------------------

## 5. `compute_mpi()` has a new signature — old profile workflow moved

In 0.2.x,
[`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
accepted a `.deprivation_profile` list of pre-computed deprivation
matrices. That workflow is now in the separate function
[`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md).

The new
[`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
uses inline
[`deprived()`](https://yng-me.github.io/mpindex/reference/deprived.md)
cutoffs instead:

**Before:**

``` r

dm1 <- define_deprivation(df, drinking_water, .cutoff = drinking_water == 2, ...)
dm2 <- define_deprivation(df, electricity,    .cutoff = electricity == 2,    ...)

result <- compute_mpi(
  df,
  .mpi_specs          = specs,
  .deprivation_profile = list(drinking_water = dm1, electricity = dm2)
)
```

**After — Option A: new inline API (`compute_mpi`)**

``` r

specs  <- global_mpi_specs()

result <- compute_mpi(
  df_household,
  mpi_specs    = specs,
  deprivations = list(
    drinking_water = deprived(drinking_water == 2),
    electricity    = deprived(electricity == 2)
  )
)
```

**After — Option B: keep the profile workflow
(`compute_mpi_from_profile`)**

``` r

specs <- global_mpi_specs()

dm1 <- define_deprivation(df_household, drinking_water,
                           cutoff = drinking_water == 2, mpi_specs = specs)
dm2 <- define_deprivation(df_household, electricity,
                           cutoff = electricity == 2,    mpi_specs = specs)

result <- compute_mpi_from_profile(
  df_household,
  list(drinking_water = dm1, electricity = dm2),
  mpi_specs = specs
)
```

------------------------------------------------------------------------

## 6. `.collapse` replaced by `collapse_fn`

The boolean `.collapse = TRUE` argument in
[`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md)
and
[`deprived()`](https://yng-me.github.io/mpindex/reference/deprived.md)
has been replaced by `collapse_fn`, which accepts the actual aggregation
function.

**Before:**

``` r

deprived(attending_school == 2, .collapse = TRUE)      # used any() internally
```

**After:**

``` r

deprived(attending_school == 2, collapse_fn = max)     # explicit function
```

Common replacements:

| Old                | New                 |
|--------------------|---------------------|
| `.collapse = TRUE` | `collapse_fn = max` |
| *(implicit any)*   | `collapse_fn = any` |

------------------------------------------------------------------------

## 7. `save_mpi()` — removed arguments

Two arguments were removed from
[`save_mpi()`](https://yng-me.github.io/mpindex/reference/save_mpi.md):

| Removed argument         | Reason                                    |
|--------------------------|-------------------------------------------|
| `.formatted_output`      | Formatting delegated to the `tsg` package |
| `.include_table_summary` | Superseded by `tsg` output templates      |

The new `include_deprivation_matrix` argument (default `TRUE`) controls
whether the deprivation-matrix sheets are written to the Excel file.

**Before:**

``` r

save_mpi(result, .formatted_output = TRUE, .include_table_summary = FALSE)
```

**After:**

``` r

save_mpi(result, mpi_specs = specs, include_deprivation_matrix = TRUE)
```

------------------------------------------------------------------------

## 8. Default `names_separator` changed

The default separator used when constructing `variable_name` columns
inside an `mpi_specs_df` changed from `">"` to `"__"`. This only affects
you if you relied on the auto-generated `variable_name` values matching
a specific pattern.

**Before:** `d01_i01>drinking_water` **After:**
`d01_i01__drinking_water`

Pass `names_separator = ">"` explicitly to restore the old behaviour:

``` r

specs <- define_mpi_specs("specs.csv", names_separator = ">")
```

------------------------------------------------------------------------

## 9. Minimum R version bumped to 4.1.0

`mpindex` 0.3.0 requires R ≥ 4.1.0 (released May 2021). The native pipe
`|>` and `\(x)` lambda syntax are used throughout the package internals.
Update R if you are on an older version.

------------------------------------------------------------------------

## Quick migration checklist

    [ ] Strip `.` prefix from every argument name (except `.data`)
    [ ] Replace use_global_mpi_specs() with specs <- global_mpi_specs()
    [ ] Remove .save_as_global_options = TRUE; store the return value instead
    [ ] Pass mpi_specs = specs explicitly in define_deprivation(),
        compute_mpi_from_profile(), and save_mpi()
    [ ] Replace .collapse = TRUE with collapse_fn = max (or collapse_fn = any)
    [ ] Move deprivation-profile workflows to compute_mpi_from_profile() or
        switch to the new inline deprived() API in compute_mpi()
    [ ] Remove .formatted_output / .include_table_summary from save_mpi() calls
    [ ] Verify your R version is ≥ 4.1.0
