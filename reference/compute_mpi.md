# Compute Multidimensional Poverty Index (MPI)

The primary single-call API for computing the MPI using the
Alkire-Foster (AF) counting method. Deprivation cutoffs are specified
inline using the
[`deprived`](https://yng-me.github.io/mpindex/reference/deprived.md)
helper, making the workflow self-contained and readable.

For a step-by-step workflow using a pre-assembled deprivation profile,
see
[`compute_mpi_from_profile`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md).

## Usage

``` r
compute_mpi(
  .data,
  mpi_specs,
  deprivations,
  ...,
  by = NULL,
  include_deprivation_matrix = TRUE,
  generate_output = FALSE,
  mpi_output_filename = NULL,
  include_specs = FALSE,
  weight = NULL,
  strata = NULL,
  cluster = NULL,
  fpc = NULL,
  survey_design = NULL,
  inference = FALSE,
  ci_level = 0.95
)
```

## Arguments

- .data:

  A data frame where each row is the unit of analysis.

- mpi_specs:

  MPI specifications from
  [`define_mpi_specs`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md).

- deprivations:

  A named list of
  [`deprived`](https://yng-me.github.io/mpindex/reference/deprived.md)
  calls. Each name must exactly match a `variable` in `mpi_specs`.

- ...:

  Grouping columns (tidyselect) or reserved for old-name detection.

- by:

  *(Optional)* Columns to group results by, passed as a tidyselect
  expression, e.g. `c(region, sex)`.

- include_deprivation_matrix:

  Whether to include deprivation matrices. Default `TRUE`.

- generate_output:

  Whether to write an Excel file as a side effect. Default `FALSE`.

- mpi_output_filename:

  Output filename when `generate_output = TRUE`.

- include_specs:

  Whether to include MPI specification sheet in Excel output.

- weight:

  Name of the sampling-weight column in `.data`. When supplied, all
  estimates are survey-weighted. Requires the survey package.

- strata:

  Name of the stratum column in `.data`.

- cluster:

  Name of the cluster / PSU column in `.data`.

- fpc:

  Name of the finite-population correction column in `.data`.

- survey_design:

  A pre-built
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  object. Provide either `weight` / `strata` / `cluster` / `fpc` *or*
  `survey_design`, not both.

- inference:

  Logical. When `TRUE` (and a survey design is supplied), standard
  errors and confidence intervals are appended as `*_se`, `*_ci_low`,
  `*_ci_high` columns. Default `FALSE`.

- ci_level:

  Confidence level for intervals. Default `0.95`.

## Value

A named list of class `mpi_output` with components:

- `$index`:

  Named list keyed by `k_*`: MPI, H, A, n.

- `$contribution`:

  Named list keyed by `k_*`: contribution by indicator/dimension.

- `$headcount_ratio`:

  Named list with `uncensored` and per-`k_*` censored ratios.

- `$deprivation_matrix`:

  Named list with `uncensored` and per-`k_*` matrices.

## References

[Alkire-Foster
Method](https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/)  
[How to Apply the Alkire-Foster
Method](https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/)

## See also

[define_mpi_specs](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md),
[deprived](https://yng-me.github.io/mpindex/reference/deprived.md),
[compute_mpi_from_profile](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md),
[save_mpi](https://yng-me.github.io/mpindex/reference/save_mpi.md)

## Examples

``` r
specs <- define_mpi_specs(
  system.file("extdata", "global-mpi-specs.csv", package = "mpindex"),
  uid = "uuid"
)

if (FALSE) { # \dontrun{
mpi_result <- compute_mpi(
  df_household,
  mpi_specs = specs,
  deprivations = list(
    nutrition = deprived(
      undernourished == 1 & age < 70,
      .data = df_household_roster,
      collapse_fn = max
    ),
    child_mortality = deprived(with_child_died == 1),
    year_schooling = deprived(
      completed_6yrs_schooling == 2,
      .data = df_household_roster,
      collapse_fn = max
    ),
    school_attendance = deprived(
      attending_school == 2 & age %in% c(5:24),
      .data = df_household_roster,
      collapse_fn = max
    ),
    cooking_fuel   = deprived(cooking_fuel %in% c(4:6, 9)),
    sanitation     = deprived(toilet > 1),
    drinking_water = deprived(drinking_water == 2),
    electricity    = deprived(electricity == 2),
    housing        = deprived(
      roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
    ),
    assets = deprived(!(
      (asset_tv + asset_telephone + asset_mobile_phone + asset_computer +
         asset_animal_cart + asset_bicycle + asset_motorcycle +
         asset_refrigerator) > 1 &
        (asset_car + asset_truck) > 0
    ))
  )
)
} # }
```
