# Compute MPI from a deprivation profile

Computes the Multidimensional Poverty Index using a pre-assembled
deprivation profile — a named list produced by calling
[`define_deprivation`](https://yng-me.github.io/mpindex/reference/define_deprivation.md)
once per indicator.

This is the lower-level entry point. For a single-call workflow with
inline cutoff expressions, use
[`compute_mpi`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
with
[`deprived`](https://yng-me.github.io/mpindex/reference/deprived.md).

## Usage

``` r
compute_mpi_from_profile(
  .data,
  deprivation_profile,
  ...,
  mpi_specs = NULL,
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

  A tidy data frame where each row is the unit of analysis.

- deprivation_profile:

  A named list of data frames produced by
  [`define_deprivation`](https://yng-me.github.io/mpindex/reference/define_deprivation.md).
  Names must exactly match the `variable` column in `mpi_specs`.

- ...:

  Grouping columns (tidyselect), e.g. `region`, `sex`.

- mpi_specs:

  MPI specifications from
  [`define_mpi_specs`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md).

- include_deprivation_matrix:

  Whether to include deprivation matrices in the output. Default `TRUE`.

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

## See also

[define_mpi_specs](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md),
[define_deprivation](https://yng-me.github.io/mpindex/reference/define_deprivation.md),
[compute_mpi](https://yng-me.github.io/mpindex/reference/compute_mpi.md),
[save_mpi](https://yng-me.github.io/mpindex/reference/save_mpi.md)

## Examples

``` r
specs <- define_mpi_specs(
  system.file("extdata", "global-mpi-specs.csv", package = "mpindex"),
  uid = "uuid"
)

deprivation_profile <- list()

deprivation_profile$drinking_water <- df_household |>
  define_deprivation(
    indicator = drinking_water,
    cutoff    = drinking_water == 2,
    mpi_specs = specs
  )

# ... (define remaining indicators) ...

if (FALSE) { # \dontrun{
mpi_result <- compute_mpi_from_profile(
  df_household, deprivation_profile, mpi_specs = specs
)
} # }
```
