# Compute MPI from a deprivation profile (internal)

Internal implementation used by
[`compute_mpi`](https://yng-me.github.io/mpindex/reference/compute_mpi.md).
Not exported. Use
[`compute_mpi`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
directly.

## Usage

``` r
compute_mpi_from_profile(
  .data,
  deprivation_profile,
  ...,
  by = character(0),
  mpi_specs = NULL,
  include_deprivation_matrix = TRUE,
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

- ...:

  Extra columns (tidyselect) to carry through into the deprivation
  matrix. Not used for grouping.

- by:

  Pre-resolved character vector of grouping column names. Populated by
  [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
  from its `by` tidyselect argument.

- mpi_specs:

  MPI specifications from
  [`define_mpi_specs`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md).

- include_deprivation_matrix:

  Whether to include deprivation matrices.

- weight, strata, cluster, fpc, survey_design, inference, ci_level:

  Survey design arguments; forwarded from
  [`compute_mpi`](https://yng-me.github.io/mpindex/reference/compute_mpi.md).

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
