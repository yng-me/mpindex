# Specify a deprivation cutoff for use in `compute_mpi`

A helper that captures a bare deprivation cutoff expression and optional
per-indicator settings for use inside the `deprivations` argument of
[`compute_mpi`](https://yng-me.github.io/mpindex/reference/compute_mpi.md).

## Usage

``` r
deprived(.cutoff, .data = NULL, collapse_fn = NULL, set_na_equal_to = 0, ...)
```

## Arguments

- .cutoff:

  A bare logical expression evaluated against the indicator's data
  frame. Rows where this evaluates to `TRUE` are considered deprived.

- .data:

  An optional data frame to use for this indicator instead of the
  primary `.data` passed to `compute_mpi`. Useful when one or more
  indicators are at a different unit of analysis (e.g. person-level
  roster).

- collapse_fn:

  An optional function applied to collapse roster-level data to the
  unit-of-analysis level (e.g. `max` to flag a household as deprived if
  any member is deprived). If `NULL` (default), no collapsing is
  performed. NAs are removed before calling the function; if all values
  are `NA` the result is `NA`.

- set_na_equal_to:

  Coerce `NA` deprivation values to `0` (not deprived, default) or `1`
  (deprived).

- ...:

  Reserved; passing old dotted names (e.g. `.collapse_fn`) triggers a
  helpful error.

## Value

An object of class `mpi_deprivation_spec`.

## See also

[compute_mpi](https://yng-me.github.io/mpindex/reference/compute_mpi.md)

## Examples

``` r
deprived(drinking_water == 2)
#> $cutoff
#> <quosure>
#> expr: ^drinking_water == 2
#> env:  0x55bfeb220df8
#> 
#> $data
#> NULL
#> 
#> $collapse_fn
#> NULL
#> 
#> $set_na_equal_to
#> [1] 0
#> 
#> attr(,"class")
#> [1] "mpi_deprivation_spec"
deprived(undernourished == 1 & age < 70, .data = df_household_roster, collapse_fn = max)
#> $cutoff
#> <quosure>
#> expr: ^undernourished == 1 & age < 70
#> env:  0x55bfeb220df8
#> 
#> $data
#> # A tibble: 905 × 8
#>    uuid            line_number class sex     age attending_school undernourished
#>    <chr>                 <int> <chr> <chr> <int>            <int>          <int>
#>  1 5dbec60a-ebda-…           1 Rural Male     55               NA              2
#>  2 5dbec60a-ebda-…           2 Rural Fema…    48               NA              2
#>  3 5dbec60a-ebda-…           3 Rural Fema…    17                1              2
#>  4 5dbec60a-ebda-…           4 Rural Male     10                1              2
#>  5 8b70c208-8642-…           1 Rural Fema…    30               NA              2
#>  6 8b70c208-8642-…           2 Rural Male     26               NA              2
#>  7 8b70c208-8642-…           3 Rural Male      3               NA              2
#>  8 8b70c208-8642-…           4 Rural Male     19                1              2
#>  9 aa7cb64d-ba16-…           1 Rural Male     66               NA              2
#> 10 aa7cb64d-ba16-…           2 Rural Fema…    62               NA              2
#> # ℹ 895 more rows
#> # ℹ 1 more variable: completed_6yrs_schooling <int>
#> 
#> $collapse_fn
#> function (..., na.rm = FALSE)  .Primitive("max")
#> 
#> $set_na_equal_to
#> [1] 0
#> 
#> attr(,"class")
#> [1] "mpi_deprivation_spec"
```
