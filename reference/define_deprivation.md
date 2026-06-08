# Define deprivation cutoffs

Sets a deprivation cutoff for a single indicator. For each unit of
analysis, the result is `0` (not deprived), `1` (deprived), or `NA`
(missing). An additional weighted column (indicator value × weight) is
also computed.

## Usage

``` r
define_deprivation(
  .data,
  indicator,
  cutoff,
  mpi_specs = NULL,
  collapse_fn = NULL,
  set_na_equal_to = 0,
  ...
)
```

## Arguments

- .data:

  A data frame or tibble.

- indicator:

  Name of the indicator as defined in the MPI Specs (must exactly match
  the `variable` column).

- cutoff:

  A logical expression that evaluates to `TRUE` for deprived units.

- mpi_specs:

  MPI specifications from
  [`define_mpi_specs`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md).

- collapse_fn:

  An optional function to collapse roster-level data to the
  unit-of-analysis level (e.g. `max`). `NULL` (default) means no
  collapsing.

- set_na_equal_to:

  Coerce `NA` values to `0` (not deprived, default) or `1` (deprived).

- ...:

  Reserved; passing old dotted names triggers a helpful error.

## Value

A data frame with columns `*_unweighted` and `*_weighted`.

## References

[How to Apply the Alkire-Foster
Method](https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/)

## See also

[define_mpi_specs](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md)

## Examples

``` r
specs_file <- system.file(
 "extdata",
 "global-mpi-specs.csv",
 package = "mpindex"
)

specs <- define_mpi_specs(specs_file, uid = "uuid")

df_household |>
  define_deprivation(
    indicator  = drinking_water,
    cutoff     = drinking_water == 2,
    mpi_specs  = specs
  )
#> # A tibble: 198 × 3
#>    uuid                            d03_i03_drinking_wat…¹ d03_i03_drinking_wat…²
#>    <chr>                                            <int>                  <dbl>
#>  1 5dbec60a-ebda-47bd-ae18-3b017a…                      0                      0
#>  2 8b70c208-8642-408c-8a51-30bcaa…                      0                      0
#>  3 aa7cb64d-ba16-4842-8994-877206…                      0                      0
#>  4 df3e5c9b-7218-451d-9917-cd552c…                      0                      0
#>  5 57babe6a-c163-4d8e-aa80-3a9bc9…                      0                      0
#>  6 ba3f75cd-102d-482d-a979-b9098e…                      0                      0
#>  7 291c03d9-7947-459a-9c02-e68b38…                      0                      0
#>  8 b8d1b52e-2b5d-4942-9939-7c0613…                      0                      0
#>  9 2e80bf1a-03e9-4894-8792-0ce936…                      0                      0
#> 10 208992f0-9c6d-4c5c-a72a-4ffab6…                      0                      0
#> # ℹ 188 more rows
#> # ℹ abbreviated names: ¹​d03_i03_drinking_water_unweighted,
#> #   ²​d03_i03_drinking_water_weighted

df_household_roster |>
  define_deprivation(
    indicator   = school_attendance,
    cutoff      = attending_school == 2,
    mpi_specs   = specs,
    collapse_fn = max
  )
#> # A tibble: 198 × 3
#>    uuid                            d02_i02_school_atten…¹ d02_i02_school_atten…²
#>    <chr>                                            <int>                  <dbl>
#>  1 018a93c5-d65f-446f-9ee1-88a789…                      0                  0    
#>  2 01c2d953-0bd3-4827-a7f0-fee99b…                      0                  0    
#>  3 06e87d18-be21-4c62-aa35-547116…                      1                  0.167
#>  4 07d03bdd-6abc-4ef7-8c2a-b76a24…                      0                  0    
#>  5 087868f3-ef7a-4ed6-a803-75ecc9…                      1                  0.167
#>  6 0a59ad2a-20c7-4b17-a6ae-0f0250…                      0                  0    
#>  7 0aa10fbf-3347-442f-86c9-83d7a2…                      0                  0    
#>  8 0ba3b628-4082-4bc6-ae19-df01ac…                      0                  0    
#>  9 0c967915-2e93-4e6e-9ddc-f3f9be…                      0                  0    
#> 10 0cbd0210-3639-4fef-bdb1-e7e090…                      0                  0    
#> # ℹ 188 more rows
#> # ℹ abbreviated names: ¹​d02_i02_school_attendance_unweighted,
#> #   ²​d02_i02_school_attendance_weighted
```
