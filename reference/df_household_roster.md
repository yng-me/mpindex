# Sample dataset of household members

This dataset contains a many-to-one relationship with the
[df_household](https://yng-me.github.io/mpindex/reference/df_household.md)
dataset. Hence, you can apply joins using the `uuid`.

## Usage

``` r
df_household_roster
```

## Format

A tibble with 905 rows and 8 variables:

- uuid:

  Unique ID

- line_number:

  Number identifier for each member within the household

- class:

  Urbanity: `Rural` or `Urban`

- sex:

  Sex of the household member

- age:

  Age of the household member

- attending_school:

  Whether the household member (aged 5-24 years old) is currently
  attending school: `1` - currently attending; `2` - currently not
  attending

- completed_6yrs_schooling:

  Whether completed at least six (6) years of schooling: `1` -
  completed; `2` -not completed

- undernourished:

  Whether the household member (aged below 70 years old) is
  undernourished: `1` - undernourished; `2` - not undernourished

## See also

[df_household](https://yng-me.github.io/mpindex/reference/df_household.md)

## Examples

``` r
df_household_roster
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
```
