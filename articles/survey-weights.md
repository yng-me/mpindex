# Survey-Weighted MPI

## Why survey weights matter

Household surveys rarely sample households with equal probability. Urban
areas, minority groups, or remote regions may be over- or under-sampled
to improve precision. Each household is therefore assigned a **sampling
weight** that records how many population households it represents.

If you compute the MPI without accounting for these weights, every
sampled household counts equally — which gives biased estimates for the
true population. With weights, a household with weight 3.2 contributes
3.2 times as much to H, A, and the indicator means as a household with
weight 1.0.

Surveys also often use a **complex design** — stratified sampling
(separate strata such as urban/rural) combined with cluster sampling
(selecting geographic clusters, then households within them). Ignoring
this structure leads to standard errors that are too small and
confidence intervals that are too narrow.

`mpindex` supports both through the `survey` package.

## Installation

``` r

install.packages("survey")   # only needed once
```

## Setup

``` r

library(mpindex)
library(survey)
#> Loading required package: grid
#> Loading required package: Matrix
#> Loading required package: survival
#> 
#> Attaching package: 'survey'
#> The following object is masked from 'package:graphics':
#> 
#>     dotchart
```

``` r

mpi_specs <- global_mpi_specs(uid = "uuid")
```

For this vignette we add synthetic survey columns to the built-in
dataset. In real work these columns come from the survey microdata.

``` r

set.seed(42)
n <- nrow(df_household)

df_hh <- df_household
df_hh$hh_weight <- runif(n, 0.8, 2.5)   # sampling weight
df_hh$strata    <- sample(c("urban", "rural"), n, replace = TRUE)
df_hh$psu       <- sample(1:30, n, replace = TRUE)  # primary sampling unit
```

Define the deprivation cutoffs (same as in the main vignette):

``` r

deprivations <- list(
  nutrition         = deprived(undernourished == 1 & age < 70,
                               .data = df_household_roster, collapse_fn = max),
  child_mortality   = deprived(with_child_died == 1),
  year_schooling    = deprived(completed_6yrs_schooling == 2,
                               .data = df_household_roster, collapse_fn = max),
  school_attendance = deprived(attending_school == 2 & age %in% 5:24,
                               .data = df_household_roster, collapse_fn = max),
  cooking_fuel      = deprived(cooking_fuel %in% c(4:6, 9)),
  sanitation        = deprived(toilet > 1),
  drinking_water    = deprived(drinking_water == 2),
  electricity       = deprived(electricity == 2),
  housing           = deprived(
    roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
  ),
  assets = deprived(!(
    (asset_tv + asset_telephone + asset_mobile_phone + asset_computer +
       asset_animal_cart + asset_bicycle + asset_motorcycle +
       asset_refrigerator) > 1 &
      (asset_car + asset_truck) > 0
  ))
)
```

------------------------------------------------------------------------

## Simple random sample (weights only)

The most common starting point: you have a **weight column** but no
stratification or clustering. This arises when households are selected
with unequal probability but the design has no explicit strata or
clusters — for example, a probability-proportional-to-size (PPS) sample
with a single stage of selection.

Pass only `weight`. No `strata` or `cluster` needed.

``` r

mpi_simple <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight"
)

mpi_simple$index$k_33
#> # A tibble: 1 × 4
#>   number_of_cases headcount_ratio intensity   mpi
#>             <int>           <dbl>     <dbl> <dbl>
#> 1             198           0.354     0.469 0.166
```

You can also request standard errors. Because there is no cluster
structure, variance is estimated under simple random sampling with
replacement (the `survey` package default when `ids = ~1`):

``` r

mpi_simple_inf <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  inference = TRUE
)

mpi_simple_inf$index$k_33[, c("headcount_ratio", "headcount_ratio_se",
                               "mpi", "mpi_se")]
#> # A tibble: 1 × 4
#>   headcount_ratio headcount_ratio_se   mpi mpi_se
#>             <dbl>              <dbl> <dbl>  <dbl>
#> 1           0.354             0.0351 0.166 0.0171
```

------------------------------------------------------------------------

## Option A — pass column names directly

The simplest way: tell
[`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
which columns in your data frame carry the weight, stratum, and cluster
identifiers.

``` r

mpi_weighted <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  strata = "strata",
  cluster = "psu"
)

mpi_weighted$index$k_33
#> # A tibble: 1 × 4
#>   number_of_cases headcount_ratio intensity   mpi
#>             <int>           <dbl>     <dbl> <dbl>
#> 1             198           0.354     0.469 0.166
```

All four components of the output — `$index`, `$contribution`,
`$headcount_ratio`, and `$deprivation_matrix` — now reflect
population-weighted estimates.

A finite-population correction can also be supplied if your sampling
frame contains the stratum sizes:

``` r

compute_mpi(df_hh, mpi_specs = mpi_specs, deprivations = deprivations,
            weight = "hh_weight", strata = "strata", cluster = "psu",
            .fpc    = "stratum_size")
```

------------------------------------------------------------------------

## Option B — pre-built `svydesign` object

If you already have a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
object — or prefer to specify the design once and reuse it — pass it via
`survey_design`:

``` r

svy <- svydesign(
  ids     = ~psu,
  strata  = ~strata,
  weights = ~hh_weight,
  nest    = TRUE,         # PSU IDs restart within each stratum
  data    = df_hh
)

mpi_from_design <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  survey_design = svy
)

mpi_from_design$index$k_33
#> # A tibble: 1 × 4
#>   number_of_cases headcount_ratio intensity   mpi
#>             <int>           <dbl>     <dbl> <dbl>
#> 1             198           0.354     0.469 0.166
```

Both options produce identical point estimates.

------------------------------------------------------------------------

## Adding standard errors and confidence intervals

Set `inference = TRUE` to append design-based standard errors and 95%
confidence intervals alongside every point estimate. The intervals use
the normal approximation and are clamped to \[0, 1\].

``` r

mpi_inference <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  strata = "strata",
  cluster = "psu",
  inference = TRUE
)

mpi_inference$index$k_33
#> # A tibble: 1 × 13
#>   number_of_cases headcount_ratio intensity   mpi headcount_ratio_se
#>             <int>           <dbl>     <dbl> <dbl>              <dbl>
#> 1             198           0.354     0.469 0.166             0.0362
#> # ℹ 8 more variables: headcount_ratio_ci_low <dbl>,
#> #   headcount_ratio_ci_high <dbl>, intensity_se <dbl>, intensity_ci_low <dbl>,
#> #   intensity_ci_high <dbl>, mpi_se <dbl>, mpi_ci_low <dbl>, mpi_ci_high <dbl>
```

The extra columns follow a consistent naming pattern:

| Column                    | Meaning                     |
|---------------------------|-----------------------------|
| `headcount_ratio`         | Point estimate for H        |
| `headcount_ratio_se`      | Design-based standard error |
| `headcount_ratio_ci_low`  | Lower bound of CI           |
| `headcount_ratio_ci_high` | Upper bound of CI           |

The same pattern applies to `intensity`, `mpi`, and every indicator
column in `$headcount_ratio`.

Change the confidence level with `ci_level`:

``` r

compute_mpi(..., inference = TRUE, ci_level = 0.90)   # 90% CI
```

------------------------------------------------------------------------

## Disaggregation by subgroup

Combine survey weighting with `by` to get group-specific weighted
estimates. Each group’s H, A, and MPI are computed using only the design
rows in that group.

``` r

mpi_by_class <- compute_mpi(
  df_hh,
  mpi_specs = mpi_specs,
  deprivations = deprivations,
  weight = "hh_weight",
  by = class,
  inference = TRUE
)

mpi_by_class$index$k_33
#> # A tibble: 2 × 14
#>   class number_of_cases headcount_ratio intensity   mpi headcount_ratio_se
#>   <chr>           <dbl>           <dbl>     <dbl> <dbl>              <dbl>
#> 1 Rural              98           0.430     0.515 0.222             0.0519
#> 2 Urban             100           0.279     0.400 0.112             0.0464
#> # ℹ 8 more variables: headcount_ratio_ci_low <dbl>,
#> #   headcount_ratio_ci_high <dbl>, intensity_se <dbl>, intensity_ci_low <dbl>,
#> #   intensity_ci_high <dbl>, mpi_se <dbl>, mpi_ci_low <dbl>, mpi_ci_high <dbl>
```

------------------------------------------------------------------------

## Comparing weighted vs. unweighted

It is instructive to compare weighted and unweighted estimates. When the
sampling design is informative (i.e. selection probability is correlated
with poverty status), the differences can be substantial.

``` r

mpi_unweighted <- compute_mpi(df_household, mpi_specs, deprivations)

cat("Unweighted H:", round(mpi_unweighted$index$k_33$headcount_ratio, 4), "\n")
#> Unweighted H: 0.3788
cat("Weighted   H:", round(mpi_weighted$index$k_33$headcount_ratio,   4), "\n")
#> Weighted   H: 0.3542
```

------------------------------------------------------------------------

## Step-by-step workflow

The same survey arguments work with
[`compute_mpi_from_profile()`](https://yng-me.github.io/mpindex/reference/compute_mpi_from_profile.md):

``` r

mpi_result <- compute_mpi_from_profile(
  df_hh,
  deprivation_profile,        # pre-assembled list from define_deprivation()
  mpi_specs = mpi_specs,
  weight = "hh_weight",
  strata = "strata",
  cluster = "psu",
  inference = TRUE
)
```

------------------------------------------------------------------------

## Saving survey-weighted output to Excel

[`save_mpi()`](https://yng-me.github.io/mpindex/reference/save_mpi.md)
works identically whether the output is weighted or not. The SE and CI
columns, when present, are written to the same sheets alongside the
point estimates.

``` r

save_mpi(mpi_inference, mpi_specs = mpi_specs, filename = "MPI Weighted Results")
```
