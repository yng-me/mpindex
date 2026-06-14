# Getting Started with mpindex

## What is the MPI?

The **Multidimensional Poverty Index (MPI)** measures poverty not just
by income, but by whether people are deprived across multiple areas of
their lives — health, education, and living standards. A person or
household is considered poor only if they fall short on *enough* of
these areas at the same time.

The approach used here is the **Alkire-Foster (AF) method**, developed
by Sabina Alkire and James Foster at the Oxford Poverty and Human
Development Initiative (OPHI). It is the basis of the [Global
MPI](https://ophi.org.uk/multidimensional-poverty-index/) published
annually by OPHI and the UNDP.

The `mpindex` package makes it straightforward to compute the MPI from
survey data in R.

------------------------------------------------------------------------

## Installation

Install the released version from CRAN:

``` r

install.packages("mpindex")
```

Or install the development version from GitHub:

``` r

# install.packages("devtools")
devtools::install_github("yng-me/mpindex")
```

Then load the package:

``` r

library(mpindex)
```

------------------------------------------------------------------------

## The workflow at a glance

Computing the MPI with `mpindex` follows three steps:

1.  **Define your indicators** — load a specification file that
    describes the dimensions, indicators, and weights.
2.  **Specify deprivation cutoffs** — tell the package when a household
    is considered deprived on each indicator.
3.  **Compute and explore the results** — run
    [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
    to get the index, headcount ratio, intensity, and indicator
    contributions.

The sections below walk through each step using a built-in dataset.

------------------------------------------------------------------------

## Step 1: Define your MPI specifications

### What goes in a specification file?

The specification file tells `mpindex` how your indicators are
organized. It must contain these columns (column names are not
case-sensitive):

| Column        | What it means                                              |
|---------------|------------------------------------------------------------|
| `Dimension`   | The broad domain (e.g. Health, Education)                  |
| `Indicator`   | The specific measure within that dimension                 |
| `Variable`    | The column name in your dataset that holds this measure    |
| `Weight`      | How much this indicator contributes to the overall score   |
| `Description` | *(optional)* A plain-language description of the indicator |

The package accepts `.csv`, `.xlsx`, `.json`, and `.txt` (tab-separated)
files.

### Using the built-in Global MPI specification

`mpindex` ships with the Global MPI specification as an example. The
[`global_mpi_specs()`](https://yng-me.github.io/mpindex/reference/global_mpi_specs.md)
(formerly
[`global_mpi_specs()`](https://yng-me.github.io/mpindex/reference/global_mpi_specs.md)
which is now deprecated) shortcut loads it in one line:

``` r

mpi_specs <- global_mpi_specs(uid = "uuid", unit_of_analysis = 'households')
```

The `uid` argument names the column in your dataset that uniquely
identifies each household (the unit of analysis). In the built-in
dataset, it is `"uuid"` (yours might be different).

Behind the scenes this is equivalent to:

``` r

specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
mpi_specs  <- define_mpi_specs(specs_file, uid = "uuid", unit_of_analysis = 'households')
```

The Global MPI covers 10 indicators across three dimensions, each
weighted equally within its dimension:

| Global MPI — Dimensions, Indicators, and Weights |  |  |  |  |
|----|----|----|----|----|
| Source: OPHI MPI Methodological Note 49 (2020) |  |  |  |  |
| Dimension | Indicator | Variable | Weight | Description |
| Health | Nutrition | nutrition | 0.167 | Any person under 70 years of age for whom there is nutritional information is undernourished. |
| Health | Child mortality | child_mortality | 0.167 | A child under 18 has died in the household in the five-year period preceding the survey. |
| Education | Years of schooling | year_schooling | 0.167 | No eligible household member has completed six years of schooling. |
| Education | School attendance | school_attendance | 0.167 | Any school-aged child is not attending school up to the age at which he/she would complete class 8. |
| Living Standards | Cooking fuel | cooking_fuel | 0.056 | A household cooks using solid fuel, such as dung, agricultural crop, shrubs, wood, charcoal, or coal. |
| Living Standards | Sanitation | sanitation | 0.056 | The household has unimproved or no sanitation facility or it is improved but shared with other households. |
| Living Standards | Drinking water | drinking_water | 0.056 | The household's source of drinking water is not safe or safe drinking water is a 30-minute or longer walk from home, roundtrip. |
| Living Standards | Electricity | electricity | 0.056 | The household has no electricity. |
| Living Standards | Housing | housing | 0.056 | The household has inadequate housing materials in any of the three components: floor, roof, or walls. |
| Living Standards | Assets | assets | 0.056 | The household does not own more than one of these assets: radio, TV, telephone, computer, animal cart, bicycle, motorbike, or refrigerator, and does not own a car or truck. |

### Using your own specification file

If you are computing a custom MPI, with different sets of dimensions and
indicators, create a CSV (or Excel) file that follows the same column
structure and load it the same way:

``` r

mpi_specs <- define_mpi_specs(
  mpi_specs_file = "path/to/my-specs.csv",
  uid = "household_id",
  poverty_cutoffs = 1/3 # default: a household needs >= 33% weighted deprivation score to be MPI poor
)
```

You can pass multiple cutoffs if you want to compare results across
different poverty thresholds:

``` r

mpi_specs <- define_mpi_specs(
  "path/to/my-specs.csv",
  uid = "household_id",
  poverty_cutoffs = c(0.20, 1/3, 0.50)   # 20%, 33%, 50%
)
```

------------------------------------------------------------------------

## Step 2: Prepare your data

### The two built-in datasets

The package includes synthetic household survey data modelled on the
Global MPI:

- **`df_household`** — one row per household, containing household-level
  variables.
- **`df_household_roster`** — one row per household member, containing
  individual-level variables (e.g. nutrition, school attendance).

``` r

library(dplyr)

glimpse(df_household)
#> Rows: 198
#> Columns: 21
#> $ uuid               <chr> "5dbec60a-ebda-47bd-ae18-3b017a221125", "8b70c208-8…
#> $ class              <chr> "Rural", "Rural", "Rural", "Rural", "Rural", "Rural…
#> $ drinking_water     <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ toilet             <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 4, 1, 1, 1, …
#> $ with_child_died    <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, …
#> $ roof               <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
#> $ walls              <int> 1, 1, 3, 3, 3, 5, 5, 1, 3, 1, 1, 4, 8, 2, 2, 10, 2,…
#> $ floor              <int> 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 5, 2, 4, 1, 2, …
#> $ electricity        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, …
#> $ cooking_fuel       <int> 3, 5, 5, 5, 5, 5, 5, 3, 3, 1, 3, 5, 5, 5, 5, 5, 5, …
#> $ asset_refrigerator <int> 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, …
#> $ asset_radio        <int> 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, …
#> $ asset_tv           <int> 1, 0, 0, 1, 1, 1, 1, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, …
#> $ asset_telephone    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ asset_mobile_phone <int> 4, 3, 2, 2, 5, 2, 1, 6, 2, 4, 1, 2, 3, 0, 0, 2, 0, …
#> $ asset_animal_cart  <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ asset_computer     <int> 0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ asset_motorcycle   <int> 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 0, …
#> $ asset_bicycle      <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, …
#> $ asset_car          <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ asset_truck        <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```

``` r

glimpse(df_household_roster)
#> Rows: 905
#> Columns: 8
#> $ uuid                     <chr> "5dbec60a-ebda-47bd-ae18-3b017a221125", "5dbe…
#> $ line_number              <int> 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 1, 2, 3, 4, 1, …
#> $ class                    <chr> "Rural", "Rural", "Rural", "Rural", "Rural", …
#> $ sex                      <chr> "Male", "Female", "Female", "Male", "Female",…
#> $ age                      <int> 55, 48, 17, 10, 30, 26, 3, 19, 66, 62, 34, 34…
#> $ attending_school         <int> NA, NA, 1, 1, NA, NA, NA, 1, NA, NA, NA, NA, …
#> $ undernourished           <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, …
#> $ completed_6yrs_schooling <int> 1, 1, 1, 2, 1, 1, NA, 2, 1, 1, 2, 1, 2, NA, 2…
```

> **Why two tables?** Some indicators (e.g. whether *any* child in the
> household is not attending school) come from individual-level data and
> must be collapsed to the household level. `mpindex` handles this
> automatically — you just tell it which table to use and how to
> collapse it.

------------------------------------------------------------------------

## Step 3: Compute the MPI

### The main function: `compute_mpi()`

[`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
takes your household data, your specifications, and a list of
**deprivation cutoffs** — one per indicator. Each cutoff is a logical
expression wrapped in
[`deprived()`](https://yng-me.github.io/mpindex/reference/deprived.md)
that evaluates to `TRUE` when a household is deprived.

``` r

mpi_result <- compute_mpi(
  df_household,
  mpi_specs = mpi_specs,
  deprivations = list(

    # --- Health ---
    nutrition = deprived(
      undernourished == 1 & age < 70,   # deprived if any member under 70 is undernourished
      .data        = df_household_roster,
      collapse_fn = max                 # household is deprived if any member is deprived
    ),
    child_mortality = deprived(with_child_died == 1),

    # --- Education ---
    year_schooling = deprived(
      completed_6yrs_schooling == 2,
      .data        = df_household_roster,
      collapse_fn = max
    ),
    school_attendance = deprived(
      attending_school == 2 & age %in% 5:24,
      .data        = df_household_roster,
      collapse_fn = max
    ),

    # --- Living Standards ---
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
```

The result is a named list:

``` r

names(mpi_result)
#> [1] "index"           "headcount_ratio" "contribution"
```

### Disaggregating by a subgroup

Pass `by` to break results down by a grouping variable (e.g. urban
vs. rural):

``` r

compute_mpi(
  df_household,
  mpi_specs = mpi_specs,
  deprivations = list(...),
  by = class        # column in df_household
)
```

### The step-by-step alternative

If you prefer to build and inspect each deprivation indicator before
combining them, use
[`define_deprivation()`](https://yng-me.github.io/mpindex/reference/define_deprivation.md)
to create each one separately, then pass the list to
[`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md)
via `deprivations`:

``` r

dp <- list()

dp$nutrition <- df_household_roster |>
  define_deprivation(
    indicator = nutrition,
    cutoff = undernourished == 1 & age < 70,
    collapse_fn = max,
    mpi_specs = mpi_specs
  )

dp$drinking_water <- df_household |>
  define_deprivation(
    indicator = drinking_water,
    cutoff = drinking_water == 2,
    mpi_specs = mpi_specs
  )

# ... define all remaining indicators, then:
mpi_result <- compute_mpi(df_household, mpi_specs = mpi_specs, deprivations = dp)
```

This is useful when your data preparation for some indicators is complex
and you want to examine intermediate results.

------------------------------------------------------------------------

## Step 4: Explore the results

### Understanding the output

`mpi_result` is a named list with varying components. Each component is
itself a named list keyed by the poverty cutoff used — e.g. `k_33` for
the 33% cutoff. If `include_deprivation_matrix` is set to `TRUE`,
`$deprivation_matrix` is added to the list. And if grouping is defined
in the `by` argument of
[`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md),
`$overall` is included in the list, showing the overall summary of the
group.

| Component | What it contains |
|----|----|
| `$index` | The headline MPI value, headcount ratio (H), intensity (A), and sample size (n) |
| `$contribution` | Each indicator’s percentage contribution to the overall MPI |
| `$headcount_ratio` | The share of households deprived on each indicator (uncensored and censored) |
| `$deprivation_matrix` | Row-level deprivation scores and indicator flags for every household if `include_deprivation_matrix` is set to `TRUE` (default is `FALSE`) |
| `$overall` | Overall summary if grouping is defined |

### The headline MPI

The three key numbers are:

- **H (headcount ratio)** — what share of the population is
  multidimensionally poor.
- **A (intensity)** — among the poor, what fraction of the weighted
  indicators are they deprived in on average.
- **MPI** — the product H × A, capturing both *how many* people are poor
  and *how poor* they are.

``` r

mpi_result$index$k_33
```

| MPI — 33% Poverty Cutoff |  |  |  |
|----|----|----|----|
| Number of households | Headcount Ratio (H) | Intensity of Deprivation Among the Poor (A) | MPI (H x A) |
| 198 | 0.379 | 0.472 | 0.179 |

### Indicator contributions

How much does each indicator drive the overall MPI? The contribution
table answers this — values sum to 100% across all indicators.

``` r

mpi_result$contribution$k_33
```

[TABLE]

### Headcount ratios

The **uncensored** headcount ratio shows the raw deprivation rate on
each indicator, regardless of whether a household crosses the poverty
line. The **censored** version counts only those households also
identified as multidimensionally poor.

``` r

mpi_result$headcount_ratio$uncensored   # deprivation rate — all households
mpi_result$headcount_ratio$k_33         # deprivation rate — poor households only
```

[TABLE]

[TABLE]

### The deprivation matrix

The deprivation matrix (added when `include_deprivation_matrix = TRUE`)
records each household’s individual score and indicator flags. The first
few rows:

``` r

mpi_result <- compute_mpi(
  df_household,
  mpi_specs = mpi_specs,
  deprivations = list(...),
  include_deprivation_matrix = TRUE
)

mpi_result$deprivation_matrix |> head()
```

[TABLE]

After applying the poverty cutoff, households with a deprivation score
below the threshold have all their indicator flags set to zero — they
are not counted as poor:

``` r

mpi_result$deprivation_matrix$k_33 |> head()
```

[TABLE]

> To save memory, you can exclude deprivation matrices from the output
> by setting `include_deprivation_matrix = FALSE` in
> [`compute_mpi()`](https://yng-me.github.io/mpindex/reference/compute_mpi.md).

------------------------------------------------------------------------

## Step 5: Save results to Excel

[`save_mpi()`](https://yng-me.github.io/mpindex/reference/save_mpi.md)
writes all results to a formatted Excel workbook:

``` r

save_mpi(mpi_result, mpi_specs = mpi_specs, filename = "MPI Results")
```

Each component gets its own sheet. To also include the specification
table as a reference:

``` r

save_mpi(
  mpi_result,
  mpi_specs = mpi_specs,
  filename = "MPI Results",
  include_specs = TRUE
)
```

------------------------------------------------------------------------

## Quick-reference script

Here is the complete workflow in one place:

``` r

library(mpindex)

# 1. Load specifications
mpi_specs <- global_mpi_specs(uid = "uuid", unit_of_analysis = 'households')

# 2. Compute the MPI
mpi_result <- compute_mpi(
  df_household,
  mpi_specs = mpi_specs,
  deprivations = list(
    nutrition = deprived(undernourished == 1 & age < 70, .data = df_household_roster, collapse_fn = max),
    child_mortality = deprived(with_child_died == 1),
    year_schooling = deprived(completed_6yrs_schooling == 2, .data = df_household_roster, collapse_fn = max),
    school_attendance = deprived(attending_school == 2 & age %in% 5:24, .data = df_household_roster, collapse_fn = max),
    cooking_fuel = deprived(cooking_fuel %in% c(4:6, 9)),
    sanitation = deprived(toilet > 1),
    drinking_water = deprived(drinking_water == 2),
    electricity = deprived(electricity == 2),
    housing = deprived(roof %in% c(5, 7, 9) |walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)),
    assets = deprived(
      !(asset_tv + 
        asset_telephone + 
        asset_mobile_phone + 
        asset_computer + 
        asset_animal_cart + 
        asset_bicycle + 
        asset_motorcycle + 
        asset_refrigerator) > 1 & 
      (asset_car + asset_truck) > 0
    )
  )
)

# 3. Inspect results
mpi_result$index$k_33          # headline MPI, H, A
mpi_result$contribution$k_33   # indicator contributions (%)
mpi_result$headcount_ratio$k_33
mpi_result$deprivation_matrix$k_33

# 4. Save to Excel
save_mpi(mpi_result, mpi_specs = mpi_specs, filename = "MPI Results")
```

------------------------------------------------------------------------

## Further reading

- [OPHI — How to Apply the Alkire-Foster
  Method](https://ophi.org.uk/research/multidimensional-poverty/how-to-apply-alkire-foster/)
- [Global MPI 2023
  Report](https://hdr.undp.org/content/2023-global-multidimensional-poverty-index-mpi)
- Package documentation:
  [`?compute_mpi`](https://yng-me.github.io/mpindex/reference/compute_mpi.md),
  [`?define_mpi_specs`](https://yng-me.github.io/mpindex/reference/define_mpi_specs.md),
  [`?define_deprivation`](https://yng-me.github.io/mpindex/reference/define_deprivation.md),
  [`?save_mpi`](https://yng-me.github.io/mpindex/reference/save_mpi.md)
