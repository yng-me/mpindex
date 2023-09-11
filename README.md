
# Multidimensional Poverty Index (MPI) Package {mpindex}

This document presents an illustration of a simple application of `mpindex` package for computing the Multidimensional Poverty Index (MPI) using the Alkire-Foster (AF) counting method developed by Sabina Alkire and James Foster.

## 1. Installation

To install the `mpindex` package from CRAN:

```r
install.packages('mpindex')
```

If you want to get the latest development version of `mpindex`, install it from GitHub. Note that you may need to install first `devtools`.

```r
devtools::install_github('yng-me/mpindex')
```

Load the package once you have successfully completed the installation.

```r
library(mpindex)
```

## 2. MPI specifications

The initial step is to prepare an MPI specification file which will serve as references in the computation as well as generation of output in the later part of the process. This file should contain information about MPI dimensions, indicators and their corresponding weights.

This file should also be easy to create using the most common and accessible file types such as `.xlsx` (Excel), `.json`, `.csv`, or `.txt` (TSV). 

### Built-in specification files

For convenience, `mpindex` has included built-in specification files (in different formats). Each file contains dimensions, indicators, weight, and other relevant information of the Global MPI. 

To see the list of files available:

```r
system.file("extdata", package = "mpindex") |> list.files()
```

To use a built-in specification file, say the `.csv` file, use below script to first get the full path of the file.

```r
specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
```

### User-defined specification file

You can also define and create your own specification file if you prefer to or if you happen to use different sets of dimensions and indicators. At the minimum, however, this file should contain the following columns/variables: 

- `Dimension`, 
- `Indicator`, 
- `Variable`, 
- `Weight`,
- `Description` (optional). 

Note that the order in which you put these columns does not matter and also the names are not case sensitive, but make sure to spell the column names correctly.

You may download the template here if you do not want to start from scratch: [MPI specification file sample](https://github.com/yng-me/mpindex/blob/main/inst/extdata/mpi-specs-sample.csv).


### Using `define_mpi_specs`

Once you have prepared the specification file, load it using the `define_mpi_specs` function (type `?define_mpi_specs` for a help text).

For demonstration purposes, we will use the built-in specification file, discussed above.

```r
specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
mpi_specs <- define_mpi_specs(specs_file)
```

`define_mpi_specs` requires a specification file as its first argument. The default poverty cutoff is set to `1/3` (based on Global MPI), with a unit of analysis set to `households`. You can also define a list of poverty cutoffs by specifying in the `.poverty_cutoffs` argument to achieve gradient MPIs.

```r
mpi_specs <- define_mpi_specs(
  .mpi_specs_file = specs_file, 
  .poverty_cutoffs = c(1/3, 0.2, 0.8)
)
```

If your dataset contains unique ID, like `uuid`, it is recommended to define it here using the `.uid` argument. 

```r
mpi_specs <- define_mpi_specs(
  .mpi_specs_file = specs_file, 
  .uid = 'uuid'
)
```

You can also set the aggregation level. Make sure it corresponds to the column name present in your dataset (see `?df_househod` and more below).

```r
mpi_specs <- define_mpi_specs(
  .mpi_specs_file = specs_file, 
  .poverty_cutoffs = c(1/3, 0.2, 0.8),
  .uid = 'uuid',
  .aggregation = 'class'
)
```

`.unit_of_analysis`, `.source_of_data`, and `.names_separator` are merely used for auto labels when generating the output later.


### Make the specs available globally

It is also recommended to make the MPI specification available globally, rather than providing the argument manually everytime you use other functions within the `mpindex` (more of this later).

```r
options(mpi_specs = mpi_specs)
```

Note: the name to be passed to `options` must exactly be `mpi_specs`.

## 3. Data preparation

The user of `mpindex` is assumed to have basic familiarity with the concept of tidy data as well as able to perform data wrangling and transformation using the `tidyverse` ecosystem. Under the hood, `mpindex` uses `dplyr` verbs to perform data manipulation.


We also assume that your dataset is already tidy and ready for analysis. See [R for Data Science](https://r4ds.had.co.nz/tidy-data.html) by Hadley Wickham and Garrett Grolemund if you need a refresher.

### Dataset

For this demonstration, we will use two (2) synthetic datasets available within the package:

- `df_household` household-level data (type `?df_household` for more info)
- `df_household_roster` individual-level data (type `?df_household_roster` for more info)


## 4. Create deprivation profile

### Using `define_deprivation`

First, we need to create an empty list, and name it `deprivation_profile` (but feel free to name it whatever you like). 

```r
deprivation_profile <- list()
```

To create a deprivation profile for each indicator, we use the `define_deprivation` function (see `?define_deprivation`) and add to the `deprivation_profile` list we created above. Make sure that the deprivation profile for each indicator matches the variable name declared in the specification file.

### 1. Heath dimension

#### 1.1. Nutrition

For this indicator, we use the `df_household_roster` dataset. By default, `define_deprivation` sets the `.collapse = FALSE`. Since we need to collapse it to the household level, we need to set `.collapse = TRUE`.


```r
deprivation_profile$nutrition <- df_household_roster |> 
  define_deprivation(
    .indicator = nutrition,
    .cutoff = undernourished == 1 & age < 70,
    .collapse = TRUE
  )
```

#### 1.2. Child mortality

For child mortality, we use the `df_household` dataset. But unlike in `nutrition`, we do not need to provide the `.collapse` argument since it is not applicable here.

```r
deprivation_profile$child_mortality <- df_household |> 
  define_deprivation(
    .indicator = child_mortality,
    .cutoff = with_child_died == 1
  )
```

### 2. Education dimension


#### 2.1. Years of schooling

```r
deprivation_profile$year_schooling <- df_household_roster |> 
  define_deprivation(
    .indicator = year_schooling,
    .cutoff = completed_6yrs_schooling == 2,
    .collapse = TRUE
  )
```

#### 2.2. School attendance

```r
deprivation_profile$school_attendance <- df_household_roster |> 
  define_deprivation(
    .indicator = school_attendance,
    .cutoff = attending_school == 2 & age %in% c(5:24),
    .collapse = TRUE
  )
```

### 3. Living standards dimension

#### 3.1. Cooking fuel

```r
deprivation_profile$cooking_fuel <- df_household |> 
  define_deprivation(
    .indicator = cooking_fuel,
    .cutoff = cooking_fuel %in% c(4:6, 9)
  )
```


#### 3.2. Sanitation

```r
deprivation_profile$sanitation <- df_household |> 
  define_deprivation(
    .indicator = sanitation,
    .cutoff = toilet > 1
  )
```


#### 3.3. Drinking water

```r
deprivation_profile$drinking_water <- df_household |> 
  define_deprivation(
    .indicator = drinking_water,
    .cutoff = drinking_water == 2
  )
```

#### 3.4. Electricity

```r
deprivation_profile$electricity <- df_household |> 
  define_deprivation(
    .indicator = electricity,
    .cutoff = electricity == 2
  )
```

#### 3.5. Housing

```r
deprivation_profile$housing <- df_household |> 
  define_deprivation(
    .indicator = housing,
    .cutoff = roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
  )
```


#### 3.6. Assets

For this indicator, we need additional transformation.

```r
deprivation_profile$assets <- df_household |> 
  mutate_at(vars(starts_with('asset_')), ~ if_else(. > 0, 1L, 0L)) |> 
  mutate(
    asset_phone = if_else(
      (asset_telephone + asset_mobile_phone) > 0, 
      1L, 
      0L
    )
  ) |> 
  mutate(
    with_hh_conveniences = (
      asset_tv + asset_phone + asset_computer + 
        asset_animal_cart + asset_bicycle + 
        asset_motorcycle + asset_refrigerator) > 1,
    with_mobility_assets = (asset_car + asset_truck) > 0
  ) |> 
  define_deprivation(
    .indicator = assets,
    .cutoff = !(with_hh_conveniences & with_mobility_assets)
  )
```

## 4. Computing the MPI

### Using `compute_mpi`

After completing the deprivation profile, use the `compute_mpi` function and pass the `deprivation_profile` list you created above.

```r
mpi_result <- df_household |>
  compute_mpi(deprivation_profile)
```

## 5. Save output

You may also save your output into an Excel file.

```r
save_mpi(mpi_result, .filename = 'MPI Sample Output')
```


## Full script

```r
# ----------------------------------
# Load MPI specs from the built-in specs file
specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid')
options(mpi_specs = mpi_specs)

# ----------------------------------
# Create an empty list to store deprivation profile for each indicator
deprivation_profile <- list()

deprivation_profile$nutrition <- df_household_roster |>
  define_deprivation(
   .indicator = nutrition,
   .cutoff = undernourished == 1 & age < 70,
   .collapse = TRUE
  )

deprivation_profile$child_mortality <- df_household |>
  define_deprivation(
   .indicator = child_mortality,
   .cutoff = with_child_died == 1
  )

deprivation_profile$year_schooling <- df_household_roster |>
  define_deprivation(
   .indicator = year_schooling,
   .cutoff = completed_6yrs_schooling == 2,
   .collapse = TRUE
  )

deprivation_profile$school_attendance <- df_household_roster |>
  define_deprivation(
   .indicator = school_attendance,
   .cutoff = attending_school == 2 & age %in% c(5:24),
   .collapse = TRUE
  )

deprivation_profile$cooking_fuel <- df_household |>
  define_deprivation(
   .indicator = cooking_fuel,
   .cutoff = cooking_fuel %in% c(4:6, 9)
  )

deprivation_profile$sanitation <- df_household |>
  define_deprivation(
   .indicator = sanitation,
   .cutoff = toilet > 1
  )

deprivation_profile$drinking_water <- df_household |>
  define_deprivation(
   .indicator = drinking_water,
   .cutoff = drinking_water == 2
  )

deprivation_profile$electricity <- df_household |>
  define_deprivation(
   .indicator = electricity,
   .cutoff = electricity == 2
  )

deprivation_profile$housing <- df_household |>
  define_deprivation(
   .indicator = housing,
   .cutoff = roof %in% c(5, 7, 9) | 
     walls %in% c(5, 8, 9, 99) == 2 | 
     floor %in% c(5, 6, 9)
  )

deprivation_profile$assets <- df_household |>
  dplyr::mutate_at(
    dplyr::vars(dplyr::starts_with('asset_')), 
    ~ dplyr::if_else(. > 0, 1L, 0L)
  ) |>
  dplyr::mutate(
   asset_phone = dplyr::if_else(
     (asset_telephone + asset_mobile_phone) > 0,
     1L,
     0L
   )
  ) |>
  dplyr::mutate(
   with_hh_conveniences = (
     asset_tv + asset_phone + asset_computer +
       asset_animal_cart + asset_bicycle +
       asset_motorcycle + asset_refrigerator) > 1,
   with_mobility_assets = (asset_car + asset_truck) > 0
  ) |>
  define_deprivation(
   .indicator = assets,
   .cutoff = !(with_hh_conveniences & with_mobility_assets)
  )

# ----------------------------------
# Compute the MPI
mpi_result <- df_household |>
  compute_mpi(deprivation_profile)

# ----------------------------------
# You may also save your output into an Excel file
save_mpi(mpi_result, .filename = 'MPI Sample Output')
