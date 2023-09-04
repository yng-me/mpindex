# Multidimensional Poverty Index

## 1. Load dataset

```r
mpi_dataset <- openxlsx::read.xlsx('./tests/data/mpi-dataset-sample.xlsx')
```

## 2. Define dimensions, indicators, and weights

There are multiple ways to define dimensions, indicators, and weights to be used in the MPI calculation. Choose one of the following methods (in order of preference).

The following examples use the MPI definition for the Philippines's MPI Interim Methodology. The MPI definition for the Philippines is available in the [MPI Philippines 2017 report](https://www.mppn.org/wp-content/uploads/2018/11/Philippines-mpi-technical-notes.pdf) (see page 8).

- Excel / CSV (sample file: [mpi-specs-sample.xlsx]('https://github.com/yng-me/mpi/blob/main/tests/data/mpi-specs-sample.xlsx'))
  
  | Dimension | Indicator | Weight | Variable | Description |
  |-----------|-----------|--------|----------|-------------|
  | Education | School attendance | 0.125 | school_attendace | If any child in the family aged 5 to 17 age child not attending years old is not school is currently attending school |
  | ... | ... | ... | ... |
  | Employment | Working children not in school | 0.125 | working_children | If any family member aged 5 to 17 years old is working and not currently attending school |

  Note: The column names must be exactly the same as the sample file (but not case sensitive). The order of the columns does not matter. Description is optional.

- Using JSON (sample file: [mpi-specs-sample.json]('https://github.com/yng-me/mpi/blob/main/tests/data/mpi-specs-sample.json'))
  ```json
  [-
      {
        "Dimension": "Education ",
        "Indicator": "School attendance",
        "Weight": 0.125,
        "Variable": "school_attendance",
        "Description": "If any child in the family aged 5 to 17 years old not currently attending school"
      },
       //...
      {
        "Dimension": "Employment",
        "Indicator": "Working children not in school",
        "Weight": 0.125,
        "Variable": "working_children",
        "Description": "If any family member aged 5 to 17 years old is working and not currently attending school"
      }
    ]
  ```

```r
mpi_definition <- define_mpi(
  .definition_file = './tests/data/mpi-specs-sample.xlsx',
  .poverty_cutoffs = c(1/3, 0.2, 0.4, 0.6, 0.8),
  .row_id = 'case_id',
)
```

## 2. Define poverty cutoffs

```r
water <- mpi_dataset |>
  define_deprivation_cutoff(
    hunger, 
    q03_improved_drinking_water == 2, 
    mpi_definition
  )

housing_materials <- mpi_dataset |>
  define_deprivation_cutoff(
    housing_materials, 
    r03_roof %in% c(5, 7, 9) & r04_outer_walls %in% c(5, 8, 9, 99), 
    mpi_definition
  )
```

## 3. Calculate MPI
