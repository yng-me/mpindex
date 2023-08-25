# Multidimensional Poverty Index

# Multidimensional Poverty Index

## 1. Define dimensions, indicators, and weights

There are multiple ways to define dimensions, indicators, and weights to be used in the MPI calculation. Choose one of the following methods (in order of preference):

- Excel / CSV (sample file: [mpi_define_sample.xlsx]('./test/data/mpi_define_sample.xlsx'))
  
  | Dimension | Indicator | Weight | Description |
  |-----------|-----------|--------|-------------|
  | Education | School attendance | 0.125 | If any child in the family aged 5 to 17 age child not attending years old is not school is currently attending school |
  | ... | ... | ... | ... |
  | Employment | Working children not in school | 0.125 | If any family member aged 5 to 17 years old is working and not currently attending school |

  Note: The column names must be exactly the same as the sample file (but not case sensitive). The order of the columns does not matter. Description is optional.

- Using JSON file (sample file: [mpi_define_sample.json]('./test/data/mpi_define_sample.json'))
  ```json
  [
      {
        "Dimension": "Education ",
        "Indicator": "School attendance",
        "Weight": 0.125,
        "Description": "If any child in the family aged 5 to 17 age child not attending years old is not school is currently attending school"
      },
       //...
      {
        "Dimension": "Employment",
        "Indicator": "Working children not in school",
        "Weight": 0.125,
        "Description": "If any family member aged 5 to 17 years old is working and not currently attending school"
      }
    ]
  ```
- Manually define in R

   ```r
     mpi_define <- data.frame(
      Dimension = c(
        "Education", 
        "Education", 
        "Health and nutrition", 
        "Health and nutrition", 
        "Health and nutrition", 
        "Housing, Water, and Sanitation", 
        "Housing, Water, and Sanitation", 
        "Housing, Water, and Sanitation", 
        "Housing, Water, and Sanitation", 
        "Housing, Water, and Sanitation", 
        "Housing, Water, and Sanitation", 
        "Employment", 
        "Employment"
      ),
      Indicator = c(
        "School attendance", 
        "Educational attainment", 
        "Hunger", 
        "Food consumption", 
        "Health insurance", 
        "Assets", 
        "Toilet", 
        "Water", 
        "Tenure", 
        "Housing materials", 
        "Electricity", 
        "Underemployment", 
        "Working children not in school"
      ),
      Weight = c(0.125, 0.125, 0.0833, 0.083, 0.0833, 0.0417, 0.0417, 0.0417, 0.0417, 0.0417, 0.0417, 0.125, 0.125),
      # Description (optional)
   )
   ```

:::INFO
The following examples use the MPI definition for the Philippines's MPI Interim Methodology. The MPI definition for the Philippines is available in the [MPI Philippines 2017 report](https://www4.hks.harvard.edu/cid/programs/building-state-capabilities/philippines-mpi-report-2017.pdf) (see page 8).
:::

## 2. Define poverty cutoffs

## 3. Calculate MPI
