specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")
mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid')

test_that("dimension is returned correctly", {
  dp <- df_household |>
    define_deprivation(
      .indicator = drinking_water,
      .cutoff = drinking_water == 2,
      .mpi_specs = mpi_specs
    )
  expect_equal(nrow(dp), nrow(df_household))
  expect_equal(ncol(dp), 3)
})


test_that("error if specs file is not defined", {
  expect_error(
    define_deprivation(df_household, drinking_water, drinking_water == 2),
    'MPI specifications must be defined first.'
  )
})

test_that("collapsing is correctly implemented", {
  dp <- df_household_roster |>
    define_deprivation(
      .indicator = nutrition,
      .cutoff = undernourished == 1 & age < 70,
      .collapse = TRUE,
      .mpi_specs = mpi_specs
    )
  expect_equal(nrow(dp), nrow(df_household))
})
