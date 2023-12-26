mpi_specs <- use_global_mpi_specs(.uid = "uuid")

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
