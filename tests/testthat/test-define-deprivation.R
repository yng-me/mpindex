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

test_that("indicator not in specs raises a clear error", {
  expect_error(
    df_household |>
      define_deprivation(
        .indicator = not_a_real_var,
        .cutoff = not_a_real_var == 1,
        .mpi_specs = mpi_specs
      ),
    "not a valid indicator"
  )
})

test_that(".set_na_equal_to = 1 treats NA as deprived", {
  # Use specs without .uid so no uid column is required in the test data frame
  specs_no_uid <- use_global_mpi_specs()
  df_with_na <- data.frame(drinking_water = c(2, NA, 1, 2))

  dp_na_1 <- df_with_na |>
    define_deprivation(
      .indicator = drinking_water,
      .cutoff = drinking_water == 2,
      .mpi_specs = specs_no_uid,
      .set_na_equal_to = 1
    )
  dp_na_0 <- df_with_na |>
    define_deprivation(
      .indicator = drinking_water,
      .cutoff = drinking_water == 2,
      .mpi_specs = specs_no_uid,
      .set_na_equal_to = 0
    )

  col <- grep("_unweighted$", names(dp_na_1), value = TRUE)
  expect_equal(dp_na_1[[col]][2], 1)
  expect_equal(dp_na_0[[col]][2], 0)
})
