source('../deprivation_profile.R')

test_that("adjusted headcount ratio works correctly", {
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, .mpi_specs = mpi_specs)

  ahr_uncensored <- dm$uncensored |>
    compute_headcount_ratio_adj(.aggregation = mpi_specs$aggregation)

  expect_equal(nrow(ahr_uncensored), 1)
  expect_equal(ncol(ahr_uncensored), 4)

  ahr_censored <- dm$censored |>
    compute_headcount_ratio_adj(.aggregation = mpi_specs$aggregation)

  expect_equal(nrow(ahr_censored), 1)
  expect_equal(ncol(ahr_censored), 4)

})


test_that("adjusted headcount ratio works correctly with aggregation", {
  mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid', .aggregation = 'class')
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, .mpi_specs = mpi_specs)

  ahr_uncensored <- dm$uncensored |>
    compute_headcount_ratio_adj(.aggregation = mpi_specs$aggregation)

  expect_equal(nrow(ahr_uncensored), 2)
  expect_equal(ncol(ahr_uncensored), 5)

  ahr_censored <- dm$censored |>
    compute_headcount_ratio_adj(.aggregation = mpi_specs$aggregation)

  expect_equal(nrow(ahr_censored), 2)
  expect_equal(ncol(ahr_censored), 5)

  expect_contains(names(ahr_censored), 'class')

})
