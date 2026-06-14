test_that("adjusted headcount ratio works correctly", {
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, mpi_specs = mpi_specs)

  ahr_uncensored <- dm$uncensored |>
    compute_headcount_ratio_adjusted()

  expect_equal(nrow(ahr_uncensored), 1)
  expect_equal(ncol(ahr_uncensored), 4)

  ahr_censored <- dm$k_33 |>
    compute_headcount_ratio_adjusted()

  expect_equal(nrow(ahr_censored), 1)
  expect_equal(ncol(ahr_censored), 4)
})


test_that("adjusted headcount ratio works correctly with grouping", {
  mpi_specs <- global_mpi_specs(uid = "uuid")
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, class, mpi_specs = mpi_specs)

  ahr_uncensored <- dm$uncensored |>
    compute_headcount_ratio_adjusted(class)

  expect_equal(nrow(ahr_uncensored), 2)
  expect_equal(ncol(ahr_uncensored), 5)

  ahr_censored <- dm$k_33 |>
    compute_headcount_ratio_adjusted(class)

  expect_equal(nrow(ahr_censored), 2)
  expect_equal(ncol(ahr_censored), 5)

  expect_contains(names(ahr_censored), "class")
})
