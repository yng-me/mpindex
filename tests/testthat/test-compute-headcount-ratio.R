test_that("headcount ratio works correctly", {
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, mpi_specs = mpi_specs)

  hr_uncensored <- dm$uncensored |>
    compute_headcount_ratio()

  expect_equal(nrow(hr_uncensored), 1)
  expect_equal(ncol(hr_uncensored), 11)

  hr_censored <- dm$k_33 |>
    compute_headcount_ratio()

  expect_equal(nrow(hr_censored), 1)
  expect_equal(ncol(hr_censored), 11)
})


test_that("headcount ratio works correctly with grouping", {
  mpi_specs <- global_mpi_specs(uid = "uuid")
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, class, mpi_specs = mpi_specs)

  hr_uncensored <- dm$uncensored |>
    compute_headcount_ratio(class)

  expect_equal(nrow(hr_uncensored), 2)
  expect_equal(ncol(hr_uncensored), 12)

  hr_censored <- dm$k_33 |>
    compute_headcount_ratio(class)

  expect_equal(nrow(hr_censored), 2)
  expect_equal(ncol(hr_censored), 12)

  expect_contains(names(hr_censored), "class")
})
