test_that("deprivation profile is incomplete.", {
  deprivation_profile$year_schooling <- NULL
  expect_error(
    create_deprivation_matrix(
      df_household, deprivation_profile,
      mpi_specs = mpi_specs
    ),
    "Deprivation profile is incomplete."
  )
})

test_that("deprivation matrix works correctly", {
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, mpi_specs = mpi_specs)
  expect_s3_class(dm, "mpi_dm")
  expect_named(dm, c("uncensored", "k_33"))
  expect_length(names(dm$uncensored), 12)
  expect_length(names(dm$k_33), 14)
  expect_equal(nrow(dm$uncensored), nrow(df_household))
  expect_equal(nrow(dm$k_33), nrow(df_household))
})

test_that("grouping works correctly", {
  mpi_specs <- global_mpi_specs(uid = "uuid")
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, class, mpi_specs = mpi_specs)
  expect_length(names(dm$uncensored), 13)
  expect_length(names(dm$k_33), 15)
  expect_contains(names(dm$k_33), "class")
  expect_contains(names(dm$uncensored), "class")
})


test_that("works correctly using gradient poverty cutoffs", {
  mpi_specs <- global_mpi_specs(
    uid = "uuid",
    poverty_cutoffs = c(1 / 3, 1 / 10)
  )

  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, mpi_specs = mpi_specs)
  expect_named(dm, c("uncensored", "k_33", "k_10"))
  expect_length(names(dm$uncensored), 12)
  expect_length(names(dm$k_33), 14)
  expect_length(names(dm$k_10), 14)
})
