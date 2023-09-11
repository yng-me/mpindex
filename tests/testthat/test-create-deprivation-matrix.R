source('../deprivation_profile.R')

test_that("error if specs file is not defined", {
  expect_error(
    create_deprivation_matrix(df_household, deprivation_profile),
    'MPI specifications must be defined first.'
  )
})

test_that("deprivation profile is incomplete.", {
  deprivation_profile$year_schooling <- NULL
  expect_error(
    create_deprivation_matrix(df_household, deprivation_profile, .mpi_specs = mpi_specs),
    'Deprivation profile is incomplete.'
  )
})

test_that("deprivation matrix works correctly", {
  dm <- df_household |> create_deprivation_matrix(deprivation_profile, .mpi_specs = mpi_specs)
  expect_s3_class(dm, 'mpi_dep_matrix')
  expect_named(dm, c('uncensored', 'censored'))
  expect_length(names(dm$uncensored), 12)
  expect_length(names(dm$censored), 14)
  expect_equal(nrow(dm$uncensored), nrow(df_household))
  expect_equal(nrow(dm$censored), nrow(df_household))
})

test_that("aggregation works correctly", {
  mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid', .aggregation = 'class')
  dm <- df_household |> create_deprivation_matrix(deprivation_profile, .mpi_specs = mpi_specs)
  expect_length(names(dm$uncensored), 13)
  expect_length(names(dm$censored), 15)
  expect_contains(names(dm$censored), 'class')
  expect_contains(names(dm$uncensored), 'class')
})


test_that("works correctly using gradient poverty cutoffs", {
  mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid', .poverty_cutoffs = c(1/3, 1/10))
  dm <- df_household |> create_deprivation_matrix(deprivation_profile, .mpi_specs = mpi_specs)
  expect_named(dm, c('uncensored', 'k_33', 'k_10'))
  expect_length(names(dm$uncensored), 12)
  expect_length(names(dm$k_33), 14)
  expect_length(names(dm$k_10), 14)
})
