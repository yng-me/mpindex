source('../deprivation_profile.R')

test_that("contribution by dimension works correctly", {
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, .mpi_specs = mpi_specs)

  hr <- dm$censored |>
    compute_headcount_ratio(.aggregation = mpi_specs$aggregation)

  m_0 <- dm$censored |>
    compute_headcount_ratio_adj(.aggregation = mpi_specs$aggregation) |>
    dplyr::select(MPI) |>
    dplyr::bind_cols(hr) |>
    compute_contribution(.mpi_specs = mpi_specs)

  expect_equal(nrow(m_0), 1)
  expect_equal(ncol(m_0), 11)

  s <- m_0 |> dplyr::mutate(s = rowSums(dplyr::across(2:11)))
  expect_equal(s$s[1], 100, tolerance = 0.001)

})


test_that("contribution by dimension works correctly with aggregation", {
  mpi_specs <- define_mpi_specs(specs_file, .uid = 'uuid', .aggregation = 'class')
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, .mpi_specs = mpi_specs)

  hr <- dm$censored |>
    compute_headcount_ratio(.aggregation = mpi_specs$aggregation)

  m_0 <- dm$censored |>
    compute_headcount_ratio_adj(.aggregation = mpi_specs$aggregation) |>
    dplyr::select(MPI) |>
    dplyr::bind_cols(hr) |>
    compute_contribution(.mpi_specs = mpi_specs)

  expect_equal(nrow(m_0), 2)
  expect_equal(ncol(m_0), 12)

})
