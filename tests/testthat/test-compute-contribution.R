test_that("contribution by dimension works correctly", {
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, mpi_specs = mpi_specs)

  hr <- dm$k_33 |>
    compute_headcount_ratio()

  m_0 <- dm$k_33 |>
    compute_headcount_ratio_adjusted() |>
    dplyr::select(mpi) |>
    dplyr::bind_cols(hr) |>
    compute_contribution(mpi_specs = mpi_specs)

  expect_equal(nrow(m_0), 1)
  expect_equal(ncol(m_0), 11)

  s <- m_0 |> dplyr::mutate(s = rowSums(dplyr::across(2:11)))
  expect_equal(s$s[1], 100, tolerance = 0.001)
})


test_that("contribution by dimension works correctly with grouping", {
  mpi_specs <- global_mpi_specs(uid = "uuid")

  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, class, mpi_specs = mpi_specs)

  hr <- dm$k_33 |>
    compute_headcount_ratio(class)

  m_0 <- dm$k_33 |>
    compute_headcount_ratio_adjusted(class) |>
    dplyr::select(mpi) |>
    dplyr::bind_cols(hr) |>
    compute_contribution(class, mpi_specs = mpi_specs)

  expect_equal(nrow(m_0), 2)
  expect_equal(ncol(m_0), 12)
})
