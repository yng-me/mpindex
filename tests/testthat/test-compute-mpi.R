test_that("mpi computation is implemented correctly", {
  mpi <- compute_mpi(df_simple, mpi_specs = specs_simple, deprivations = dp_simple)
  s <- mpi$contribution$k_50 |> dplyr::mutate(s = rowSums(dplyr::across(2:5)))
  expect_equal(s$s[1], 100, tolerance = 0.001)
  expect_equal(mpi$index$k_50[[4]][1], 0.375)
  expect_equal(mpi$index$k_50[[3]][1], 0.75)
  expect_equal(mpi$index$k_50[[2]][1], 0.5)
  expect_equal(
    mpi$index$k_50[[3]][1] * mpi$index$k_50[[2]][1],
    mpi$index$k_50[[4]][1]
  )
})


test_that("mpi computation works correctly", {
  mpi1 <- compute_mpi(df_household, mpi_specs = mpi_specs, deprivations = deprivation_profile,
                      include_deprivation_matrix = TRUE)

  expect_named(
    mpi1,
    c("index", "headcount_ratio", "contribution", "deprivation_matrix")
  )
  expect_named(mpi1$headcount_ratio, c("uncensored", "k_33"))
  expect_named(mpi1$deprivation_matrix, c("uncensored", "k_33"))

  expect_equal(nrow(mpi1$index$k_33), 1)
  expect_equal(ncol(mpi1$index$k_33), 4)
  expect_equal(nrow(mpi1$contribution$k_33), 1)
  expect_equal(ncol(mpi1$contribution$k_33), 11)
  expect_equal(nrow(mpi1$headcount_ratio$uncensored), 1)
  expect_equal(ncol(mpi1$headcount_ratio$uncensored), 11)
  expect_equal(nrow(mpi1$headcount_ratio$k_33), 1)
  expect_equal(ncol(mpi1$headcount_ratio$k_33), 11)

  expect_equal(nrow(mpi1$deprivation_matrix$uncensored), nrow(df_household))
  expect_equal(ncol(mpi1$deprivation_matrix$uncensored), 12)
  expect_equal(nrow(mpi1$deprivation_matrix$k_33), nrow(df_household))
  expect_equal(ncol(mpi1$deprivation_matrix$k_33), 12)

  mpi2 <- compute_mpi(
    df_household,
    mpi_specs = mpi_specs,
    deprivations = deprivation_profile,
    include_deprivation_matrix = FALSE
  )

  expect_named(mpi2, c("index", "headcount_ratio", "contribution"))
  expect_named(mpi2$headcount_ratio, c("uncensored", "k_33"))
  expect_equal(nrow(mpi2$index$k_33), 1)
  expect_equal(ncol(mpi2$index$k_33), 4)
  expect_equal(nrow(mpi2$contribution$k_33), 1)
  expect_equal(ncol(mpi2$contribution$k_33), 11)
  expect_equal(nrow(mpi2$headcount_ratio$uncensored), 1)
  expect_equal(ncol(mpi2$headcount_ratio$uncensored), 11)
  expect_equal(nrow(mpi2$headcount_ratio$k_33), 1)
  expect_equal(ncol(mpi2$headcount_ratio$k_33), 11)
})

test_that("mpi computation works correctly with different poverty cutoffs", {
  mpi_specs_local <- global_mpi_specs(
    uid = "uuid",
    poverty_cutoffs = c(1 / 3, 1 / 10, 9 / 10)
  )

  mpi <- compute_mpi(df_household, mpi_specs = mpi_specs_local, deprivations = deprivation_profile)

  expect_named(mpi$index, c("k_33", "k_10", "k_90"))
  expect_named(mpi$contribution, c("k_33", "k_10", "k_90"))
  expect_named(mpi$headcount_ratio, c("uncensored", "k_33", "k_10", "k_90"))

  expect_lte(mpi$index$k_33[4], mpi$index$k_10[4])
  expect_lte(mpi$index$k_90[4], mpi$index$k_33[4])
})


test_that("mpi computation works correctly with grouping and different poverty cutoffs", {
  mpi_specs_local <- global_mpi_specs(
    uid = "uuid",
    poverty_cutoffs = c(1 / 3, 1 / 10, 9 / 10)
  )

  mpi <- compute_mpi(
    df_household,
    mpi_specs = mpi_specs_local,
    deprivations = deprivation_profile,
    by = class
  )

  expect_named(mpi$index, c("k_33", "k_10", "k_90"))
  expect_named(mpi$contribution, c("k_33", "k_10", "k_90"))
  expect_named(mpi$headcount_ratio, c("uncensored", "k_33", "k_10", "k_90"))

  expect_equal(nrow(mpi$index$k_33), 2)
  expect_equal(ncol(mpi$index$k_33), 5)

  expect_equal(nrow(mpi$index$k_10), nrow(mpi$index$k_90))
  expect_equal(ncol(mpi$index$k_10), ncol(mpi$index$k_10))
})

test_that("compute_mpi_from_profile accepts a pre-built mpi_deprivation_matrix directly", {
  dm <- df_household |>
    create_deprivation_matrix(deprivation_profile, mpi_specs = mpi_specs)

  mpi_from_dm      <- dm |> compute_mpi_from_profile(mpi_specs = mpi_specs)
  mpi_from_profile <- compute_mpi(df_household, mpi_specs = mpi_specs, deprivations = deprivation_profile)

  expect_equal(mpi_from_dm$index, mpi_from_profile$index)
  expect_equal(mpi_from_dm$contribution, mpi_from_profile$contribution)
})

test_that("contributions sum to 100 across each dimension for multi-cutoff", {
  mpi_specs_multi <- global_mpi_specs(
    uid = "uuid",
    poverty_cutoffs = c(1 / 3, 1 / 2)
  )
  mpi <- compute_mpi(df_household, mpi_specs = mpi_specs_multi, deprivations = deprivation_profile)

  for (k in names(mpi$contribution)) {
    contrib <- mpi$contribution[[k]] |>
      dplyr::select(-dplyr::any_of(c("number_of_cases", "n")))
    row_sums <- rowSums(dplyr::select(contrib, dplyr::where(is.numeric)))
    expect_equal(row_sums, rep(100, nrow(contrib)), tolerance = 0.01)
  }
})

test_that("compute_mpi returns an mpi_output S3 object", {
  result <- compute_mpi(df_household, mpi_specs = mpi_specs, deprivations = deprivation_profile)
  expect_s3_class(result, "mpi_output")
})
