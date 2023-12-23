source("../deprivation-profile.R")
source("../deprivation-profile-simple.R")

test_that("mpi computation is implemented correctly", {
  mpi <- df_simple |> compute_mpi(dp_simple, .mpi_specs = specs_simple)
  s <- mpi$contribution |> dplyr::mutate(s = rowSums(dplyr::across(2:5)))
  expect_equal(s$s[1], 100, tolerance = 0.001)
  expect_equal(mpi$index[[4]][1], 0.375)
  expect_equal(mpi$index[[3]][1], 0.75)
  expect_equal(mpi$index[[2]][1], 0.5)
  expect_equal(
    mpi$index[[3]][1] * mpi$index[[2]][1],
    mpi$index[[4]][1]
  )
})


test_that("mpi computation works correctly", {
  mpi1 <- df_household |>
    compute_mpi(deprivation_profile, .mpi_specs = mpi_specs)

  expect_named(
    mpi1,
    c("index", "contribution", "headcount_ratio", "deprivation_matrix")
  )
  expect_named(mpi1$headcount_ratio, c("uncensored", "censored"))
  expect_named(mpi1$deprivation_matrix, c("uncensored", "censored"))

  expect_equal(nrow(mpi1$index), 1)
  expect_equal(ncol(mpi1$index), 4)
  expect_equal(nrow(mpi1$contribution), 1)
  expect_equal(ncol(mpi1$contribution), 11)
  expect_equal(nrow(mpi1$headcount_ratio$uncensored), 1)
  expect_equal(ncol(mpi1$headcount_ratio$uncensored), 11)
  expect_equal(nrow(mpi1$headcount_ratio$censored), 1)
  expect_equal(ncol(mpi1$headcount_ratio$censored), 11)

  expect_equal(nrow(mpi1$deprivation_matrix$uncensored), nrow(df_household))
  expect_equal(ncol(mpi1$deprivation_matrix$uncensored), 12)
  expect_equal(nrow(mpi1$deprivation_matrix$censored), nrow(df_household))
  expect_equal(ncol(mpi1$deprivation_matrix$censored), 12)

  mpi2 <- df_household |> compute_mpi(
    deprivation_profile,
    .mpi_specs = mpi_specs,
    .include_deprivation_matrix = FALSE
  )
  expect_named(mpi2, c("index", "contribution", "headcount_ratio"))
  expect_named(mpi2$headcount_ratio, c("uncensored", "censored"))
  expect_equal(nrow(mpi2$index), 1)
  expect_equal(ncol(mpi2$index), 4)
  expect_equal(nrow(mpi2$contribution), 1)
  expect_equal(ncol(mpi2$contribution), 11)
  expect_equal(nrow(mpi2$headcount_ratio$uncensored), 1)
  expect_equal(ncol(mpi2$headcount_ratio$uncensored), 11)
  expect_equal(nrow(mpi2$headcount_ratio$censored), 1)
  expect_equal(ncol(mpi2$headcount_ratio$censored), 11)
})

test_that("mpi computation works correctly with different poverty cutoffs", {
  mpi_specs <- use_global_mpi_specs(
    .uid = "uuid",
    .poverty_cutoffs = c(1 / 3, 1 / 10, 9 / 10)
  )

  mpi <- df_household |>
    compute_mpi(deprivation_profile, .mpi_specs = mpi_specs)

  expect_named(mpi$index, c("k_33", "k_10", "k_90"))
  expect_named(mpi$contribution, c("k_33", "k_10", "k_90"))
  expect_named(mpi$headcount_ratio, c("uncensored", "k_33", "k_10", "k_90"))

  expect_lte(mpi$index$k_33[4], mpi$index$k_10[4])
  expect_lte(mpi$index$k_90[4], mpi$index$k_33[4])
})


test_that("mpi computation works correctly with aggregation and different poverty cutoffs", {
  mpi_specs <- use_global_mpi_specs(
    .uid = "uuid",
    .aggregation = "class",
    .poverty_cutoffs = c(1 / 3, 1 / 10, 9 / 10)
  )

  mpi <- df_household |>
    compute_mpi(deprivation_profile, .mpi_specs = mpi_specs)

  expect_named(mpi$index, c("k_33", "k_10", "k_90"))
  expect_named(mpi$contribution, c("k_33", "k_10", "k_90"))
  expect_named(mpi$headcount_ratio, c("uncensored", "k_33", "k_10", "k_90"))

  expect_equal(nrow(mpi$index$k_33), 2)
  expect_equal(ncol(mpi$index$k_33), 5)

  expect_equal(nrow(mpi$index$k_10), nrow(mpi$index$k_90))
  expect_equal(ncol(mpi$index$k_10), ncol(mpi$index$k_10))
})
