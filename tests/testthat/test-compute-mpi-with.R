test_that("compute_mpi with deprived() produces the same result as with pre-built profile", {
  profile_result <- compute_mpi(
    df_simple,
    mpi_specs = specs_simple,
    deprivations = dp_simple
  )

  inline_result <- compute_mpi(
    df_simple,
    mpi_specs = specs_simple,
    deprivations = list(
      a1 = deprived(a1 == 1),
      a2 = deprived(a2 == 1),
      b1 = deprived(b1 == 1),
      b2 = deprived(b2 == 1)
    )
  )

  expect_equal(inline_result$index, profile_result$index)
  expect_equal(inline_result$contribution, profile_result$contribution)
  expect_equal(inline_result$headcount_ratio, profile_result$headcount_ratio)
})

test_that("compute_mpi returns an mpi_output object", {
  result <- compute_mpi(
    df_simple,
    mpi_specs = specs_simple,
    deprivations = list(
      a1 = deprived(a1 == 1),
      a2 = deprived(a2 == 1),
      b1 = deprived(b1 == 1),
      b2 = deprived(b2 == 1)
    )
  )

  expect_s3_class(result, "mpi_output")
  expect_named(result, c("index", "headcount_ratio", "contribution"))
})

test_that("compute_mpi raises an error for an indicator not in specs", {
  expect_error(
    compute_mpi(
      df_simple,
      mpi_specs = specs_simple,
      deprivations = list(
        a1 = deprived(a1 == 1),
        not_a_real_indicator = deprived(a2 == 1)
      )
    ),
    regexp = "not found in specs"
  )
})

test_that("compute_mpi accepts a per-indicator .data override via deprived()", {
  df_ab_only <- df_simple[, c("a1", "a2")]
  df_b_only  <- df_simple[, c("b1", "b2")]

  inline_result <- compute_mpi(
    df_simple,
    mpi_specs = specs_simple,
    deprivations = list(
      a1 = deprived(a1 == 1, .data = df_ab_only),
      a2 = deprived(a2 == 1, .data = df_ab_only),
      b1 = deprived(b1 == 1, .data = df_b_only),
      b2 = deprived(b2 == 1, .data = df_b_only)
    )
  )

  manual_result <- compute_mpi(df_simple, mpi_specs = specs_simple, deprivations = dp_simple)

  expect_equal(inline_result$index, manual_result$index)
})

test_that("compute_mpi passes .by grouping to the output", {
  df_grouped <- df_simple |>
    dplyr::mutate(region = c("X", "X", "Y", "Y"), .before = 1)

  dp_grouped <- list()
  dp_grouped$a1 <- df_grouped |>
    define_deprivation(a1, a1 == 1, mpi_specs = specs_simple)
  dp_grouped$a2 <- df_grouped |>
    define_deprivation(a2, a2 == 1, mpi_specs = specs_simple)
  dp_grouped$b1 <- df_grouped |>
    define_deprivation(b1, b1 == 1, mpi_specs = specs_simple)
  dp_grouped$b2 <- df_grouped |>
    define_deprivation(b2, b2 == 1, mpi_specs = specs_simple)

  manual_result <- compute_mpi(
    df_grouped,
    mpi_specs = specs_simple,
    deprivations = dp_grouped,
    by = region
  )

  inline_result <- compute_mpi(
    df_grouped,
    mpi_specs = specs_simple,
    deprivations = list(
      a1 = deprived(a1 == 1),
      a2 = deprived(a2 == 1),
      b1 = deprived(b1 == 1),
      b2 = deprived(b2 == 1)
    ),
    by = region
  )

  expect_equal(inline_result$index, manual_result$index)
})

test_that("deprived() with .collapse_fn collapses roster to household level", {
  specs <- global_mpi_specs(uid = "uuid")

  result <- compute_mpi(
    df_household,
    mpi_specs = specs,
    deprivations = list(
      nutrition = deprived(
        undernourished == 1 & age < 70,
        .data = df_household_roster,
        collapse_fn = max
      ),
      child_mortality   = deprived(with_child_died == 1),
      year_schooling    = deprived(
        completed_6yrs_schooling == 2,
        .data = df_household_roster,
        collapse_fn = max
      ),
      school_attendance = deprived(
        attending_school == 2 & age %in% c(5:24),
        .data = df_household_roster,
        collapse_fn = max
      ),
      cooking_fuel   = deprived(cooking_fuel %in% c(4:6, 9)),
      sanitation     = deprived(toilet > 1),
      drinking_water = deprived(drinking_water == 2),
      electricity    = deprived(electricity == 2),
      housing        = deprived(
        roof %in% c(5, 7, 9) | walls %in% c(5, 8, 9, 99) == 2 | floor %in% c(5, 6, 9)
      ),
      assets = deprived(!(with_child_died == 1))  # simplified stand-in
    )
  )

  expect_s3_class(result, "mpi_output")
  expect_named(result$index, "k_33")
})

