test_that("compute_mpi_with produces the same output as the manual 3-step workflow", {
  manual_result <- df_simple |>
    compute_mpi(dp_simple, .mpi_specs = specs_simple)

  wrapper_result <- compute_mpi_with(
    .data = df_simple,
    .mpi_specs = specs_simple,
    .deprivations = list(
      a1 = list(.cutoff = quote(a1 == 1)),
      a2 = list(.cutoff = quote(a2 == 1)),
      b1 = list(.cutoff = quote(b1 == 1)),
      b2 = list(.cutoff = quote(b2 == 1))
    )
  )

  expect_equal(wrapper_result$index, manual_result$index)
  expect_equal(wrapper_result$contribution, manual_result$contribution)
  expect_equal(wrapper_result$headcount_ratio, manual_result$headcount_ratio)
})

test_that("compute_mpi_with returns an mpi_output object", {
  wrapper_result <- compute_mpi_with(
    .data = df_simple,
    .mpi_specs = specs_simple,
    .deprivations = list(
      a1 = list(.cutoff = quote(a1 == 1)),
      a2 = list(.cutoff = quote(a2 == 1)),
      b1 = list(.cutoff = quote(b1 == 1)),
      b2 = list(.cutoff = quote(b2 == 1))
    )
  )

  expect_s3_class(wrapper_result, "mpi_output")
  expect_named(wrapper_result, c("index", "contribution", "headcount_ratio", "deprivation_matrix"))
})

test_that("compute_mpi_with raises an error for an indicator not in specs", {
  expect_error(
    compute_mpi_with(
      .data = df_simple,
      .mpi_specs = specs_simple,
      .deprivations = list(
        a1 = list(.cutoff = quote(a1 == 1)),
        not_a_real_indicator = list(.cutoff = quote(a2 == 1))
      )
    ),
    regexp = "not a valid indicator"
  )
})

test_that("compute_mpi_with accepts a per-indicator .data override", {
  # a1 and a2 live in a separate data frame from b1 and b2
  df_ab_only <- df_simple[, c("a1", "a2")]
  df_b_only <- df_simple[, c("b1", "b2")]

  wrapper_result <- compute_mpi_with(
    .data = df_simple,
    .mpi_specs = specs_simple,
    .deprivations = list(
      a1 = list(.data = df_ab_only, .cutoff = quote(a1 == 1)),
      a2 = list(.data = df_ab_only, .cutoff = quote(a2 == 1)),
      b1 = list(.data = df_b_only, .cutoff = quote(b1 == 1)),
      b2 = list(.data = df_b_only, .cutoff = quote(b2 == 1))
    )
  )

  manual_result <- df_simple |>
    compute_mpi(dp_simple, .mpi_specs = specs_simple)

  expect_equal(wrapper_result$index, manual_result$index)
})

test_that("compute_mpi_with passes grouping arguments to compute_mpi", {
  df_grouped <- df_simple |>
    dplyr::mutate(region = c("X", "X", "Y", "Y"), .before = 1)

  dp_grouped <- list()
  dp_grouped$a1 <- df_grouped |>
    define_deprivation(a1, a1 == 1, .mpi_specs = specs_simple)
  dp_grouped$a2 <- df_grouped |>
    define_deprivation(a2, a2 == 1, .mpi_specs = specs_simple)
  dp_grouped$b1 <- df_grouped |>
    define_deprivation(b1, b1 == 1, .mpi_specs = specs_simple)
  dp_grouped$b2 <- df_grouped |>
    define_deprivation(b2, b2 == 1, .mpi_specs = specs_simple)

  manual_result <- df_grouped |>
    compute_mpi(dp_grouped, .mpi_specs = specs_simple, region)

  wrapper_result <- compute_mpi_with(
    .data = df_grouped,
    .mpi_specs = specs_simple,
    .deprivations = list(
      a1 = list(.cutoff = quote(a1 == 1)),
      a2 = list(.cutoff = quote(a2 == 1)),
      b1 = list(.cutoff = quote(b1 == 1)),
      b2 = list(.cutoff = quote(b2 == 1))
    ),
    region
  )

  expect_equal(wrapper_result$index, manual_result$index)
})
