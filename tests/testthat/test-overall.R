# Tests for $overall summary in grouped mpi_output

df_grouped <- df_simple |>
  dplyr::mutate(region = c("X", "X", "Y", "Y"), .before = 1)

deps_simple <- list(
  a1 = deprived(a1 == 1),
  a2 = deprived(a2 == 1),
  b1 = deprived(b1 == 1),
  b2 = deprived(b2 == 1)
)

# ---------------------------------------------------------------------------
# Cycle 1: absent when by is not set
# ---------------------------------------------------------------------------

test_that("$overall is absent when by is not set", {
  r <- compute_mpi(df_simple, mpi_specs = specs_simple, deprivations = deps_simple)
  expect_false("overall" %in% names(r))
})

# ---------------------------------------------------------------------------
# Cycle 2: present when by is set
# ---------------------------------------------------------------------------

test_that("$overall is present when by is set", {
  r <- compute_mpi(df_grouped, mpi_specs = specs_simple, deprivations = deps_simple, by = region)
  expect_true("overall" %in% names(r))
})

# ---------------------------------------------------------------------------
# Cycle 3: $overall structure
# ---------------------------------------------------------------------------

test_that("$overall has exactly index, contribution, headcount_ratio", {
  r <- compute_mpi(df_grouped, mpi_specs = specs_simple, deprivations = deps_simple, by = region)
  expect_named(r$overall, c("index", "contribution", "headcount_ratio"), ignore.order = TRUE)
})

# ---------------------------------------------------------------------------
# Cycle 4: $overall$index is a 1-row frame
# ---------------------------------------------------------------------------

test_that("$overall$index$k_50 is a 1-row frame", {
  r <- compute_mpi(df_grouped, mpi_specs = specs_simple, deprivations = deps_simple, by = region)
  expect_equal(nrow(r$overall$index$k_50), 1L)
})

# ---------------------------------------------------------------------------
# Cycle 5: $overall values match compute_mpi() without by
# ---------------------------------------------------------------------------

test_that("$overall$index matches ungrouped compute_mpi()", {
  r_grouped   <- compute_mpi(df_grouped, mpi_specs = specs_simple, deprivations = deps_simple, by = region)
  r_ungrouped <- compute_mpi(df_grouped, mpi_specs = specs_simple, deprivations = deps_simple)

  expect_equal(
    r_grouped$overall$index$k_50$headcount_ratio,
    r_ungrouped$index$k_50$headcount_ratio,
    tolerance = 1e-10
  )
  expect_equal(
    r_grouped$overall$index$k_50$mpi,
    r_ungrouped$index$k_50$mpi,
    tolerance = 1e-10
  )
})

test_that("$overall$headcount_ratio matches ungrouped compute_mpi()", {
  r_grouped   <- compute_mpi(df_grouped, mpi_specs = specs_simple, deprivations = deps_simple, by = region)
  r_ungrouped <- compute_mpi(df_grouped, mpi_specs = specs_simple, deprivations = deps_simple)

  expect_equal(
    r_grouped$overall$headcount_ratio$uncensored,
    r_ungrouped$headcount_ratio$uncensored,
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})

# ---------------------------------------------------------------------------
# Cycle 6: survey weights + by -> $overall present
# ---------------------------------------------------------------------------

test_that("$overall present with survey weights and by", {
  skip_if_not_installed("survey")
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", by = class)
  expect_true("overall" %in% names(r))
  expect_equal(nrow(r$overall$index$k_33), 1L)
})

# ---------------------------------------------------------------------------
# Cycle 7: inference = TRUE -> SE columns in $overall
# ---------------------------------------------------------------------------

test_that("$overall$index has SE columns when inference = TRUE", {
  skip_if_not_installed("survey")
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", by = class, inference = TRUE)
  expect_true("headcount_ratio_se" %in% names(r$overall$index$k_33))
})

# ---------------------------------------------------------------------------
# Cycle 8: multi-cutoff -> all k keys in $overall$index
# ---------------------------------------------------------------------------

test_that("$overall$index has all cutoff keys for multi-cutoff specs", {
  specs_multi <- global_mpi_specs(uid = "uuid", poverty_cutoffs = c(1/3, 1/2))
  r <- compute_mpi(df_household, mpi_specs = specs_multi,
                   deprivations = deprivation_profile, by = class)
  expect_named(r$overall$index, c("k_33", "k_50"), ignore.order = FALSE)
})
