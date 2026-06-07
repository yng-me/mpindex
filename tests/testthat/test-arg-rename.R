## ADR-0006: argument names no longer carry a `.` prefix (except `.data`)

specs_file <- system.file("extdata", "global-mpi-specs.csv", package = "mpindex")

# ---------------------------------------------------------------------------
# define_mpi_specs
# ---------------------------------------------------------------------------

test_that("define_mpi_specs accepts unprefixed argument names", {
  s <- define_mpi_specs(mpi_specs_file = specs_file, uid = "uuid")
  expect_s3_class(s, "mpi_specs_df")
  expect_equal(as.character(attributes(s)$uid), "uuid")
})

test_that("define_mpi_specs accepts unprefixed poverty_cutoffs", {
  s <- define_mpi_specs(mpi_specs_file = specs_file, poverty_cutoffs = c(1/3, 1/5))
  expect_equal(attributes(s)$poverty_cutoffs, c(1/3, 1/5))
})

test_that("define_mpi_specs old dotted name triggers helpful error", {
  expect_error(
    define_mpi_specs(mpi_specs_file = specs_file, .uid = "uuid"),
    "renamed"
  )
})

# ---------------------------------------------------------------------------
# global_mpi_specs
# ---------------------------------------------------------------------------

test_that("global_mpi_specs accepts unprefixed uid", {
  s <- global_mpi_specs(uid = "uuid")
  expect_s3_class(s, "mpi_specs_df")
  expect_equal(as.character(attributes(s)$uid), "uuid")
})

# ---------------------------------------------------------------------------
# deprived
# ---------------------------------------------------------------------------

test_that("deprived accepts unprefixed collapse_fn", {
  d <- deprived(age > 5, collapse_fn = max)
  expect_equal(d$collapse_fn, max)
})

test_that("deprived accepts unprefixed set_na_equal_to", {
  d <- deprived(age > 5, set_na_equal_to = 1)
  expect_equal(d$set_na_equal_to, 1)
})

test_that("deprived old dotted names trigger helpful error", {
  expect_error(deprived(age > 5, .collapse_fn = max), "renamed")
})

# ---------------------------------------------------------------------------
# define_deprivation
# ---------------------------------------------------------------------------

test_that("define_deprivation accepts unprefixed mpi_specs and cutoff", {
  s <- global_mpi_specs(uid = "uuid")
  dp <- define_deprivation(
    df_household,
    drinking_water,
    cutoff = drinking_water == 2,
    mpi_specs = s
  )
  expect_s3_class(dp, "mpi_dm")
})

test_that("define_deprivation old dotted names trigger helpful error", {
  s <- global_mpi_specs(uid = "uuid")
  expect_error(
    define_deprivation(df_household, drinking_water, cutoff = drinking_water == 2, mpi_specs = s, .collapse_fn = max),
    "renamed"
  )
})

# ---------------------------------------------------------------------------
# compute_mpi
# ---------------------------------------------------------------------------

test_that("compute_mpi accepts unprefixed mpi_specs and deprivations", {
  s <- global_mpi_specs(uid = "uuid")
  deps <- list(
    drinking_water    = deprived(drinking_water == 2),
    electricity       = deprived(electricity == 2),
    sanitation        = deprived(toilet > 1),
    cooking_fuel      = deprived(cooking_fuel %in% c(4:6, 9)),
    housing           = deprived(roof %in% c(5,7,9) | walls %in% c(5,8,9,99) == 2 | floor %in% c(5,6,9)),
    assets            = deprived(!((asset_tv + asset_telephone + asset_mobile_phone + asset_computer + asset_animal_cart + asset_bicycle + asset_motorcycle + asset_refrigerator) > 1 & (asset_car + asset_truck) > 0)),
    nutrition         = deprived(undernourished == 1 & age < 70, .data = df_household_roster, collapse_fn = max),
    child_mortality   = deprived(with_child_died == 1),
    year_schooling    = deprived(completed_6yrs_schooling == 2, .data = df_household_roster, collapse_fn = max),
    school_attendance = deprived(attending_school == 2 & age %in% 5:24, .data = df_household_roster, collapse_fn = max)
  )
  r <- compute_mpi(df_household, mpi_specs = s, deprivations = deps)
  expect_s3_class(r, "mpi_output")
})

test_that("compute_mpi old dotted name triggers helpful error", {
  s <- global_mpi_specs(uid = "uuid")
  expect_error(
    compute_mpi(df_household, .mpi_specs = s),
    "renamed"
  )
})

# ---------------------------------------------------------------------------
# compute_mpi_from_profile
# ---------------------------------------------------------------------------

test_that("compute_mpi_from_profile accepts unprefixed mpi_specs", {
  s <- global_mpi_specs(uid = "uuid")
  r <- compute_mpi_from_profile(df_household, deprivation_profile, mpi_specs = s)
  expect_s3_class(r, "mpi_output")
})

test_that("compute_mpi_from_profile old dotted name triggers helpful error", {
  s <- global_mpi_specs(uid = "uuid")
  expect_error(
    compute_mpi_from_profile(df_household, deprivation_profile, .mpi_specs = s),
    "renamed"
  )
})

# ---------------------------------------------------------------------------
# save_mpi
# ---------------------------------------------------------------------------

test_that("save_mpi accepts unprefixed mpi_specs and filename", {
  s   <- global_mpi_specs(uid = "uuid")
  r   <- compute_mpi_from_profile(df_household, deprivation_profile, mpi_specs = s)
  f   <- tempfile(fileext = ".xlsx")
  out <- save_mpi(r, mpi_specs = s, filename = f)
  expect_true(file.exists(out))
})

test_that("save_mpi old dotted name triggers helpful error", {
  s <- global_mpi_specs(uid = "uuid")
  r <- compute_mpi_from_profile(df_household, deprivation_profile, mpi_specs = s)
  expect_error(save_mpi(r, .mpi_specs = s), "renamed")
})
