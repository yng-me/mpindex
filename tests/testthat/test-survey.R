skip_if_not_installed("survey")

# ---------------------------------------------------------------------------
# resolve_survey_design
# ---------------------------------------------------------------------------

test_that("resolve_survey_design returns NULL when no survey args given", {
  expect_null(resolve_survey_design(df_household_svy))
})

test_that("resolve_survey_design builds a svydesign from column names", {
  d <- resolve_survey_design(df_household_svy,
                              weight = "hh_weight",
                              strata = "strata",
                              cluster = "psu")
  expect_s3_class(d, "survey.design")
  expect_equal(nrow(d$variables), nrow(df_household_svy))
})

test_that("resolve_survey_design accepts a pre-built svydesign", {
  d <- resolve_survey_design(df_household_svy,
                              survey_design = svy_design_prebuilt)
  expect_s3_class(d, "survey.design")
})

test_that("resolve_survey_design errors when .survey_design is not a svydesign", {
  expect_error(
    resolve_survey_design(df_household_svy, survey_design = list()),
    "survey::svydesign"
  )
})

test_that("resolve_survey_design errors when weight column is missing", {
  expect_error(
    resolve_survey_design(df_household_svy, weight = "nonexistent"),
    "not found in .data"
  )
})

# ---------------------------------------------------------------------------
# design_with_dm
# ---------------------------------------------------------------------------

test_that("design_with_dm injects new variables into a design", {
  d   <- resolve_survey_design(df_household_svy, weight = "hh_weight")
  dm  <- data.frame(
    uuid      = df_household_svy$uuid,
    is_deprived = as.integer(runif(nrow(df_household_svy)) > 0.5)
  )
  d2  <- design_with_dm(d, dm, "uuid")
  expect_true("is_deprived" %in% names(d2$variables))
})

test_that("design_with_dm does not overwrite variables already in the design", {
  d   <- resolve_survey_design(df_household_svy, weight = "hh_weight")
  # uuid is already in the design variables; injecting a different value should be ignored
  dm  <- data.frame(uuid = df_household_svy$uuid, hh_weight = 99)
  d2  <- design_with_dm(d, dm, "uuid")
  expect_false(any(d2$variables$hh_weight == 99))
})

# ---------------------------------------------------------------------------
# check_survey_pkg
# ---------------------------------------------------------------------------

test_that("check_survey_pkg passes when survey is installed", {
  expect_no_error(check_survey_pkg())
})

# ---------------------------------------------------------------------------
# compute_mpi — weighted point estimates (column-name path)
# ---------------------------------------------------------------------------

test_that("weighted MPI differs from unweighted MPI", {
  r_weighted   <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                               weight = "hh_weight")
  r_unweighted <- compute_mpi(df_household,     mpi_specs, svy_deps)

  h_w  <- r_weighted$index$k_33$headcount_ratio
  h_uw <- r_unweighted$index$k_33$headcount_ratio
  expect_false(isTRUE(all.equal(h_w, h_uw, tolerance = 1e-6)))
})

test_that("weighted MPI output has the same structure as unweighted", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps, weight = "hh_weight")

  expect_named(r, c("index", "contribution", "headcount_ratio", "deprivation_matrix"))
  expect_named(r$index, "k_33")
  expect_named(r$headcount_ratio, c("uncensored", "k_33"))
  expect_equal(ncol(r$index$k_33), 4)        # n, H, A, MPI — no extra cols
  expect_equal(ncol(r$contribution$k_33), 11)
  expect_equal(ncol(r$headcount_ratio$uncensored), 11)
})

test_that("weighted H is a proportion between 0 and 1", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps, weight = "hh_weight")
  h <- r$index$k_33$headcount_ratio
  expect_true(h >= 0 && h <= 1)
})

test_that("weighted MPI equals H * A", {
  r   <- compute_mpi(df_household_svy, mpi_specs, svy_deps, weight = "hh_weight")
  idx <- r$index$k_33
  expect_equal(idx$mpi, idx$headcount_ratio * idx$intensity,
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("weighted contributions sum to 100", {
  r   <- compute_mpi(df_household_svy, mpi_specs, svy_deps, weight = "hh_weight")
  s   <- rowSums(dplyr::select(r$contribution$k_33, -1))   # exclude n column
  expect_equal(s, 100, tolerance = 0.001)
})

test_that("weight-only path (no strata/cluster) produces valid output", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps, weight = "hh_weight")
  expect_s3_class(r, "mpi_output")
  expect_true(is.numeric(r$index$k_33$mpi))
})

test_that("full design path (weight + strata + cluster) produces valid output", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", strata = "strata", cluster = "psu")
  expect_s3_class(r, "mpi_output")
  expect_true(is.numeric(r$index$k_33$mpi))
})

# ---------------------------------------------------------------------------
# compute_mpi — pre-built svydesign path
# ---------------------------------------------------------------------------

test_that("pre-built svydesign path produces same estimates as column-name path", {
  r_col <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                        weight = "hh_weight",
                        strata = "strata",
                        cluster = "psu")
  r_pre <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                        survey_design = svy_design_prebuilt)

  expect_equal(r_col$index$k_33$headcount_ratio,
               r_pre$index$k_33$headcount_ratio,
               tolerance = 1e-10)
  expect_equal(r_col$index$k_33$mpi,
               r_pre$index$k_33$mpi,
               tolerance = 1e-10)
})

test_that("pre-built svydesign output has correct structure", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   survey_design = svy_design_prebuilt)
  expect_named(r, c("index", "contribution", "headcount_ratio", "deprivation_matrix"))
  expect_equal(ncol(r$index$k_33), 4)
})

# ---------------------------------------------------------------------------
# compute_mpi — inference (SE and CI columns)
# ---------------------------------------------------------------------------

test_that("inference = TRUE appends SE and CI columns to index", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", strata = "strata", cluster = "psu",
                   inference = TRUE)
  idx <- r$index$k_33
  expect_true(all(c("headcount_ratio_se", "headcount_ratio_ci_low",
                     "headcount_ratio_ci_high", "intensity_se",
                     "mpi_se", "mpi_ci_low", "mpi_ci_high") %in% names(idx)))
})

test_that("inference = TRUE appends SE and CI columns to headcount_ratio", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", inference = TRUE)
  hr <- r$headcount_ratio$k_33
  se_cols <- grep("_se$", names(hr), value = TRUE)
  expect_gt(length(se_cols), 0)
})

test_that("SE values are positive", {
  r  <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                    weight = "hh_weight", strata = "strata", cluster = "psu",
                    inference = TRUE)
  se <- r$index$k_33$headcount_ratio_se
  expect_true(se > 0)
})

test_that("CI is wider than 0 and contains the point estimate", {
  r   <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                     weight = "hh_weight", inference = TRUE)
  idx <- r$index$k_33
  expect_lt(idx$headcount_ratio_ci_low,  idx$headcount_ratio)
  expect_gt(idx$headcount_ratio_ci_high, idx$headcount_ratio)
})

test_that("CI bounds are clamped to [0, 1]", {
  r   <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                     weight = "hh_weight", inference = TRUE)
  idx <- r$index$k_33
  expect_gte(idx$headcount_ratio_ci_low,  0)
  expect_lte(idx$headcount_ratio_ci_high, 1)
  expect_gte(idx$mpi_ci_low,  0)
  expect_lte(idx$mpi_ci_high, 1)
})

test_that("ci_level = 0.90 produces narrower CI than default 0.95", {
  r95 <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                     weight = "hh_weight", inference = TRUE, ci_level = 0.95)
  r90 <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                     weight = "hh_weight", inference = TRUE, ci_level = 0.90)

  width95 <- r95$index$k_33$headcount_ratio_ci_high - r95$index$k_33$headcount_ratio_ci_low
  width90 <- r90$index$k_33$headcount_ratio_ci_high - r90$index$k_33$headcount_ratio_ci_low
  expect_lt(width90, width95)
})

test_that("inference columns absent when inference = FALSE", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", inference = FALSE)
  expect_false(any(grepl("_se$|_ci_", names(r$index$k_33))))
})

test_that("pre-built design + inference gives same SEs as column-name + inference", {
  r_col <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                        weight = "hh_weight", strata = "strata", cluster = "psu",
                        inference = TRUE)
  r_pre <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                        survey_design = svy_design_prebuilt,
                        inference = TRUE)
  expect_equal(r_col$index$k_33$headcount_ratio_se,
               r_pre$index$k_33$headcount_ratio_se,
               tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# compute_mpi — disaggregation with .by
# ---------------------------------------------------------------------------

test_that("weighted .by disaggregation returns one row per group", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", by = class)
  expect_equal(nrow(r$index$k_33), 2)
  expect_true("class" %in% names(r$index$k_33))
})

test_that("weighted .by groups each have valid H in [0, 1]", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", by = class)
  h <- r$index$k_33$headcount_ratio
  expect_true(all(h >= 0 & h <= 1))
})

test_that("weighted .by + inference appends SE columns per group", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps,
                   weight = "hh_weight", by = class, inference = TRUE)
  expect_equal(nrow(r$index$k_33), 2)
  expect_true("headcount_ratio_se" %in% names(r$index$k_33))
})

# ---------------------------------------------------------------------------
# compute_mpi_from_profile — survey path
# ---------------------------------------------------------------------------

test_that("compute_mpi_from_profile accepts survey args", {
  r <- compute_mpi_from_profile(
    df_household_svy,
    deprivation_profile,
    mpi_specs = mpi_specs,
    weight = "hh_weight"
  )
  expect_s3_class(r, "mpi_output")
  expect_true(is.numeric(r$index$k_33$headcount_ratio))
})

test_that("compute_mpi_from_profile weighted + inference matches compute_mpi", {
  r_profile <- compute_mpi_from_profile(
    df_household_svy,
    deprivation_profile,
    mpi_specs = mpi_specs,
    weight = "hh_weight",
    strata = "strata",
    cluster = "psu",
    inference = TRUE
  )
  r_inline <- compute_mpi(
    df_household_svy, mpi_specs, svy_deps,
    weight = "hh_weight",
    strata = "strata",
    cluster = "psu",
    inference = TRUE
  )
  expect_equal(r_profile$index$k_33$headcount_ratio,
               r_inline$index$k_33$headcount_ratio,
               tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Backward-compatibility: unweighted path unchanged
# ---------------------------------------------------------------------------

test_that("unweighted path still works when no survey args given", {
  r <- compute_mpi(df_household, mpi_specs, svy_deps)
  expect_s3_class(r, "mpi_output")
  expect_equal(ncol(r$index$k_33), 4)
  expect_false(any(grepl("_se$", names(r$index$k_33))))
})

test_that("mpi_output class is set on weighted result", {
  r <- compute_mpi(df_household_svy, mpi_specs, svy_deps, weight = "hh_weight")
  expect_s3_class(r, "mpi_output")
})
