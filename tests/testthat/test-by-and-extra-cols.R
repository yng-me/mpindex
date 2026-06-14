df_grouped <- df_simple |>
  dplyr::mutate(region = c("X", "X", "Y", "Y"), ea = c(1L, 1L, 2L, 2L), .before = 1)

deps_simple <- list(
  a1 = deprived(a1 == 1),
  a2 = deprived(a2 == 1),
  b1 = deprived(b1 == 1),
  b2 = deprived(b2 == 1)
)

# --- Tracer bullet -----------------------------------------------------------

test_that("... extra cols appear in deprivation matrix but do not group summaries", {
  result <- compute_mpi(
    df_grouped,
    mpi_specs = specs_simple,
    deprivations = deps_simple,
    region,                          # extra col via ...
    include_deprivation_matrix = TRUE
  )

  # column is in the matrix
  expect_true("region" %in% names(result$deprivation_matrix$uncensored))

  # summaries are NOT grouped by region — still a single row
  expect_equal(nrow(result$index$k_50), 1L)
})

test_that("matrix column order is uid -> by cols -> extra ... cols -> indicators", {
  result <- compute_mpi(
    df_grouped,
    mpi_specs = specs_simple,
    deprivations = deps_simple,
    by = region,
    ea,                              # extra col via ...
    include_deprivation_matrix = TRUE
  )

  cols <- names(result$deprivation_matrix$uncensored)
  uid_pos    <- which(cols == "uid")
  region_pos <- which(cols == "region")
  ea_pos     <- which(cols == "ea")

  # uid first, then by col, then extra col
  expect_true(uid_pos < region_pos)
  expect_true(region_pos < ea_pos)
  # indicator variables come after ea
  non_ind_cols <- c("uid", "region", "ea", "deprivation_score")
  expect_true(all(which(cols %in% non_ind_cols) < min(which(!cols %in% non_ind_cols))))
})
