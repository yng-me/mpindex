test_that("save_mpi returns a valid .xlsx file path", {
  mpi_result <- df_household |>
    compute_mpi_from_profile(deprivation_profile, mpi_specs = mpi_specs)

  out <- save_mpi(mpi_result, mpi_specs = mpi_specs, filename = tempfile(fileext = ".xlsx"))

  expect_type(out, "character")
  expect_true(grepl("\\.xlsx$", out))
  expect_true(file.exists(out))

  unlink(out)
})

test_that("save_mpi output contains expected sheet names", {
  mpi_result <- df_household |>
    compute_mpi_from_profile(deprivation_profile, mpi_specs = mpi_specs)

  out <- save_mpi(mpi_result, mpi_specs = mpi_specs, filename = tempfile(fileext = ".xlsx"))

  wb_sheets <- openxlsx::getSheetNames(out)
  expect_true("MPI (33%)" %in% wb_sheets)
  expect_true(any(grepl("Contribution by dimension", wb_sheets)))
  expect_true(any(grepl("Headcount ratio", wb_sheets)))
  expect_true("Uncensored deprivation matrix" %in% wb_sheets)
  expect_true("Dep. matrix (k = 33%)" %in% wb_sheets)

  unlink(out)
})

test_that("save_mpi works with include_deprivation_matrix = FALSE", {
  mpi_result <- df_household |>
    compute_mpi_from_profile(deprivation_profile, mpi_specs = mpi_specs)

  out <- save_mpi(
    mpi_result,
    mpi_specs = mpi_specs,
    filename = tempfile(fileext = ".xlsx"),
    include_deprivation_matrix = FALSE
  )

  wb_sheets <- openxlsx::getSheetNames(out)
  expect_true("MPI (33%)" %in% wb_sheets)
  expect_false(any(grepl("deprivation matrix", wb_sheets, ignore.case = TRUE)))

  unlink(out)
})

test_that("save_mpi appends .xlsx extension when missing", {
  mpi_result <- df_household |>
    compute_mpi_from_profile(deprivation_profile, mpi_specs = mpi_specs)

  tmp <- tempfile()
  out <- save_mpi(mpi_result, mpi_specs = mpi_specs, filename = tmp)

  expect_true(grepl("\\.xlsx$", out))
  expect_true(file.exists(out))

  unlink(out)
})

test_that("save_mpi includes specs sheet when include_specs = TRUE", {
  mpi_result <- df_household |>
    compute_mpi_from_profile(deprivation_profile, mpi_specs = mpi_specs)

  out <- save_mpi(
    mpi_result,
    mpi_specs = mpi_specs,
    filename = tempfile(fileext = ".xlsx"),
    include_specs = TRUE
  )

  wb_sheets <- openxlsx::getSheetNames(out)
  expect_true("MPI Specification" %in% wb_sheets)

  unlink(out)
})
