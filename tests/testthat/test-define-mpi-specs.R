list_name <- c("indicators", "poverty_cutoffs", "uid", "unit_of_analysis", "aggregation", "source_of_data", "names_separator")
colname <- c("dimension", "indicator", "variable", "weight")

get_file <- function(file_type) {
  specs_file <- system.file(
    "extdata",
    paste0("global-mpi-specs.", file_type),
    package = "mpindex"
  )
}

test_that("cannot accept poverty cutoff greather than 1", {
  file <- get_file('csv')
  expect_error(
    define_mpi_specs(file, .poverty_cutoffs = 1.001),
    '.poverty_cutoffs cannot contain values greater than 1.'
  )
  expect_error(
    define_mpi_specs(file, .poverty_cutoffs = c(1/3, 1.5)),
    '.poverty_cutoffs cannot contain values greater than 1.'
  )
})

test_that("cannot accept poverty cutoff less than 1 divided by the total number of indicators", {
  file <- get_file('csv')
  expect_error(
    define_mpi_specs(file, .poverty_cutoffs = 0.001),
    '.poverty_cutoffs cannot contain values less than 1 divided by the total number of indicators.'
  )
  expect_error(
    define_mpi_specs(file, .poverty_cutoffs = c(-1, 1/3)),
    '.poverty_cutoffs cannot contain values less than 1 divided by the total number of indicators.'
  )
})

test_that("`.unit_of_analysis` argument only accepts a string of characters with length of 1", {
  file <- get_file('csv')
  expect_error(
    define_mpi_specs(file, .unit_of_analysis = 1),
    '.unit_of_analysis argument only accepts string of characters.'
  )
  expect_error(
    define_mpi_specs(file, .unit_of_analysis = c('households', 'families')),
    '.unit_of_analysis argument cannot accept multiple values.'
  )
})

test_that("`.uid` argument only accepts a string of characters with length of 1", {
  file <- get_file('csv')
  expect_error(
    define_mpi_specs(file, .uid = c('uuid', 'case_id')),
    '.uid argument cannot accept multiple values.'
  )
})

test_that("`.names_separator` only accepts limited characters", {
  file <- get_file('csv')
  expect_error(
    define_mpi_specs(file, .names_separator = c('>', '<', '_')),
    '.names_separator argument cannot accept multiple values.'
  )
})


test_that("variable_name column concatenates correctly", {
  file <- get_file('csv')
  mpi_specs <- define_mpi_specs(file)
  concat <- grepl('^d\\d{2}_i\\d{2}.*', mpi_specs$indicators$variable_name)
  expect_length(concat[concat == T], 10)
})


test_that("[csv] sample specs file loads correctly", {
  file <- get_file('csv')
  specs <- read.csv(file) |> clean_colnames()
  specs_name <- to_lowercase(sort(names(specs)))
  mpi_specs <- define_mpi_specs(file)

  expect_contains(specs_name, colname)
  expect_equal(ncol(specs), 5)
  expect_equal(nrow(specs), 10)
  expect_equal(sum(specs$weight), 1, tolerance = 0.001)
  expect_length(unique(specs$dimension), 3)
  expect_length(unique(specs$indicator), 10)
  expect_length(unique(specs$variable), 10)
  expect_identical(names(mpi_specs), list_name)
  expect_equal(ncol(mpi_specs$indicators), 7)
  expect_equal(nrow(mpi_specs$indicators), 10)
})

test_that("[xlsx] sample specs file loads correctly", {
  file <- get_file('xlsx')
  specs <- openxlsx::read.xlsx(file, sheet = 1) |> clean_colnames()
  specs_name <- to_lowercase(sort(names(specs)))
  mpi_specs <- define_mpi_specs(file)

  expect_contains(specs_name, colname)
  expect_equal(ncol(specs), 5)
  expect_equal(nrow(specs), 10)
  expect_equal(sum(specs$weight), 1, tolerance = 0.001)
  expect_length(unique(specs$dimension), 3)
  expect_length(unique(specs$indicator), 10)
  expect_length(unique(specs$variable), 10)
  expect_identical(names(mpi_specs), list_name)
  expect_equal(ncol(mpi_specs$indicators), 7)
  expect_equal(nrow(mpi_specs$indicators), 10)
})

test_that("[txt] sample specs file loads correctly", {
  file <- get_file('txt')
  specs <- read.delim(file) |> clean_colnames()
  specs_name <- to_lowercase(sort(names(specs)))
  mpi_specs <- define_mpi_specs(file)

  expect_contains(specs_name, colname)
  expect_equal(ncol(specs), 5)
  expect_equal(nrow(specs), 10)
  expect_equal(sum(specs$weight), 1, tolerance = 0.001)
  expect_length(unique(specs$dimension), 3)
  expect_length(unique(specs$indicator), 10)
  expect_length(unique(specs$variable), 10)
  expect_identical(names(mpi_specs), list_name)
  expect_equal(ncol(mpi_specs$indicators), 7)
  expect_equal(nrow(mpi_specs$indicators), 10)
})

test_that("[json] sample specs file loads correctly", {
  file <- get_file('json')
  specs <- jsonlite::read_json(file, simplifyVector = T) |> clean_colnames()
  specs_name <- to_lowercase(sort(names(specs)))
  mpi_specs <- define_mpi_specs(file)

  expect_contains(specs_name, colname)
  expect_equal(ncol(specs), 5)
  expect_equal(nrow(specs), 10)
  expect_equal(sum(specs$weight), 1, tolerance = 0.001)
  expect_length(unique(specs$dimension), 3)
  expect_length(unique(specs$indicator), 10)
  expect_length(unique(specs$variable), 10)
  expect_identical(names(mpi_specs), list_name)
  expect_equal(ncol(mpi_specs$indicators), 7)
  expect_equal(nrow(mpi_specs$indicators), 10)
})
