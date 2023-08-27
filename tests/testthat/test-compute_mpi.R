# df <- openxlsx::read.xlsx('./tests/data/mpi-define-sample.xlsx')
# jsonlite::write_json(df, './tests/data/mpi-define-sample.json', pretty = T)


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
