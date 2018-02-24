context("load raw SEA csv data")

sample_data = paste(aceR_sample_data_path(), "sample-sea.csv", sep = "/")

test_that("SEA data loads properly", {
  expect_false(any(endsWith(load_sea_file(sample_data)[[COL_MODULE]], ".")))
  expect_is(load_sea_bulk(aceR_sample_data_path(), pattern = "sea", verbose = F), "data.frame")
})

