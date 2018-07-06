context("load raw SEA csv data")

test_that("SEA data loads properly", {
  sample_data = paste(aceR_sample_data_path(), "sample-sea.csv", sep = "/")
  
  expect_false(any(endsWith(load_sea_file(sample_data)[[COL_MODULE]], ".")))
  expect_is(load_sea_bulk(aceR_sample_data_path(), pattern = "sea", verbose = F), "data.frame")
})

