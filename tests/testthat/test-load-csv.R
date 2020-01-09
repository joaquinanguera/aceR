context("load raw csv data")

test_that("ACE Explorer CSV loads in", {
  sample_brt = paste(aceR_sample_data_path(), "sample-ace-brt-2.csv", sep = "/")
  sample_flanker = paste(aceR_sample_data_path(), "sample-ace-flanker-2.csv", sep = "/")
  
  expect_gt(nrow(load_ace_file(sample_brt)), 0)
  expect_gt(nrow(load_ace_file(sample_flanker)), 0)
})

test_that("SEA CSV loads in", {
  sample_sea = paste(aceR_sample_data_path(), "sample-sea-1.csv", sep = "/")
  
  expect_gt(nrow(load_sea_file(sample_sea)), 0)
  expect_false(any(endsWith(load_sea_file(sample_sea)[[COL_MODULE]], ".")))
})
