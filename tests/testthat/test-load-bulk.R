context("bulk load multiple raw files")

test_that("SEA data loads properly", {
  expect_is(load_sea_bulk(aceR_sample_data_path("sea"), verbose = F), "data.frame")
})
