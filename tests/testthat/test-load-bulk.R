context("bulk load multiple raw files")

test_that("ACE data loads properly", {
  expect_is(load_ace_bulk(aceR_sample_data_path("explorer"), app_type = "explorer", verbose = F), "data.frame")
  expect_is(load_ace_bulk(aceR_sample_data_path("email"),
                          exclude = "bad-data",
                          pattern = ".csv",
                          app_type = "email",
                          verbose = F),
            "data.frame")
})

test_that("SEA data loads properly", {
  expect_is(load_sea_bulk(aceR_sample_data_path("sea"), verbose = F), "data.frame")
})
