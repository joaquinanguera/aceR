context("process summary stats on loaded data")

raw_explorer <- load_ace_bulk(aceR_sample_data_path("explorer"), app_type = "explorer", verbose = F)

raw_email <- load_ace_bulk(aceR_sample_data_path("email"),
                           exclude = "bad-data",
                           pattern = ".csv",
                           app_type = "email",
                           verbose = F)

test_that("ACE Ishihara works", {
  expect_named(attempt_module(raw_explorer$data[[ISHIHARA]], ISHIHARA, verbose = FALSE),
               expected = c(COL_BID, "colorblind"))
})

test_that("ACE BRT works", {
  expect_gt(ncol(attempt_module(raw_explorer$data[[BRT]], BRT, verbose = FALSE)), 1)
  expect_warning(attempt_module(raw_explorer$data[[BRT]], BRT, verbose = FALSE),
                 regexp = "No handedness data found")
})

test_that("ACE Classroom email data bulk processes properly", {
  expect_gt(nrow(proc_by_module(raw_explorer, app_type = "explorer", output = "long", verbose = F)), 0)
})

test_that("ACE Explorer data bulk processes properly", {
  expect_gt(nrow(proc_by_module(raw_email, app_type = "classroom", output = "long", verbose = F)), 0)
})
