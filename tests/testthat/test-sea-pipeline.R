context("SEA pipeline from loading to proc")

test_that("SEA data loads properly", {
  expect_error(load_sea_bulk(aceR_sample_data_path("sea"), verbose = F), NA)
  expect_is(load_sea_bulk(aceR_sample_data_path("sea"), verbose = F), "data.frame")
})

raw_sea <- load_sea_bulk(aceR_sample_data_path("sea"), verbose = F)

test_that("module proc: SEA arithmetic verification works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[ARITHM_VER]], ARITHM_VER, verbose = FALSE)), 1)
})

test_that("module proc: SEA math recall works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[MATH_REC]], MATH_REC, verbose = FALSE)), 1)
})

test_that("module proc: SEA math fluency works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[MATH_FLU]], MATH_FLU, verbose = FALSE)), 1)
})

test_that("module proc: SEA reading fluency works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[READ_FLU]], READ_FLU, verbose = FALSE)), 1)
})

test_that("module proc: SEA reading comprehension works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[READ_COMP]], READ_COMP, verbose = FALSE)), 1)
})

test_that("module proc: SEA groupitizing works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[GROUPITIZE]], GROUPITIZE, verbose = FALSE)), 1)
})

test_that("module proc: SEA running memory span works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[RUN_MEM_SPAN]], RUN_MEM_SPAN, verbose = FALSE)), 1)
})

test_that("module proc: SEA relational matching works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[REL_MATCH]], REL_MATCH, verbose = FALSE)), 1)
})

test_that("module proc: SEA fractions level 1 works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[FRAC_1]], FRAC_1, verbose = FALSE)), 1)
})

test_that("module proc: SEA fractions level 2 works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[FRAC_2]], FRAC_2, verbose = FALSE)), 1)
})

test_that("module proc: SEA fractions level 3 works", {
  expect_gt(nrow(attempt_module(raw_sea$data[[FRAC_3]], FRAC_3, verbose = FALSE)), 1)
})

proc_sea_long <- proc_by_module(raw_sea, app_type = "sea", output = "long", verbose = F)
proc_sea_wide <- proc_by_module(raw_sea, app_type = "sea", output = "wide", verbose = F)

test_that("module proc: SEA data bulk processes properly", {

  # Data exists
  expect_gt(nrow(proc_sea_long), 0)
  expect_gt(nrow(proc_sea_wide), 0)
  # Make sure name binding did not duplicate names accidentally
  expect_false(any(endsWith(unlist(map(proc_sea_long$proc, names), use.names = F), ".x") | endsWith(unlist(map(proc_sea_long$proc, names), use.names = F), ".y")))
  expect_false(any(endsWith(names(proc_sea_wide), ".x") | endsWith(names(proc_sea_wide), ".y")))
})