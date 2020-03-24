context("process summary stats on loaded data")

raw_explorer <- load_ace_bulk(aceR_sample_data_path("explorer"), app_type = "explorer", verbose = F)

test_that("ACE Ishihara works", {
  expect_named(module_ishihara(raw_explorer$data[[ISHIHARA]]),
               expected = c(COL_BID, "colorblind"))
})

test_that("ACE Explorer data bulk processes properly", {
  
  expect_gt(nrow(proc_by_module(raw_explorer, app_type = "explorer", verbose = F)), 0)
})