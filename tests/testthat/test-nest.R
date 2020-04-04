context("unnest and re-nest loaded ACE data")

raw_explore <- load_ace_bulk(aceR_sample_data_path("explorer"), app_type = "explorer", verbose = F)

raw_email <- load_ace_bulk(aceR_sample_data_path("email"),
              exclude = "bad-data",
              pattern = ".csv",
              app_type = "email",
              verbose = F)

test_that("unnesting is long", {
  expect_false("list" %in% (raw_email %>%
                              unnest_ace_raw(app_type = "classroom") %>%
                              purrr::map_chr(rlang::type_of)))
  
  expect_false("list" %in% (raw_explore %>%
                              unnest_ace_raw(app_type = "explorer") %>%
                              purrr::map_chr(rlang::type_of)))
})

test_that("re-nesting yields identical", {
  renest_explore <- raw_explore %>%
    unnest_ace_raw(app_type = "explorer") %>%
    nest_ace_raw(app_type = "explorer")
  
  renest_email <- raw_email %>%
    unnest_ace_raw(app_type = "classroom") %>%
    nest_ace_raw(app_type = "classroom")
  
  for (i in 1:nrow(raw_explore)) {
   expect_mapequal(raw_explore$data[[raw_explore$module[i]]], renest_explore$data[[raw_explore$module[i]]])
  }
  
  for (i in 1:nrow(raw_email)) {
    expect_mapequal(raw_email$data[[raw_email$module[i]]], renest_email$data[[raw_email$module[i]]])
    expect_mapequal(raw_email$demos[[raw_email$module[i]]], renest_email$demos[[raw_email$module[i]]])
  }
})
