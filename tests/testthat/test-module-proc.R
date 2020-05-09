context("operations on loaded data")

raw_explorer <- load_ace_bulk(aceR_sample_data_path("explorer"), app_type = "explorer", verbose = F)

raw_email <- load_ace_bulk(aceR_sample_data_path("email"),
                           exclude = "bad-data",
                           pattern = ".csv",
                           app_type = "email",
                           verbose = F)

range_cutoff <- c(150, 2000)

trimmed_ace_email <- raw_email %>%
  mutate(rt_within_pre = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_nogo_pre = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt)))) %>%
  trim_rt_trials(range_cutoff = range_cutoff) %>%
  mutate(rt_within_post = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_without_post = map_int(data, ~sum((.x$rt < range_cutoff[1] | .x$rt > range_cutoff[2]) & !is.na(.x$rt))),
         rt_nogo_post = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt))))

trimmed_ace_explore <- raw_explorer %>%
  mutate(rt_within_pre = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_nogo_pre = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt)))) %>%
  trim_rt_trials(range_cutoff = range_cutoff) %>%
  mutate(rt_within_post = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_without_post = map_int(data, ~sum((.x$rt < range_cutoff[1] | .x$rt > range_cutoff[2]) & !is.na(.x$rt))),
         rt_nogo_post = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt))))

test_that("trimming: range cutoff behaves", {
  
  expect_equal(trimmed_ace_email$rt_within_pre, trimmed_ace_email$rt_within_post)
  expect_equal(trimmed_ace_explore$rt_within_pre, trimmed_ace_explore$rt_within_post)
  
  expect_equal(trimmed_ace_email$rt_without_post, trimmed_ace_email$rt_nogo_post)
  expect_equal(trimmed_ace_explore$rt_without_post, trimmed_ace_explore$rt_nogo_post)
  
})

test_that("trimming: sd cutoff behaves", {
  
})

test_that("trimming: nogo trials are untouched", {
  expect_equal(trimmed_ace_email$rt_nogo_pre, trimmed_ace_email$rt_nogo_post)
  expect_equal(trimmed_ace_explore$rt_nogo_pre, trimmed_ace_explore$rt_nogo_post)
})

test_that("nesting: unnesting is long", {
  expect_false("list" %in% (raw_email %>%
                              unnest_ace_raw(app_type = "classroom") %>%
                              purrr::map_chr(rlang::type_of)))
  
  expect_false("list" %in% (raw_explorer %>%
                              unnest_ace_raw(app_type = "explorer") %>%
                              purrr::map_chr(rlang::type_of)))
})

test_that("nesting: re-nesting yields identical", {
  renest_explore <- raw_explorer %>%
    unnest_ace_raw(app_type = "explorer") %>%
    nest_ace_raw(app_type = "explorer")
  
  renest_email <- raw_email %>%
    unnest_ace_raw(app_type = "classroom") %>%
    nest_ace_raw(app_type = "classroom")
  
  for (i in 1:nrow(raw_explorer)) {
    expect_mapequal(raw_explorer$data[[raw_explorer$module[i]]], renest_explore$data[[raw_explorer$module[i]]])
  }
  
  for (i in 1:nrow(raw_email)) {
    expect_mapequal(raw_email$data[[raw_email$module[i]]], renest_email$data[[raw_email$module[i]]])
    expect_mapequal(raw_email$demos[[raw_email$module[i]]], renest_email$demos[[raw_email$module[i]]])
  }
})

test_that("module proc: ACE Ishihara works", {
  expect_named(attempt_module(raw_explorer$data[[ISHIHARA]], ISHIHARA, verbose = FALSE),
               expected = c(COL_BID, "colorblind"))
})

test_that("module proc: ACE BRT works", {
  expect_gt(ncol(attempt_module(raw_explorer$data[[BRT]], BRT, verbose = FALSE)), 1)
  expect_warning(attempt_module(raw_explorer$data[[BRT]], BRT, verbose = FALSE),
                 regexp = "No handedness data found")
})

test_that("module proc: ACE Classroom email data bulk processes properly", {
  expect_gt(nrow(proc_by_module(raw_explorer, app_type = "explorer", output = "long", verbose = F)), 0)
})

test_that("module proc: ACE Explorer data bulk processes properly", {
  expect_gt(nrow(proc_by_module(raw_email, app_type = "classroom", output = "long", verbose = F)), 0)
})
