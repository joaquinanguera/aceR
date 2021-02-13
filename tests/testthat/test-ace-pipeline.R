context("Whole ACE pipeline from loading to post")

raw_explorer <- load_ace_bulk(aceR_sample_data_path("explorer"), app_type = "explorer", verbose = F)

raw_email <- load_ace_bulk(aceR_sample_data_path("email"),
                           exclude = "bad-data",
                           pattern = ".csv",
                           app_type = "email",
                           verbose = F)

test_that("ACE data loads properly", {
  expect_s3_class(raw_explorer, "tbl_df")
  expect_s3_class(raw_email, "tbl_df")
})

range_cutoff <- c(150, 2000)

trimmed_ace_email <- raw_email %>%
  filter(!(module %in% c(SPATIAL_SPAN, BACK_SPATIAL_SPAN, ISHIHARA)) ) %>% 
  mutate(rt_within_pre = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_nogo_pre = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt)))) %>%
  trim_rt_trials(range_cutoff = range_cutoff) %>%
  mutate(rt_within_post = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_without_post = map_int(data, ~sum((.x$rt < range_cutoff[1] | .x$rt > range_cutoff[2]) & !is.na(.x$rt))),
         rt_nogo_post = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt))))

trimmed_ace_explorer <- raw_explorer %>%
  filter(!(module %in% c(DEMOS, SPATIAL_SPAN, BACK_SPATIAL_SPAN, ISHIHARA))) %>% 
  mutate(rt_within_pre = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_nogo_pre = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt)))) %>%
  trim_rt_trials(range_cutoff = range_cutoff) %>%
  mutate(rt_within_post = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_without_post = map_int(data, ~sum((.x$rt < range_cutoff[1] | .x$rt > range_cutoff[2]) & !is.na(.x$rt))),
         rt_nogo_post = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt))))

test_that("trimming: range cutoff behaves", {
  
  expect_equal(trimmed_ace_email$rt_within_pre, trimmed_ace_email$rt_within_post)
  expect_equal(trimmed_ace_explorer$rt_within_pre, trimmed_ace_explorer$rt_within_post)
  
  expect_equal(trimmed_ace_email$rt_without_post, trimmed_ace_email$rt_nogo_post)
  expect_equal(trimmed_ace_explorer$rt_without_post, trimmed_ace_explorer$rt_nogo_post)
  
})

test_that("trimming: sd cutoff behaves", {
  
})

test_that("trimming: nogo trials are untouched", {
  expect_equal(trimmed_ace_email$rt_nogo_pre, trimmed_ace_email$rt_nogo_post)
  expect_equal(trimmed_ace_explorer$rt_nogo_pre, trimmed_ace_explorer$rt_nogo_post)
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
  renest_explorer <- raw_explorer %>%
    unnest_ace_raw(app_type = "explorer") %>%
    nest_ace_raw(app_type = "explorer") %>% 
    arrange(module)
  
  renest_email <- raw_email %>%
    unnest_ace_raw(app_type = "classroom") %>%
    nest_ace_raw(app_type = "classroom") %>% 
    arrange(!!Q_COL_MODULE)
  
  for (i in 1:nrow(raw_explorer)) {
    if (raw_explorer$module[i] == DEMOS) {
      expect_mapequal(raw_explorer$data[[raw_explorer$module[i]]],
                      # force rows to be in same order
                      arrange(renest_explorer$data[[raw_explorer$module[i]]],
                              !!Q_COL_BID)
      )
    } else {
      expect_mapequal(raw_explorer$data[[raw_explorer$module[i]]],
                      renest_explorer$data[[raw_explorer$module[i]]]
      )
    }
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
  expect_gt(ncol(attempt_module(raw_explorer$data[[BRT]] %>% 
                                  reconstruct_pid() %>%
                                  left_join(raw_explorer$data[[DEMOS]] %>%
                                              select(COL_PID, COL_HANDEDNESS),
                                            by = COL_PID),
                                BRT, verbose = FALSE)),
            1)
  expect_warning(attempt_module(raw_explorer$data[[BRT]], BRT, verbose = FALSE),
                 regexp = "No handedness data found")
})

test_that("module proc: ACE backwards spatial span works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[BACK_SPATIAL_SPAN]], BACK_SPATIAL_SPAN, verbose = FALSE)), 1)
})

test_that("module proc: ACE forward spatial span works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[SPATIAL_SPAN]], SPATIAL_SPAN, verbose = FALSE)), 1)
})

test_that("module proc: ACE Boxed works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[BOXED]], BOXED, verbose = FALSE)), 1)
})

test_that("module proc: ACE Filter works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[FILTER]], FILTER, verbose = FALSE)), 1)
})

test_that("module proc: ACE Flanker works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[FLANKER]], FLANKER, verbose = FALSE)), 1)
})

test_that("module proc: ACE SAAT works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[SAAT]], SAAT, verbose = FALSE)), 1)
})

test_that("module proc: ACE Flanker works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[FLANKER]], FLANKER, verbose = FALSE)), 1)
})

test_that("module proc: ACE spatial cueing works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[SPATIAL_CUE]], SPATIAL_CUE, verbose = FALSE)), 1)
})

test_that("module proc: ACE Stroop works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[STROOP]], STROOP, verbose = FALSE)), 1)
})

test_that("module proc: ACE task switching works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[TASK_SWITCH]], TASK_SWITCH, verbose = FALSE)), 1)
})

test_that("module proc: ACE tap and trace works", {
  expect_gt(nrow(attempt_module(raw_explorer$data[[TNT]], TNT, verbose = FALSE)), 1)
})

test_that("module proc: ACE Explorer data bulk processes properly", {
  long = proc_by_module(raw_explorer, app_type = "explorer", output = "long", verbose = F)
  wide = proc_by_module(raw_explorer, app_type = "explorer", output = "wide", verbose = F)
  expect_gt(nrow(long), 0)
  expect_gt(nrow(wide), 0)
  expect_false(any(endsWith(unlist(map(long$proc, names), use.names = F), ".x") | endsWith(unlist(map(long$proc, names), use.names = F), ".y")))
  expect_false(any(endsWith(names(wide), ".x") | endsWith(names(wide), ".y")))
})

test_that("module proc: ACE Classroom email data bulk processes properly", {
  long = proc_by_module(raw_email, app_type = "classroom", output = "long", verbose = F)
  wide = proc_by_module(raw_email, app_type = "classroom", output = "wide", verbose = F)
  expect_gt(nrow(long), 0)
  expect_gt(nrow(wide), 0)
  expect_false(any(endsWith(unlist(map(long$proc, names), use.names = F), ".x") | endsWith(unlist(map(long$proc, names), use.names = F), ".y")))
  expect_false(any(endsWith(names(wide), ".x") | endsWith(names(wide), ".y")))
})

