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

sd_cutoff <- 2

trimmed_range_ace_email <- raw_email %>%
  filter(!(module %in% c(SPATIAL_SPAN, BACK_SPATIAL_SPAN, ISHIHARA)) ) %>% 
  mutate(rt_within_pre = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_nogo_pre = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt)))) %>%
  trim_rt_trials_range(cutoff_min = range_cutoff[1], cutoff_max = range_cutoff[2]) %>%
  mutate(rt_within_post = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_without_post = map_int(data, ~sum((.x$rt < range_cutoff[1] | .x$rt > range_cutoff[2]) & !is.na(.x$rt))),
         rt_nogo_post = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt))))

trimmed_range_ace_explorer <- raw_explorer %>%
  filter(!(module %in% c(DEMOS, SPATIAL_SPAN, BACK_SPATIAL_SPAN, ISHIHARA))) %>% 
  mutate(rt_within_pre = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_nogo_pre = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt)))) %>%
  trim_rt_trials_range(cutoff_min = range_cutoff[1], cutoff_max = range_cutoff[2]) %>%
  mutate(rt_within_post = map_int(data, ~sum(.x$rt >= range_cutoff[1] & .x$rt <= range_cutoff[2] & !is.na(.x$rt))),
         rt_without_post = map_int(data, ~sum((.x$rt < range_cutoff[1] | .x$rt > range_cutoff[2]) & !is.na(.x$rt))),
         rt_nogo_post = map_int(data, ~sum(.x$rt == -99 & !is.na(.x$rt))))

trimmed_sd_ace_explorer <- raw_explorer %>%
  filter(!(module %in% c(DEMOS, SPATIAL_SPAN, BACK_SPATIAL_SPAN, ISHIHARA, SAAT))) %>% 
  mutate(data = map(data, ~.x %>% 
                      group_by(!!Q_COL_BID) %>% 
                      mutate(rt_scaled_test = na_if(!!Q_COL_RT, -99),
                             rt_scaled_test = c(scale(rt_scaled_test))) %>% 
                      ungroup())) %>%
  trim_rt_trials_sd(cutoff = 2) %>% 
  mutate(rt_within_aligned = map_int(data, ~.x %>%
                                   filter(abs(rt_scaled_test) <= sd_cutoff & is.na(rt)) %>% 
                              nrow()),
         rt_without_aligned = map_int(data, ~.x %>%
                           filter(abs(rt_scaled_test) > sd_cutoff & !is.na(rt)) %>% 
                             nrow()))

test_that("trimming: throws warning when range min < 150 ms", {
  expect_warning(trim_rt_trials_range(raw_explorer, cutoff_min = 100),
                 "Minimum allowable RT specified less than 150 ms")
})

test_that("trimming: range cutoff behaves", {
  
  expect_equal(trimmed_range_ace_email$rt_within_pre, trimmed_range_ace_email$rt_within_post)
  expect_equal(trimmed_range_ace_explorer$rt_within_pre, trimmed_range_ace_explorer$rt_within_post)
  
  expect_equal(trimmed_range_ace_email$rt_without_post, trimmed_range_ace_email$rt_nogo_post)
  expect_equal(trimmed_range_ace_explorer$rt_without_post, trimmed_range_ace_explorer$rt_nogo_post)
  
})

test_that("trimming: sd cutoff behaves", {
  # Check that no trials where pre-calculated SD was outside of range remain un-scrubbed
  expect_setequal(trimmed_sd_ace_explorer$rt_within_aligned, 0)
  expect_setequal(trimmed_sd_ace_explorer$rt_without_aligned, 0)
})

test_that("trimming: nogo trials are untouched", {
  expect_equal(trimmed_range_ace_email$rt_nogo_pre, trimmed_range_ace_email$rt_nogo_post)
  expect_equal(trimmed_range_ace_explorer$rt_nogo_pre, trimmed_range_ace_explorer$rt_nogo_post)
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

proc_explorer_long = proc_by_module(raw_explorer, app_type = "explorer", output = "long", verbose = F)
proc_explorer_wide = proc_by_module(raw_explorer, app_type = "explorer", output = "wide", verbose = F)

test_that("module proc: ACE Explorer data bulk processes properly", {
  # Data exists
  expect_gt(nrow(proc_explorer_long), 0)
  expect_gt(nrow(proc_explorer_wide), 0)
  # Make sure name binding did not duplicate names accidentally
  expect_false(any(endsWith(unlist(map(proc_explorer_long$proc, names), use.names = F), ".x") | endsWith(unlist(map(proc_explorer_long$proc, names), use.names = F), ".y")))
  expect_false(any(endsWith(names(proc_explorer_wide), ".x") | endsWith(names(proc_explorer_wide), ".y")))
})

test_that("module proc: ACE Classroom email data bulk processes properly", {
  long = proc_by_module(raw_email, app_type = "classroom", output = "long", verbose = F)
  wide = proc_by_module(raw_email, app_type = "classroom", output = "wide", verbose = F)
  expect_gt(nrow(long), 0)
  expect_gt(nrow(wide), 0)
  expect_false(any(endsWith(unlist(map(long$proc, names), use.names = F), ".x") | endsWith(unlist(map(long$proc, names), use.names = F), ".y")))
  expect_false(any(endsWith(names(wide), ".x") | endsWith(names(wide), ".y")))
})

test_that("module post-processing: cleaning below-chance trials works", {
  # In the testing data currently loaded with the package,
  # There are 3 participants with Flanker acc_mean.overall below 0.6
  # So if we forcibly set the cutoff_2choice to 0.6 only those guys should get trimmed
  # Does not test all conditions but better than nothing
  this_cutoff_2choice <- 0.6
  
  long = post_clean_chance(proc_explorer_long, app_type = "explorer", cutoff_2choice = this_cutoff_2choice)
  wide = post_clean_chance(proc_explorer_wide, app_type = "explorer", cutoff_2choice = this_cutoff_2choice)
  
  test_flanker_long <- proc_explorer_long$proc$FLANKER %>%
    filter(!is.na(!!Q_COL_BID)) %>%
    select(!!Q_COL_BID, acc_mean.overall) %>% 
    full_join(long$proc$FLANKER %>%
                filter(!is.na(!!Q_COL_BID)) %>%
                select(!!Q_COL_BID, acc_mean.overall),
              by = COL_BID) %>% 
    mutate(below_cutoff = acc_mean.overall.x <= this_cutoff_2choice)
  
  test_flanker_wide <- proc_explorer_wide %>%
    select(!!Q_COL_BID, FLANKER.acc_mean.overall) %>%
    filter(!is.na(FLANKER.acc_mean.overall)) %>% 
    full_join(wide %>%
                select(!!Q_COL_BID, FLANKER.acc_mean.overall) %>%
                filter(!is.na(FLANKER.acc_mean.overall)),
              by = COL_BID) %>% 
    mutate(below_cutoff = FLANKER.acc_mean.overall.x <= this_cutoff_2choice)
  
  expect_identical(is.na(test_flanker_long$acc_mean.overall.y), test_flanker_long$below_cutoff)
  expect_identical(is.na(test_flanker_wide$FLANKER.acc_mean.overall.y), test_flanker_wide$below_cutoff)
})

test_that("module post-processing: cleaning below-chance trials handles extra demos", {
  long = post_clean_chance(proc_explorer_long %>% 
                             mutate(proc = map(proc, ~mutate(.x, extrademo = rnorm(nrow(.))))),
                           app_type = "explorer",
                           extra_demos = "extrademo")
  wide = post_clean_chance(proc_explorer_wide %>% 
                             mutate(extrademo = rnorm(n())),
                           app_type = "explorer",
                           extra_demos = "extrademo")
  
  expect_true(all(map_lgl(long$proc, ~"extrademo" %in% names(.x))))
  expect_true("extrademo" %in% names(wide))
})
