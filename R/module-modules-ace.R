
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols mutate if_else left_join
#' @importFrom rlang !!
#' @keywords internal
#' @name ace_procs

module_adp <- function(df) {
  df <- mutate(df, expression_nonneutral = if_else(cue_expression != "neutral", "nonneutral", cue_expression))
  gen = proc_generic_module(df)
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_CONDITION, FUN = ace_rcs)
  gen = left_join(gen, rcs, by = COL_BID)
  happy_cost = multi_subtract(gen, "\\.happy_happy", "\\.happy_neutral", "\\.happy_cost")
  sad_cost = multi_subtract(gen, "\\.sad_sad", "\\.sad_neutral", "\\.sad_cost")
  gen_nonneutral = proc_generic_module(df, col_condition = rlang::sym("expression_nonneutral"))
  nonneutral_cost = multi_subtract(gen_nonneutral, "\\.nonneutral", "\\.neutral", "\\.nonneutral_cost")
  return (bind_cols(gen, happy_cost, sad_cost, nonneutral_cost))
}

#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  conditions = df %>% 
    distinct(!!Q_COL_BID, !!Q_COL_CONDITION) %>% 
    count(!!Q_COL_BID) %>% 
    rename(n_conditions = n)
  
  gen = proc_generic_module(df)
  gen$score = (((gen$rt_mean.conjunction_12 - gen$rt_mean.conjunction_4) / gen$rt_mean.conjunction_4) * 100) + 100
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_CONDITION, FUN = ace_rcs)
  gen = left_join(gen, rcs, by = COL_BID)
  proc_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_median", ace_median) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_median", ace_median)
  proc_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_mean", ace_mean) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_mean", ace_mean)
  dist_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_median", ace_median) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_median", ace_median)
  dist_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_mean", ace_mean) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_mean", ace_mean)
  conj_cost = multi_subtract(gen, "\\.conjunction_12", "\\.conjunction_4", "\\.conj_cost")
  feat_cost = multi_subtract(gen, "\\.feature_12", "\\.feature_4", "\\.feat_cost")
  
  out = bind_cols(gen,
                  proc_cost_median,
                  proc_cost_mean,
                  dist_cost_median,
                  dist_cost_mean,
                  conj_cost,
                  feat_cost) %>% 
    left_join(conditions, by = COL_BID) %>% 
    mutate(across(contains("overall"), ~na_if_true(.x, n_conditions < 4))) %>% 
    select(-n_conditions)
  
  return (out)
}

#' @importFrom magrittr %>%
#' @importFrom rlang sym !! :=
#' @importFrom dplyr case_when mutate recode select starts_with
#' @keywords internal
#' @name ace_procs

module_brt <- function(df) {
  if (COL_HANDEDNESS %in% names(df)) {
    df <- df %>%
      mutate(!!COL_HANDEDNESS := tolower(!!Q_COL_HANDEDNESS))
    
    if (!all(df[[COL_HANDEDNESS]] %in% c("right", "left"))) {
      warning("Nonstandard handedness levels detected.\n",
              "Handedness levels found in data (coerced to lowercase): ",
              paste(unique(df[[COL_HANDEDNESS]]), collapse = " "),
              "\n",
              "Dominant hand recoding may be unknown for these levels")
    }
    
    df <- df %>%
      mutate(condition_hand = case_when(
        grepl("right", !!Q_COL_HANDEDNESS) ~ recode(!!Q_COL_CONDITION,
                                                    right = "dominant",
                                                    left = "nondominant",
                                                    rightthumb="dominant.thumb",
                                                    leftthumb="nondominant.thumb"),
        grepl("left", !!Q_COL_HANDEDNESS) ~ recode(!!Q_COL_CONDITION,
                                                   left = "dominant",
                                                   right = "nondominant",
                                                   leftthumb="dominant.thumb",
                                                   rightthumb="nondominant.thumb"),
        TRUE ~ !!Q_COL_CONDITION))
    gen = proc_generic_module(df, col_condition = sym("condition_hand"))
  } else {
    warning("No handedness data found. Unable to label BRT data by dominant hand")
    gen = proc_generic_module(df)
  }
  gen = select(gen, -starts_with(PROC_COL_OLD[1]), -starts_with(PROC_COL_OLD[2]))
  return (gen)
}


#' @keywords internal
#' @name ace_procs
#' @importFrom magrittr %>%
#' @importFrom dplyr full_join select
#' @importFrom rlang !! := sym
#' @importFrom tidyselect ends_with

module_colorselection <- function(df) {
  gen_strict = proc_generic_module(df, col_condition = NULL)
  gen_loose = df %>% 
    select(-(!!COL_CORRECT_BUTTON), -(!!COL_PREV_CORRECT_BUTTON)) %>% 
    rename(!!COL_CORRECT_BUTTON := !!paste0(COL_CORRECT_BUTTON, "_loose"),
           !!COL_PREV_CORRECT_BUTTON := !!paste0(COL_PREV_CORRECT_BUTTON, "_loose")) %>% 
    proc_generic_module(col_condition = NULL)
  
  if (COL_PRACTICE_COUNT %in% names(df)) {
    gen_join_by = c(COL_BID, COL_PRACTICE_COUNT)
  } else {
    gen_join_by = COL_BID
  }
  gen = full_join(gen_strict, gen_loose, by = gen_join_by, suffix = c(".strict", ".loose"))
  
  max_delay_strict = proc_by_condition(df,
                                       "test_delay_window",
                                       Q_COL_CORRECT_BUTTON,
                                       include_overall = F,
                                       FUN = ace_max_delay) %>% 
    select(-ends_with("incorrect"), -ends_with("no_response"))
  max_delay_loose = proc_by_condition(df,
                                      "test_delay_window",
                                      sym("correct_button_loose"),
                                      include_overall = F,
                                      FUN = ace_max_delay) %>% 
    select(-ends_with("incorrect"), -ends_with("no_response"))
  
  max_delay = full_join(max_delay_strict, max_delay_loose, by = COL_BID, suffix = c(".strict", ".loose"))
  
  analy = full_join(gen, max_delay, by = COL_BID)
  
  if (COL_PRACTICE_COUNT %in% names(df)) {
    prac = proc_by_condition(df, COL_PRACTICE_COUNT, include_overall = FALSE, FUN = ace_practice_count)
    return (full_join(analy, prac, by = c(COL_BID, COL_PRACTICE_COUNT)))
  } else {
    return (analy)
  }
}

#' @keywords internal
#' @name ace_procs

module_discrimination <- function(df) {
  # TODO: Standardize correct? column name
  gen = proc_generic_module(df, col_acc = Q_COL_CORRECT_RESPONSE, col_condition = rlang::sym("cue_type"))
  rcs = proc_by_condition(df, c(Q_COL_CORRECT_RESPONSE, COL_RT), Q_COL_TRIAL_TYPE, FUN = ace_rcs)
  return (left_join(gen, rcs, by = COL_BID))
}

#' @keywords internal
#' @name ace_procs

module_flanker <- function(df) {
  gen = proc_generic_module(df, col_condition = Q_COL_TRIAL_TYPE)
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_TRIAL_TYPE, FUN = ace_rcs)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (left_join(gen, rcs, by = COL_BID) %>% dplyr::bind_cols(cost))
}

#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := !!
#' @importFrom tidyselect any_of
#' @keywords internal
#' @name ace_procs

module_saat <- function(df, app_type) {
  stopifnot(app_type %in% c("classroom", "explorer")) # Failsafe
  
  # df = replace_empty_values(df, COL_CONDITION, "saattype")
  df = mutate(df,
              # !!COL_CONDITION := tolower(!!Q_COL_CONDITION),
              # non-response trials should have NA rt, not 0 rt
              # so it will be excluded from mean calculations
              !!COL_RT := na_if(!!Q_COL_RT, 0))
  
  gen = proc_generic_module(df, col_condition = NULL)
  sdt = proc_by_condition(df, "trial_accuracy", FUN = ace_dprime_dplyr)
  
  # Remove duplicate d' column created by condition-wise processing if it's a single SAAT submodule
  if (length(unique(df[[COL_CONDITION]])) == 1) {
    sdt <- sdt %>% 
      select(-!c(!!COL_BID, ends_with("overall")))
  }
  
  # Calc this attention span metric thing
  if (app_type == "explorer") {
    attention <- df %>% 
      group_by(!!Q_COL_BID) %>% 
      # The meat and bones of the attention span logic are here
      mutate(mistake = as.integer(trial_accuracy %in% c("Miss", "False Alarm") | 
                                    is.na(!!Q_COL_RT) | 
                                    (!!Q_COL_RT != -99 & (!!Q_COL_RT - ace_mean(!!Q_COL_RT)) / ace_sd(!!Q_COL_RT) > 1)),
             cum_mistake = cumsum(mistake)) %>%
      group_by(bid, cum_mistake) %>%
      summarize(trial_start = min(trial_number),
                trial_end = max(trial_number),
                rt_end = rt[trial_number == max(trial_number)],
                rw_end = rw[trial_number == max(trial_number)]) %>%
      mutate(rt_end = if_else(rt_end == -99, rw_end, rt_end),
             duration = 2200 * (trial_end - trial_start - 1) + rt_end) %>%
      group_by(!!Q_COL_BID) %>%
      summarize(attention_span_max.overall = max(duration),
                attention_span_mean.overall = mean(duration))
    
    out <- left_join(gen, sdt, by = COL_BID) %>% 
      left_join(attention, by = COL_BID)
  } else {
    out <- left_join(gen, sdt, by = COL_BID)
  }
  
  return (out)
}

#' @keywords internal
#' @name ace_procs

module_stroop <- function(df) {
  gen = proc_generic_module(df, col_condition = Q_COL_TRIAL_TYPE)
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_TRIAL_TYPE, FUN = ace_rcs)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (left_join(gen, rcs, by = COL_BID) %>% dplyr::bind_cols(cost))
}

#' @keywords internal
#' @name ace_procs

module_spatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, Q_COL_CORRECT_BUTTON)
  span = proc_by_condition(df, "object_count", FUN = ace_spatial_span)
  rt_block_half = proc_by_condition(df, COL_RT, factors = Q_COL_BLOCK_HALF, include_overall = F)
  analy = list(rt, span, rt_block_half)
  if (COL_PRACTICE_COUNT %in% names(df)) {
    prac = proc_by_condition(df, COL_PRACTICE_COUNT, include_overall = FALSE, FUN = ace_practice_count)
    analy = c(analy, list(prac))
  }
  merged = multi_merge(analy, by = COL_BID)
  # Assume that all subjects who return a span less than 3 are technical failures and scrub
  merged = dplyr::filter(merged, object_count_span.overall >= 3)
  return (merged)
}

#' @keywords internal
#' @name ace_procs

module_taskswitch <- function(df) {
  df$taskswitch_state = plyr::mapvalues(df$taskswitch_state, from = c(0, 1 , 2), to = c("start", "switch", "stay"), warn_missing = FALSE)
  gen = proc_generic_module(df, col_condition = rlang::sym("taskswitch_state"))
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), rlang::sym("taskswitch_state"), FUN = ace_rcs)
  cost = multi_subtract(gen, "\\.switch", "\\.stay", "\\.cost")
  return (left_join(gen, rcs, by = COL_BID) %>% dplyr::bind_cols(cost))
}

#' @import dplyr
#' @importFrom magrittr %>%
#' @keywords internal
#' @name ace_procs

module_tnt <- function(df) {
  df$condition = dplyr::recode(df$condition, `tap & trace` = "tap_trace", `tap only` = "tap_only")
  gen = proc_generic_module(df)
  
  conditions = df %>% 
    distinct(!!Q_COL_BID, !!Q_COL_CONDITION) %>% 
    count(!!Q_COL_BID) %>% 
    rename(n_conditions = n)
  
  cost = multi_subtract(gen, "\\.tap_trace", "\\.tap_only", "\\.cost")
  sdt = proc_by_condition(df, "trial_accuracy", Q_COL_CONDITION, FUN = ace_dprime_dplyr)
  out <- bind_cols(gen, cost) %>% 
    left_join(sdt, by = COL_BID) %>% 
    left_join(conditions, by = COL_BID) %>% 
    mutate(across(contains("overall"), ~na_if_true(.x, n_conditions < 4))) %>% 
    select(-n_conditions)
  return (out)
}

#' @keywords internal
#' @name ace_procs

module_backwardsspatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, Q_COL_CORRECT_BUTTON)
  span = proc_by_condition(df, "object_count", FUN = ace_spatial_span)
  rt_block_half = proc_by_condition(df, COL_RT, factors = Q_COL_BLOCK_HALF, include_overall = F)
  analy = list(rt, span, rt_block_half)
  if (COL_PRACTICE_COUNT %in% names(df)) {
    prac = proc_by_condition(df, COL_PRACTICE_COUNT, include_overall = FALSE, FUN = ace_practice_count)
    analy = c(analy, list(prac))
  }
  merged = multi_merge(analy, by = COL_BID)
  merged = dplyr::filter(merged, object_count_span.overall >= 3)
  return (merged)
}

#' @import dplyr
#' @importFrom rlang sym !!
#' @importFrom stringr str_sub
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @name ace_procs

module_filter <- function(df) {
  
  df <- df %>%
    mutate(cue_rotated = dplyr::recode(cue_rotated,
                                       `0` = "no_change",
                                       `1` = "change"))
  
  rt = proc_by_condition(df, COL_RT, factors = c(Q_COL_CONDITION, Q_COL_CORRECT_BUTTON))
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_CONDITION, FUN = ace_rcs)
  dprime = proc_by_condition(df, "trial_accuracy", Q_COL_CONDITION, FUN = ace_dprime_dplyr)
  k = proc_by_condition(df,
                        "trial_accuracy",
                        Q_COL_CONDITION,
                        include_overall = FALSE,
                        FUN = ace_wm_prek_dplyr,
                        transform_dir = "long") %>%
    mutate(targets = as.integer(str_sub(!!Q_COL_CONDITION, start = 2L, end = 2L)),
           k = k * targets) %>% 
    select(-targets) %>% 
    pivot_wider(names_from = !!Q_COL_CONDITION, values_from = k, names_prefix = "k.")
    
  merged = reduce(list(rt, rcs, dprime, k), left_join, by = COL_BID)
  if (COL_PRACTICE_COUNT %in% names(df)) {
    prac = proc_by_condition(df, COL_PRACTICE_COUNT, include_overall = FALSE, FUN = ace_practice_count)
    merged = left_join(merged, prac, by = COL_BID)
  }
  
  return (select(merged, -contains(".."), -starts_with(PROC_COL_OLD[1]), -starts_with(PROC_COL_OLD[2])))
}

#' @importFrom rlang !!
#' @keywords internal
#' @name ace_procs

module_ishihara <- function(df) {
  if (!("rg_color_deficiency" %in% names(df))) {
    df$rg_color_deficiency = (df$trial_correct - 1L) * -1L
  }
  df = dplyr::group_by(df, !!Q_COL_BID)
  return (ungroup(ace_ishihara_dplyr(df, "rg_color_deficiency")))
}

#' @keywords internal
#' @name ace_procs

module_spatialcueing <- function(df) {
  gen = proc_generic_module(df, col_condition = Q_COL_TRIAL_TYPE)
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_TRIAL_TYPE, FUN = ace_rcs)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.inc_con_cost")
  # This should only trigger for newer versions of ACE Explorer where a "neutral" condition was added
  if (any(grepl("neutral", names(gen)))) {
    cost_neutral_incongruent = multi_subtract(gen, "\\.neutral", "\\.incongruent", "\\.neu_inc_cost")
    cost_neutral_congruent = multi_subtract(gen, "\\.neutral", "\\.congruent", "\\.neu_con_cost")
    
    cost_full = dplyr::bind_cols(cost, cost_neutral_incongruent, cost_neutral_congruent)
  } else {
    cost_full = cost
  }
  return (left_join(gen, rcs, by = COL_BID) %>% dplyr::bind_cols(cost_full))
}
