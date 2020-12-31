
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  gen = proc_generic_module(df)
  gen$score = (((gen$rt_mean.conjunction_12 - gen$rt_mean.conjunction_4) / gen$rt_mean.conjunction_4) * 100) + 100
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_CONDITION, FUN = ace_rcs)
  gen = dplyr::left_join(gen, rcs, by = COL_BID)
  proc_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_median", ace_median) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_median", ace_median)
  proc_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_mean", ace_mean) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_mean", ace_mean)
  dist_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_median", ace_median) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_median", ace_median)
  dist_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_mean", ace_mean) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_mean", ace_mean)
  conj_cost = multi_subtract(gen, "\\.conjunction_12", "\\.conjunction_4", "\\.conj_cost")
  feat_cost = multi_subtract(gen, "\\.feature_12", "\\.feature_4", "\\.feat_cost")
  return (dplyr::bind_cols(gen, proc_cost_median, proc_cost_mean, dist_cost_median, dist_cost_mean, conj_cost, feat_cost))
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

#' @importFrom dplyr left_join mutate na_if rename_with
#' @importFrom magrittr %>%
#' @importFrom rlang := !!
#' @importFrom tidyselect everything
#' @keywords internal
#' @name ace_procs

module_saat <- function(df) {
  df = replace_empty_values(df, COL_CONDITION, "saattype")
  df = mutate(df,
              !!COL_CONDITION := tolower(!!Q_COL_CONDITION),
              # non-response trials should have NA rt, not 0 rt
              # so it will be excluded from mean calculations
              !!COL_RT := na_if(!!Q_COL_RT, 0))
  
  gen = proc_generic_module(df)
  # doing this will output true hit and FA rates (accuracy by target/non-target condition) for calculating SDT metrics in later code
  # TODO: fix functions in math-detection.R to calculate SDT metrics inline. this is a bandaid
  sdt = proc_by_condition(df, "trial_accuracy", Q_COL_CONDITION, FUN = ace_dprime_dplyr) %>%
    rename_with(~stringr::str_replace(., "trial_accuracy_", ""), .cols = everything())
  return (left_join(gen, sdt, by = COL_BID))
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

#' @importFrom magrittr %>%
#' @keywords internal
#' @name ace_procs

module_tnt <- function(df) {
  df$condition = dplyr::recode(df$condition, `tap & trace` = "tap_trace", `tap only` = "tap_only")
  gen = proc_generic_module(df)
  cost = multi_subtract(gen, "\\.tap_trace", "\\.tap_only", "\\.cost")
  sdt = proc_by_condition(df, "trial_accuracy", Q_COL_CONDITION, FUN = ace_dprime_dplyr) %>%
    dplyr::rename_with(~stringr::str_replace(., "trial_accuracy_", ""), .cols = dplyr::everything())
  out <- left_join(dplyr::bind_cols(gen, cost), sdt, by = COL_BID)
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
#' @importFrom stats reshape
#' @importFrom stringr str_sub
#' @importFrom tidyr pivot_wider separate
#' @keywords internal
#' @name ace_procs

module_filter <- function(df) {
  # MT: implementing long format for filter only because it appears only appropriate for this module.
  # open to changing if later modules benefit from this
  
  df <- df %>%
    mutate(cue_rotated = dplyr::recode(cue_rotated,
                                       `0` = "no_change",
                                       `1` = "change"))
  
  acc = proc_by_condition(df, COL_CORRECT_BUTTON, factors = c(Q_COL_CONDITION, sym("cue_rotated")), transform_dir = "long")
  rt = proc_by_condition(df, COL_RT, factors = c(Q_COL_CONDITION, Q_COL_CORRECT_BUTTON), transform_dir = "long")
  rcs = proc_by_condition(df, c(COL_CORRECT_BUTTON, COL_RT), Q_COL_CONDITION, FUN = ace_rcs, transform_dir = "long")
  merged = reduce(list(acc, rt, rcs), left_join, by = c(COL_BID, COL_CONDITION)) %>%
    separate(!!Q_COL_CONDITION, c("targets", "distractors"), sep = 2, remove = FALSE) %>%
    mutate(across(c("targets", "distractors"), ~as.integer(str_sub(., start = -1L)))) %>%
    # TODO: implement k w/ proc_standard (if possible)
    mutate(k = ace_wm_k(correct_button_mean.change, 1 - correct_button_mean.no_change, targets),
           dprime = ace_dprime_wide(correct_button_mean.change, 1 - correct_button_mean.no_change,
                                    correct_button_count.change, correct_button_count.no_change)) %>%
    select(-targets, -distractors) %>%
    pivot_wider(names_from = !!COL_CONDITION,
                values_from = -c(!!Q_COL_BID, !!Q_COL_CONDITION, contains("overall")),
                names_sep = ".")
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
