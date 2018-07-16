
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION)
  gen$score = (((gen$rt_mean.conjunction_12 - gen$rt_mean.conjunction_4) / gen$rt_mean.conjunction_4) * 100) + 100
  proc_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_median", ace_median) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_median", ace_median)
  proc_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_mean", ace_mean) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_mean", ace_mean)
  dist_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_median", ace_median) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_median", ace_median)
  dist_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_mean", ace_mean) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_mean", ace_mean)
  conj_cost = multi_subtract(gen, "\\.conjunction_12", "\\.conjunction_4", "\\.conj_cost")
  feat_cost = multi_subtract(gen, "\\.feature_12", "\\.feature_4", "\\.feat_cost")
  return (data.frame(gen, proc_cost_median, proc_cost_mean, dist_cost_median, dist_cost_mean, conj_cost, feat_cost))
}

#' @keywords internal
#' @name ace_procs

module_brt <- function(df) {
  if (COL_HANDEDNESS %in% names(df)) {
    df[, COL_HANDEDNESS] = tolower(df[, COL_HANDEDNESS])
    df$condition_hand = ifelse(grepl("right", df[, COL_HANDEDNESS]),
                               recode(df[, COL_HANDEDNESS], "right" = "dominant", "left" = "nondominant"),
                               recode(df[, COL_HANDEDNESS], "left" = "dominant", "right" = "nondominant"))
    gen = proc_generic_module(df, COL_CORRECT_BUTTON, "condition_hand")
  } else {
    warning("No handedness data found. Unable to label BRT data by dominant hand")
    gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION)
  }
  gen = select(gen, -starts_with(PROC_COL_OLD[1]), -starts_with(PROC_COL_OLD[2]))
  return (gen)
}

#' @keywords internal
#' @name ace_procs

module_discrimination <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_RESPONSE, "cue_type"))
}

#' @keywords internal
#' @name ace_procs

module_flanker <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (data.frame(gen, cost))
}

#' @importFrom dplyr mutate na_if
#' @keywords internal
#' @name ace_procs

module_saat <- function(df) {
  df = replace_empty_values(df, COL_CONDITION, "saattype")
  df = mutate_at(df, COL_CONDITION, tolower)
  # non-response trials should have NA rt, not 0 rt, so it will be excluded from mean calculations
  df[, COL_RT] = na_if(df[, COL_RT], 0)
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION)
  # doing this will output true hit and FA rates (accuracy by target/non-target condition) for calculating SDT metrics in later code
  # TODO: fix functions in math-detection.R to calculate SDT metrics inline. this is a bandaid
  sdt = proc_by_condition(df, "trial_accuracy", COL_CONDITION, FUN = ace_dprime_dplyr)
  names(sdt) = stringr::str_replace(names(sdt), "trial_accuracy_", "")
  return (left_join(gen, sdt))
}

#' @keywords internal
#' @name ace_procs

module_stroop <- function(df) {
  df[df$color_pressed == df$color_shown, COL_CORRECT_BUTTON] = "correct" # repairing error where all late responses are marked "incorrect"
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name ace_procs

module_spatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, COL_CORRECT_BUTTON)
  span = proc_by_condition(df, "object_count", FUN = ace_spatial_span)
  rt_block_half = proc_by_condition(df, COL_RT, factors = COL_BLOCK_HALF, include_overall = F)
  analy = list(rt, span, rt_block_half)
  merged = multi_merge(analy, by = COL_BID)
  # Assume that all subjects who return a span less than 3 are technical failures and scrub
  merged = dplyr::filter(merged, object_count_span.overall >= 3)
  return (merged)
}

#' @keywords internal
#' @name ace_procs

module_taskswitch <- function(df) {
  df$taskswitch_state = plyr::mapvalues(df$taskswitch_state, from = c(0, 1 , 2), to = c("start", "switch", "stay"), warn_missing = FALSE)
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "taskswitch_state")
  cost = multi_subtract(gen, "\\.switch", "\\.stay", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name ace_procs

module_tnt <- function(df) {
  df$condition = plyr::mapvalues(df$condition, from = c("Tap & Trace", "Tap Only"), to = c("tap_trace", "tap_only"), warn_missing = FALSE)
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION)
  cost = multi_subtract(gen, "\\.tap_trace", "\\.tap_only", "\\.cost")
  sdt = proc_by_condition(df, "trial_accuracy", COL_CONDITION, FUN = ace_dprime_dplyr)
  names(sdt) = stringr::str_replace(names(sdt), "trial_accuracy_", "")
  return (data.frame(gen, cost, sdt))
}

#' @keywords internal
#' @name ace_procs

module_backwardsspatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, COL_CORRECT_BUTTON)
  span = proc_by_condition(df, "object_count", FUN = ace_spatial_span)
  rt_block_half = proc_by_condition(df, COL_RT, factors = COL_BLOCK_HALF, include_overall = F)
  analy = list(rt, span, rt_block_half)
  merged = multi_merge(analy, by = COL_BID)
  merged = dplyr::filter(merged, object_count_span.overall >= 3)
  return (merged)
}

#' @import dplyr
#' @importFrom rlang sym !!
#' @importFrom stats reshape
#' @importFrom stringr str_sub
#' @importFrom tidyr separate
#' @keywords internal
#' @name ace_procs

module_filter <- function(df) {
  # MT: implementing long format for filter only because it appears only appropriate for this module. open to changing if later modules benefit from this
  df <- df %>%
    mutate(cue_rotated = dplyr::recode(cue_rotated,
                                       `0` = "no_change",
                                       `1` = "change"))

  acc = proc_by_condition(df, COL_CORRECT_BUTTON, factors = c(COL_CONDITION, "cue_rotated"), transform_dir = "long")
  rt = proc_by_condition(df, COL_RT, factors = c(COL_CONDITION, COL_CORRECT_BUTTON), transform_dir = "long")
  merged = left_join(acc, rt, by = c("bid", "condition")) %>%
    separate(!!sym(COL_CONDITION), c("targets", "distractors"), sep = 2, remove = FALSE) %>%
    mutate_at(c("targets", "distractors"), funs(as.integer(str_sub(., start = -1L)))) %>%
    # TODO: implement k w/ proc_standard (if possible)
    mutate(k = ace_wm_k(correct_button_mean.change, 1 - correct_button_mean.no_change, targets)) %>%
    select(-targets, -distractors) %>%
    super_spread(condition, -bid, -condition, name_order = "value_first", sep = ".")

  return (select(merged, -contains(".."), -starts_with(PROC_COL_OLD[1]), -starts_with(PROC_COL_OLD[2])))
}

#' @keywords internal
#' @name ace_procs

module_ishihara <- function(df) {
  df = group_by_(df, COL_BID)
  return (ungroup(ace_ishihara_dplyr(df, "rg_color_deficiency")))
}

#' @keywords internal
#' @name ace_procs

module_spatialcueing <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (data.frame(gen, cost))
}