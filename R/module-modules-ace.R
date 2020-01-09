
#' @importFrom rlang !!
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  gen = proc_generic_module(df)
  gen$score = (((gen$rt_mean.conjunction_12 - gen$rt_mean.conjunction_4) / gen$rt_mean.conjunction_4) * 100) + 100
  proc_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_median", ace_median) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_median", ace_median)
  proc_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.conjunction_12", "\\.proc_cost_mean", ace_mean) - multi_fun(gen, "\\.feature_4", "\\.feature_12", "\\.proc_cost_mean", ace_mean)
  dist_cost_median = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_median", ace_median) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_median", ace_median)
  dist_cost_mean = multi_fun(gen, "\\.conjunction_4", "\\.feature_4", "\\.dist_cost_mean", ace_mean) - multi_fun(gen, "\\.conjunction_12", "\\.feature_12", "\\.dist_cost_mean", ace_mean)
  conj_cost = multi_subtract(gen, "\\.conjunction_12", "\\.conjunction_4", "\\.conj_cost")
  feat_cost = multi_subtract(gen, "\\.feature_12", "\\.feature_4", "\\.feat_cost")
  return (dplyr::bind_cols(gen, proc_cost_median, proc_cost_mean, dist_cost_median, dist_cost_mean, conj_cost, feat_cost))
}

#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#' @importFrom dplyr mutate mutate_at recode select starts_with
#' @keywords internal
#' @name ace_procs

module_brt <- function(df) {
  if (COL_HANDEDNESS %in% names(df)) {
    df <- df %>%
      mutate_at(COL_HANDEDNESS, tolower) %>%
      mutate(condition_hand = ifelse(grepl("right", !!Q_COL_HANDEDNESS),
                               recode(!!Q_COL_HANDEDNESS, right = "dominant", left = "nondominant"),
                               recode(!!Q_COL_HANDEDNESS, left = "dominant", right = "nondominant")))
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
  return (proc_generic_module(df, col_acc = Q_COL_CORRECT_RESPONSE, col_condition = rlang::sym("cue_type")))
}

#' @keywords internal
#' @name ace_procs

module_flanker <- function(df) {
  gen = proc_generic_module(df, col_condition = Q_COL_TRIAL_TYPE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (dplyr::bind_cols(gen, cost))
}

#' @importFrom dplyr funs left_join mutate na_if rename_all
#' @importFrom magrittr %>%
#' @keywords internal
#' @name ace_procs

module_saat <- function(df) {
  df = replace_empty_values(df, COL_CONDITION, "saattype")
  df = mutate_at(df, COL_CONDITION, tolower) %>%
    # non-response trials should have NA rt, not 0 rt, so it will be excluded from mean calculations
    mutate_at(COL_RT, funs(na_if(., 0)))
 
  gen = proc_generic_module(df)
  # doing this will output true hit and FA rates (accuracy by target/non-target condition) for calculating SDT metrics in later code
  # TODO: fix functions in math-detection.R to calculate SDT metrics inline. this is a bandaid
  sdt = proc_by_condition(df, "trial_accuracy", Q_COL_CONDITION, FUN = ace_dprime_dplyr) %>%
    rename_all(funs(stringr::str_replace(., "trial_accuracy_", "")))
  return (left_join(gen, sdt))
}

#' @keywords internal
#' @name ace_procs

module_stroop <- function(df) {
  gen = proc_generic_module(df, col_condition = Q_COL_TRIAL_TYPE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (dplyr::bind_cols(gen, cost))
}

#' @keywords internal
#' @name ace_procs

module_spatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, Q_COL_CORRECT_BUTTON)
  span = proc_by_condition(df, "object_count", FUN = ace_spatial_span)
  rt_block_half = proc_by_condition(df, COL_RT, factors = Q_COL_BLOCK_HALF, include_overall = F)
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
  gen = proc_generic_module(df, col_condition = rlang::sym("taskswitch_state"))
  cost = multi_subtract(gen, "\\.switch", "\\.stay", "\\.cost")
  return (dplyr::bind_cols(gen, cost))
}

#' @importFrom magrittr %>%
#' @keywords internal
#' @name ace_procs

module_tnt <- function(df) {
  df$condition = plyr::mapvalues(df$condition, from = c("Tap & Trace", "Tap Only"), to = c("tap_trace", "tap_only"), warn_missing = FALSE)
  gen = proc_generic_module(df)
  cost = multi_subtract(gen, "\\.tap_trace", "\\.tap_only", "\\.cost")
  sdt = proc_by_condition(df, "trial_accuracy", Q_COL_CONDITION, FUN = ace_dprime_dplyr) %>%
    dplyr::rename_all(dplyr::funs(stringr::str_replace(., "trial_accuracy_", "")))
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
  # MT: implementing long format for filter only because it appears only appropriate for this module.
  # open to changing if later modules benefit from this
  
  df <- df %>%
    mutate(cue_rotated = dplyr::recode(cue_rotated,
                                       `0` = "no_change",
                                       `1` = "change"))

  acc = proc_by_condition(df, COL_CORRECT_BUTTON, factors = c(Q_COL_CONDITION, sym("cue_rotated")), transform_dir = "long")
  rt = proc_by_condition(df, COL_RT, factors = c(Q_COL_CONDITION, Q_COL_CORRECT_BUTTON), transform_dir = "long")
  merged = left_join(acc, rt, by = c("bid", "condition")) %>%
    separate(!!Q_COL_CONDITION, c("targets", "distractors"), sep = 2, remove = FALSE) %>%
    mutate_at(c("targets", "distractors"), funs(as.integer(str_sub(., start = -1L)))) %>%
    # TODO: implement k w/ proc_standard (if possible)
    mutate(k = ace_wm_k(correct_button_mean.change, 1 - correct_button_mean.no_change, targets)) %>%
    select(-targets, -distractors) %>%
    super_spread(condition, -bid, -condition, name_order = "value_first", sep = ".")

  return (select(merged, -contains(".."), -starts_with(PROC_COL_OLD[1]), -starts_with(PROC_COL_OLD[2])))
}

#' @importFrom rlang !!
#' @keywords internal
#' @name ace_procs

module_ishihara <- function(df) {
  df = dplyr::group_by(df, !!Q_COL_BID)
  return (ungroup(ace_ishihara_dplyr(df, "rg_color_deficiency")))
}

#' @keywords internal
#' @name ace_procs

module_spatialcueing <- function(df) {
  gen = proc_generic_module(df, col_condition = Q_COL_TRIAL_TYPE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (dplyr::bind_cols(gen, cost))
}