
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

#' @keywords internal
#' @name ace_procs

module_saat <- function(df) {
  df = replace_empty_values(df, COL_CONDITION, "saattype")
  df[, COL_CONDITION] = tolower(df[, COL_CONDITION])
  # non-response trials should have NA rt, not 0 rt, so it will be excluded from mean calculations
  df[, COL_RT] = na_if(df[, COL_RT], 0)
  # This fixes a condition naming error in the raw log files. Please remove this functionality if this ever gets fixed in the ACE program.
  df[, COL_CONDITION] = plyr::mapvalues(df[, COL_CONDITION], from = c("impulsive", "sustained"), to = c("sustained", "impulsive"), warn_missing = FALSE)
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
  return (merged)
}

#' @keywords internal
#' @name ace_procs

module_filter <- function(df) {
  # MT: implementing long format for filter only because it appears only appropriate for this module. open to changing if later modules benefit from this
  df$cue_rotated = base::as.factor(df$cue_rotated)
  df$cue_rotated = plyr::mapvalues(df$cue_rotated, from = c("0", "1"), to = c("no_change", "change"), warn_missing = FALSE)
  acc = proc_by_condition(df, COL_CORRECT_BUTTON, factors = c(COL_CONDITION, "cue_rotated"), transform_dir = "long")
  rt = proc_by_condition(df, COL_RT, factors = c(COL_CONDITION, COL_CORRECT_BUTTON), transform_dir = "long")
  merged = left_join(acc, rt, by = c("bid" = "bid", "condition" = "condition"))
  merged = tidyr::separate_(merged, COL_CONDITION, c("targets", "distractors"), sep = 2, remove = TRUE)
  merged$targets = as.numeric(plyr::mapvalues(merged$targets, from = c("R2", "R4"), to = c(2, 4)))
  merged$distractors = as.numeric(plyr::mapvalues(merged$distractors, from = c("B0", "B2", "B4"), to = c(0, 2, 4)))
  # TODO: implement k w/ proc_standard (if possible)
  merged$k = ace_wm_k(merged$correct_button_mean.change, 1 - merged$correct_button_mean.no_change, merged$targets)
  out = stats::reshape(as.data.frame(merged), timevar = "targets", idvar = c(COL_BID, "distractors"), direction = "wide")
  return (select(out, -contains(".."), -starts_with(PROC_COL_OLD[1]), -starts_with(PROC_COL_OLD[2])))
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