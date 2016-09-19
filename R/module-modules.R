
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, TRUE)
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
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, TRUE))
}

#' @keywords internal
#' @name ace_procs

module_discrimination <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_RESPONSE, "cue_type", FALSE))
}

#' @keywords internal
#' @name ace_procs

module_flanker <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE, TRUE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name ace_procs

module_saat <- function(df) {
  df = replace_empty_values(df, COL_CONDITION, "saattype")
  df[, COL_CONDITION] = tolower(df[, COL_CONDITION])
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, TRUE)
  return (gen)
}

#' @keywords internal
#' @name ace_procs

module_stroop <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE, TRUE)
  cost = multi_subtract(gen, "\\.incongruent", "\\.congruent", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name ace_procs

module_spatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, COL_CORRECT_BUTTON, FUN = ace_descriptive_statistics)
  acc = proc_standard(df, COL_CORRECT_BUTTON, col_condition = NULL, FUN = ace_descriptive_statistics, y = c(COL_BID), suffix = "overall")
  span = proc_standard(df, "object_count", col_condition = NULL, FUN = ace_spatial_span, y = c(COL_BID), suffix = "overall")
  rt_block_half = proc_standard(df, COL_RT, NULL, factor = COL_BLOCK_HALF, FUN = ace_descriptive_statistics_by_group)
  analy = list(rt, acc, span, rt_block_half)
  merged = multi_merge(analy, by = COL_BID)
  return (merged)
}

#' @keywords internal
#' @name ace_procs

module_taskswitch <- function(df) {
  df$taskswitch_state = plyr::mapvalues(df$taskswitch_state, from = c(0, 1 , 2), to = c("start", "switch", "stay"), warn_missing = FALSE)
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "taskswitch_state", FALSE)
  cost = multi_subtract(gen, "\\.switch", "\\.stay", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name ace_procs

module_tnt <- function(df) {
  df$condition = plyr::mapvalues(df$condition, from = c("Tap & Trace", "Tap Only"), to = c("tap_trace", "tap_only"), warn_missing = FALSE)
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, TRUE)
  cost = multi_subtract(gen, "\\.tap_trace", "\\.tap_only", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name ace_procs

module_backwardsspatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, COL_CORRECT_BUTTON, FUN = ace_descriptive_statistics)
  acc = proc_standard(df, COL_CORRECT_BUTTON, col_condition = NULL, FUN = ace_descriptive_statistics, y = c(COL_BID), suffix = "overall")
  span = proc_standard(df, "object_count", col_condition = NULL, FUN = ace_spatial_span, y = c(COL_BID), suffix = "overall")
  rt_block_half = proc_standard(df, COL_RT, NULL, factor = COL_BLOCK_HALF, FUN = ace_descriptive_statistics_by_group)
  analy = list(rt, acc, span, rt_block_half)
  merged = multi_merge(analy, by = COL_BID)
  return (merged)
}

module_filter <- function(df) {
  # MT: implementing long format for filter only because it appears only appropriate for this module. open to changing if later modules benefit from this
  df$cue_rotated = base::as.factor(df$cue_rotated)
  df$cue_rotated = plyr::mapvalues(df$cue_rotated, from = c("0", "1"), to = c("no_change", "change"), warn_missing = FALSE)
  df = proc_standard(df, COL_CORRECT_BUTTON, COL_CONDITION, factor = "cue_rotated", FUN = ace_descriptive_statistics_by_group, transform_dir = "long")
  df = tidyr::separate_(df, COL_CONDITION, c("targets", "distractors"), sep = 2, remove = TRUE)
  df$targets = as.numeric(plyr::mapvalues(df$targets, from = c("R2", "R4"), to = c(2, 4)))
  df$distractors = as.numeric(plyr::mapvalues(df$distractors, from = c("B0", "B2", "B4"), to = c(0, 2, 4)))
  # MT: will try to bring this implementation in line w/ proc_standard later, but it asks for 3 inputs and is long form so maybe not
  filter_k = ace_wm_k(df$correct_button_mean.change, 1 - df$correct_button_mean.no_change, df$targets)
  return (data.frame(df, k = filter_k))
}

module_ishihara <- function(df) {
  return (proc_standard(df, "trial_correct", col_condition = NULL, FUN = ace_ishihara, y = c(COL_BID)))
}