
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, TRUE))
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
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE, TRUE))
}

#' @keywords internal
#' @name ace_procs

module_saat <- function(df) {
  df$trial_type = plyr::mapvalues(df$position_is_top, from = c("0", "1"), to = c("nontarget", "target"))
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, TRUE)
  return (gen)
}

#' @keywords internal
#' @name ace_procs

module_stroop <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE, TRUE))
}

#' @keywords internal
#' @name ace_procs

module_spatialspan <- function(df) {
  rt = proc_by_condition(df, COL_RT, COL_CORRECT_BUTTON, FUN = ace_descriptive_statistics)
  acc = proc_standard(df, COL_CORRECT_BUTTON, col_condition = NULL, FUN = ace_descriptive_statistics, y = c(COL_BID), suffix = "overall")
  span = proc_standard(df, "object_count", col_condition = NULL, FUN = ace_spatial_span, y = c(COL_BID), suffix = "overall")
  analy = list(rt, acc, span)
  merged = multi_merge(analy, by = COL_BID)
  return (merged)
}

#' @keywords internal
#' @name ace_procs

module_taskswitch <- function(df) {
  df$taskswitch_state = plyr::mapvalues(df$taskswitch_state, from = c(0, 1 , 2), to = c("start", "switch", "stay"))
  return (proc_generic_module(df, COL_CORRECT_BUTTON, "taskswitch_state", FALSE))
}

#' @keywords internal
#' @name ace_procs

module_tnt <- function(df) {
  df$condition = plyr::mapvalues(df$condition, from = c("Tap & Trace", "Tap Only"), to = c("tap_trace", "tap_only"))
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, TRUE))
}
