
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

module_stroop <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE, TRUE))
}

# TODO: spatialspan

#' @keywords internal
#' @name ace_procs

module_taskswitch <- function(df) {
  df$taskswitch_state = plyr::mapvalues(df$taskswitch_state, from = c(0, 1 , 2), to = c("start", "switch", "stay"))
  return (proc_generic_module(df, COL_CORRECT_BUTTON, "taskswitch_state", FALSE))
}
