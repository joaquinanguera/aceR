
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

# TODO: see about xls files

# #' @keywords internal
# #' @name ace_procs
# 
# module_flanker <- function(df) {
#   return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE, TRUE))
# }

#' @keywords internal
#' @name ace_procs

module_stroop <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_TRIAL_TYPE, TRUE))
}
