
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION))
}

#' @keywords internal
#' @name ace_procs

module_brt <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION))
}

#' @keywords internal
#' @name ace_procs

module_discrimination <- function(df) {
  return (proc_generic_module(df, COL_CORRECT_RESPONSE, "cue_type"))
}