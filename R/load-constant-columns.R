
#' ACE column name constants
#'
#' @keywords internal
#' @name ace_header
NULL

#' @name ace_header
COL_RT = "rt"

#' @name ace_header
COL_ACC = "acc"

#' @name ace_header
COL_RW = "rw"

#' @name ace_header
COL_CONDITION = "condition"

#' @name ace_header
COL_PID = "pid"

#' @name ace_header

standardize_ace_column_names <- function(df) {
  new = names(df)
  new[new == "response_time"] = COL_RT
  new[new == "correct_button"] = COL_ACC
  # new[new == "correct_response"] = COL_ACC
  new[new == "response_window"] = COL_RW
  new[new == "participant_id"] = COL_PID
  names(df) = new
  return (df)
}