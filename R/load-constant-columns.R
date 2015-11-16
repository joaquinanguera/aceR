
#' ACE column name constants
#'
#' @keywords internal
#' @name ace_header
NULL

#' @name ace_header
COL_FILE = "file"

#' @name ace_header
COL_TIME = "time"

#' @name ace_header
COL_RT = "rt"

#' @name ace_header
COL_CORRECT_BUTTON = "correct_button"

#' @name ace_header
COL_CORRECT_RESPONSE = "correct_response"

#' @name ace_header
COL_RW = "rw"

#' @name ace_header
COL_CONDITION = "condition"

#' @name ace_header
COL_PID = "pid"

#' @name ace_header
COL_BID = "bid"

#' @name ace_header
COL_SUB_ID = "subid"

#' @name ace_header
COL_TRIAL_TYPE = "trial_type"

#' @name ace_header

standardize_ace_column_names <- function(df) {
  new = names(df)
  new[new == "response_time"] = COL_RT
  new[new == "response_window"] = COL_RW
  new[new == "participant_id"] = COL_PID
  names(df) = new
  return (df)
}