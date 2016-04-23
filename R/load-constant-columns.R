
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
COL_BLOCK_HALF = "half"

#' @name ace_header

standardize_ace_column_names <- function(df) {
  new = names(df)
  new[new == "response_time"] = COL_RT
  new[new == "response_window"] = COL_RW
  new[new == "participant_id"] = COL_PID
  new[new == "user_id"] = COL_PID
  names(df) = new
  return (df)
}

#' @name ace_header

standardize_ace_values <- function(df) {
  cols = names(df)
  if (COL_CORRECT_BUTTON %in% cols) {
    df[, COL_CORRECT_BUTTON] = plyr::mapvalues(df[, COL_CORRECT_BUTTON], from = c(0, 1), to = c("incorrect", "correct"), warn_missing = FALSE)
  }
  if (COL_CORRECT_RESPONSE %in% cols) {
    df[, COL_CORRECT_RESPONSE] = plyr::mapvalues(df[, COL_CORRECT_RESPONSE], from = c(0, 1), to = c("incorrect", "correct"), warn_missing = FALSE)
  }
  return (df)  
}