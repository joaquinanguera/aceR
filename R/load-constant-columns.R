
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
COL_AGE = "age"

#' @name ace_header
COL_GRADE = "grade"

#' @name ace_header
COL_GENDER = "gender"

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
  new[new == "user_age"] = COL_AGE
  new[new == "user_grade"] = COL_GRADE
  new[new == "user_gender"] = COL_GENDER
  new[new == "time_gameplayed_utc"] = COL_TIME
  new[new == "id"] = COL_SUB_ID
  new[new == "details"] = COL_CONDITION
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