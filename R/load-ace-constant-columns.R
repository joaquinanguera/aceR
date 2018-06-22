
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
COL_LATE_RESPONSE = "late_response"

#' @name ace_header
COL_CONDITION = "condition"

#' @name ace_header
COL_STUDY_COND = "study_condition"

#' @name ace_header
COL_PID = "pid"

#' @name ace_header
COL_BID = "bid"

#' @name ace_header
COL_NAME = "name"

#' @name ace_header
COL_AGE = "age"

#' @name ace_header
COL_GRADE = "grade"

#' @name ace_header
COL_GENDER = "gender"

#' @name ace_header
COL_HANDEDNESS = "handedness"

#' @name ace_header
COL_SUB_ID = "subid"

#' @name ace_header
COL_TRIAL_TYPE = "trial_type"

#' @name ace_header
COL_BLOCK_HALF = "half"

#' @name ace_header
ALL_POSSIBLE_DEMOS <- c(COL_BID, COL_PID, COL_AGE, COL_GRADE, COL_GENDER, COL_TIME, COL_FILE)

#' @name ace_header

standardize_ace_column_names <- function(df) {
  new = names(df)
  new[new == "response_time"] = COL_RT
  new[new == "response_window"] = COL_RW
  new[new == "participant_id"] = COL_PID
  new[new == "user_id"] = COL_PID
  new[new == "user_name"] = COL_NAME
  new[new == "user_age"] = COL_AGE
  new[new == "user_grade"] = COL_GRADE
  new[new == "user_gender"] = COL_GENDER
  new[new == "age1"] = COL_GENDER
  new[new == "user_age1"] = COL_GENDER
  new[new == "user_handedness"] = COL_HANDEDNESS
  new[new == "time_gameplayed_utc"] = COL_TIME
  new[new == "time_sent_utc"] = "timesent_utc"
  new[new == "id"] = COL_SUB_ID
  new[new == "details"] = COL_CONDITION
  new[new == "task_switch_state"] = "taskswitch_state" # for cross compatibility b/w emailed and pulvinar
  names(df) = new
  return (df)
}

#' @name ace_header
#' @importFrom dplyr recode

standardize_ace_values <- function(df) {
  cols = names(df)
  if (COL_CORRECT_BUTTON %in% cols) {
    # TODO: Forcibly recode correct button using trial accuracy (hit/CR to "correct", miss/FA to "incorrect"?)
    df[, COL_CORRECT_BUTTON] = dplyr::recode(df[, COL_CORRECT_BUTTON], `0` = "incorrect", `1` = "correct")
    # automatically render all RTs < 150 ms to be incorrect responses
    suppressWarnings({
      df[, COL_CORRECT_BUTTON][as.numeric(df[, COL_RT]) < 150] <- "incorrect"
    })
  }
  if (COL_CORRECT_RESPONSE %in% cols) {
    df[, COL_CORRECT_RESPONSE] = dplyr::recode(df[, COL_CORRECT_RESPONSE], `0` = "incorrect", `1` = "correct")
  }
  if (COL_LATE_RESPONSE %in% cols) {
    df[, COL_LATE_RESPONSE] = dplyr::recode(df[, COL_LATE_RESPONSE], `0` = "early", `1` = "late")
  }
  if ("SAAT" %in% df$module) {
    # This fixes a condition naming error in the raw log files. Please remove this functionality if this ever gets fixed in the ACE program.
    df[, COL_CONDITION] = plyr::mapvalues(df[, COL_CONDITION], from = c("Impulsive", "Sustained"), to = c("sustained", "impulsive"), warn_missing = FALSE)
  }
  return (df)  
}