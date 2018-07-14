
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

#' @importFrom dplyr case_when
#' @name ace_header

standardize_ace_column_names <- function(df) {
  new = names(df)
  new = case_when(new == "response_time" ~ COL_RT,
                  new == "response_window" ~ COL_RW,
                  new %in% c("participant_id", "user_id") ~ COL_PID,
                  new == "user_name" ~ COL_NAME,
                  new == "user_age" ~ COL_AGE,
                  new == "user_grade" ~ COL_GRADE,
                  new %in% c("user_gender", "age1", "user_age1") ~ COL_GENDER,
                  new == "user_handedness" ~ COL_HANDEDNESS,
                  new == "time_gameplayed_utc" ~ COL_TIME,
                  new == "time_sent_utc" ~ "timesent_utc",
                  new == "id" ~ COL_SUB_ID,
                  new == "details" ~ COL_CONDITION,
                  new == "task_switch_state" ~ "taskswitch_state",
                  TRUE ~ new) # for cross compatibility b/w emailed and pulvinar)
  names(df) = new
  return (df)
}

#' @name ace_header
#' @importFrom dplyr funs group_by if_else lag mutate mutate_at recode ungroup
#' @importFrom lubridate parse_date_time
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace

standardize_ace_values <- function(df) {
  # TODO: this function should handle re-typing of columns
  # especially in emailed data, all columns are necessarily read in as character
  # we SHOULD hard-code expected type of columns by module
  
  # FIRST: re-type non-character columns to their intended types
  # ACROSS TASK I think:
  df <- df %>%
    mutate(time = str_replace(time, "T", ""), # the T causes parse_date_time to flip out
           time = parse_date_time(time, "Y-m-dHMSz"))
  
  try({
    df <- df %>%
      mutate_at(COL_RT, as.numeric)
  }, silent = TRUE)
  
  try({df <- df %>%
    mutate_at(COL_RW, as.numeric)
  }, silent = TRUE)
  
  try({
    df <- df %>%
      mutate_at(COL_AGE, as.numeric)
  }, silent = TRUE)
  
  cols = names(df)
  
  if (COL_CORRECT_BUTTON %in% cols) {
    if (!(SPATIAL_SPAN %in% df$module) & !(BACK_SPATIAL_SPAN %in% df$module)) {
    df <- df %>%
      mutate(correct_button = dplyr::recode(correct_button, `0` = "incorrect", `1` = "correct"),
             correct_button = case_when(rt < 200 & rt > 0 ~ "incorrect",
                                        rt == rw ~ "no_response",
                                        #In older version, 'no response' trial RTs were replaced with Max Intertrial Interval. 
                                        #However, the value for Max Intertrial Interval is not always recorded in the data output
                                        #This line marks trials with an RT that is evenly divisible by 10
                                        # (i.e. is an integer and divisible by 10) as a 'no response' trial. 
                                        rt %% 10 == 0 & rt != 0 ~ "no_response",
                                        is.na(rt) ~ "no_response",
                                        TRUE ~ correct_button))
    
    } else { # RT for the span tasks is handled differently and is left uninterpreted really
      # so don't do all of this recoding of correctness by RT
      df <- df %>%
        mutate(correct_button = dplyr::recode(correct_button, `0` = "incorrect", `1` = "correct"))
    }
  }
  if (COL_CORRECT_RESPONSE %in% cols) {
    df[, COL_CORRECT_RESPONSE] = dplyr::recode(df[, COL_CORRECT_RESPONSE], `0` = "incorrect", `1` = "correct")
  }
  if (COL_LATE_RESPONSE %in% cols) {
    # original form of this column is 0/1
    df[, COL_LATE_RESPONSE] = case_when(df[, COL_RT] < 200 & df[, COL_RT] > 0 ~ "short",
                                        df[, COL_RT] > df[, COL_RW] ~ "late",
                                        df[, COL_RT] < df[, COL_RW] ~ "early",
                                        df[, COL_RT] > 0 & df[, COL_RT] == df[, COL_RW] ~ "no_response",
                                        df[, COL_RT] %% 10 == 0 & df[, COL_RT] != 0 ~ "no_response",
                                        is.na(df[, COL_RT]) ~ "no_response",
                                        TRUE ~ "late")
  }
  
  # Forcible recoding of accuracy and other things for various modules below
  # Most of this is an attempt to reconstruct accuracy as orthogonal to response lateness
  if (SAAT %in% df$module) {
    # This fixes a condition naming error in the raw log files. Please remove this functionality if this ever gets fixed in the ACE program.
    df[, COL_CONDITION] = plyr::mapvalues(df[, COL_CONDITION], from = c("Impulsive", "Sustained"), to = c("sustained", "impulsive"), warn_missing = FALSE)
    
    #Correct hits and misses. For position is on top, if RT >200ms and not equal to response window, hit, else miss
    #For position in not on top, if RT == 0, then correct rejection, else false alarm
    #This will also ensure RTs < 200ms are incorrect regardless of condition/button press
    df$trial_accuracy = with(df, case_when(position_is_top == 1 & rt >= 200 & rt != rw ~ "Hit",
                                           position_is_top == 1 & (rt < 200 | rt == rw) ~ "Miss",
                                           late_response == "no_response" ~ "Miss", # ??? really?
                                           position_is_top == 0 & rt == 0 ~ "Correct Rejection",
                                           position_is_top == 0 & rt != 0 ~ "False Alarm",
                                           TRUE ~ ""))
    df[, COL_CORRECT_BUTTON] = case_when(df$trial_accuracy %in% c("Hit", "Correct Rejection") ~ "correct",
                                         df$trial_accuracy %in% c("Miss", "False Alarm") ~ "incorrect",
                                         TRUE ~ "")
  } else if (STROOP %in% df$module) {
    df[, COL_CORRECT_BUTTON] = if_else(df$color_pressed == df$color_shown, "correct", "incorrect", missing = "incorrect") # missing implies no response
  } else if (FLANKER %in% df$module) {
    df[(df$displayed_cue %in% c("A", "B") & df$first_button=="YES") | (df$displayed_cue %in% c("C", "D") & df$second_button=="YES"), COL_CORRECT_BUTTON] = "correct"
  } else if (BRT %in% df$module) {
    # retype and clean accuracy
    df <- df %>%
      mutate(inter_time_interval = as.numeric(inter_time_interval),
             correct_button = if_else(rt >= 200 & rt != inter_time_interval,
                                      "correct",
                                      correct_button,
                                      missing = correct_button))
  } else if (TNT %in% df$module) {
    #Make sure late trials are marked as late
    df$late_response = if_else(df$rt > df$rw, "late", "early", missing = "late")
    #Correct hits and misses. For is valid cue, if RT >200ms and not equal to response window, hit, else miss
    #For is not valid cue, if RT == 0, then correct rejection, else false alarm
    #This will also ensure RTs < 200ms are incorrect regardless of condition/button press
    df$trial_accuracy = with(df, case_when(is_valid_cue == 1 & rt >= 200 & rt != rw ~ "Hit",
                                           is_valid_cue == 1 & (rt < 200 | rt == rw) ~ "Miss",
                                           late_response == "no_response" ~ "Miss", # ??? really?
                                           is_valid_cue == 0 & rt == 0 ~ "Correct Rejection",
                                           is_valid_cue == 0 & rt != 0 ~ "False Alarm",
                                           TRUE ~ ""))
    df[, COL_CORRECT_BUTTON] = case_when(df$trial_accuracy %in% c("Hit", "Correct Rejection") ~ "correct",
                                         df$trial_accuracy %in% c("Miss", "False Alarm") ~ "incorrect",
                                         TRUE ~ "")
  } else if (FILTER %in% df$module) {
    #Add in trial_accuracy labels for Filter. For cue is rotated, if RT >200ms and not equal to response window, and correct_button is correct, hit, else miss
    #For cue is not rotated, if RT >200ms and not equal to response window, and correct_button is correct, then correct rejection, else false alarm
    #This will also ensure RTs < 200ms are incorrect regardless of condition/button press
    df$trial_accuracy = with(df, case_when(cue_rotated == 1 & rt >= 200 & correct_button == "correct" ~ "Hit",
                                           cue_rotated == 1 & rt >= 200 & correct_button == "incorrect" ~ "Miss",
                                           cue_rotated == 0 & rt >= 200 & correct_button == "correct" ~ "Correct Rejection",
                                           cue_rotated == 0 & rt >- 200 & correct_button == "incorrect" ~ "False Alarm",
                                           is.na(rt) ~ "no_response",
                                           rt == rw ~ "no_response",
                                           TRUE ~ ""))
    df[, COL_CORRECT_BUTTON] = case_when(df$trial_accuracy %in% c("Hit", "Correct Rejection") ~ "correct",
                                         df$trial_accuracy %in% c("Miss", "False Alarm") ~ "incorrect",
                                         TRUE ~ "")
  } else if (SPATIAL_SPAN %in% df$module | BACK_SPATIAL_SPAN %in% df$module) {
    df$object_count = as.numeric(df$object_count)
  }
  
  # needs to be called LAST, after all the other boutique accuracy corrections are complete
  if (COL_CORRECT_BUTTON %in% cols) {
    df <- df %>%
      # needs to be grouped to prevent previous_correct_button from bleeding over between records
      group_by(bid) %>%
      mutate(previous_correct_button = lag(correct_button)) %>%
      ungroup()
  }
  
  return (df)
}