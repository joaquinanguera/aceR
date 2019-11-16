
#' ACE column name constants
#'
#' @keywords internal
#' @name ace_header
NULL

#' @name ace_header
COL_FILE = "file"

#' @name ace_header
Q_COL_FILE = rlang::sym(COL_FILE)

#' @name ace_header
COL_TIME = "time"

#' @name ace_header
Q_COL_TIME = rlang::sym(COL_TIME)

#' @name ace_header
COL_N_FINISHED = "times_finished_game"

#' @name ace_header
Q_COL_N_FINISHED = rlang::sym(COL_N_FINISHED)

#' @name ace_header
COL_RT = "rt"

#' @name ace_header
Q_COL_RT = rlang::sym(COL_RT)

#' @name ace_header
COL_CORRECT_BUTTON = "correct_button"

#' @name ace_header
Q_COL_CORRECT_BUTTON = rlang::sym(COL_CORRECT_BUTTON)

#' @name ace_header
COL_PREV_CORRECT_BUTTON = "previous_correct_button"

#' @name ace_header
Q_COL_PREV_CORRECT_BUTTON = rlang::sym(COL_PREV_CORRECT_BUTTON)

#' @name ace_header
COL_CORRECT_RESPONSE = "correct_response"

#' @name ace_header
Q_COL_CORRECT_RESPONSE = rlang::sym(COL_CORRECT_RESPONSE)

#' @name ace_header
COL_RW = "rw"

#' @name ace_header
Q_COL_RW = rlang::sym(COL_RW)

#' @name ace_header
COL_LATE_RESPONSE = "late_response"

#' @name ace_header
Q_COL_LATE_RESPONSE = rlang::sym(COL_LATE_RESPONSE)

#' @name ace_header
COL_CONDITION = "condition"

#' @name ace_header
Q_COL_CONDITION = rlang::sym(COL_CONDITION)

#' @name ace_header
COL_STUDY_COND = "study_condition"

#' @name ace_header
Q_COL_STUDY_COND = rlang::sym(COL_STUDY_COND)

#' @name ace_header
COL_PID = "pid"

#' @name ace_header
Q_COL_PID = rlang::sym(COL_PID)

#' @name ace_header
COL_BID = "bid"

#' @name ace_header
Q_COL_BID = rlang::sym(COL_BID)

#' @name ace_header
COL_BID_SHORT = "bid_short"

#' @name ace_header
Q_COL_BID_SHORT = rlang::sym(COL_BID_SHORT)

#' @name ace_header
COL_NAME = "name"

#' @name ace_header
Q_COL_NAME = rlang::sym(COL_NAME)

#' @name ace_header
COL_AGE = "age"

#' @name ace_header
Q_COL_AGE = rlang::sym(COL_AGE)

#' @name ace_header
COL_GRADE = "grade"

#' @name ace_header
Q_COL_GRADE = rlang::sym(COL_GRADE)

#' @name ace_header
COL_GENDER = "gender"

#' @name ace_header
Q_COL_GENDER = rlang::sym(COL_GENDER)

#' @name ace_header
COL_HANDEDNESS = "handedness"

#' @name ace_header
Q_COL_HANDEDNESS = rlang::sym(COL_HANDEDNESS)

#' @name ace_header
COL_SUB_ID = "subid"

#' @name ace_header
Q_COL_SUB_ID = rlang::sym(COL_SUB_ID)

#' @name ace_header
COL_TRIAL_TYPE = "trial_type"

#' @name ace_header
Q_COL_TRIAL_TYPE = rlang::sym(COL_TRIAL_TYPE)

#' @name ace_header
COL_BLOCK_HALF = "half"

#' @name ace_header
Q_COL_BLOCK_HALF = rlang::sym(COL_BLOCK_HALF)

#' @name ace_header
ALL_POSSIBLE_DEMOS <- c(COL_BID, COL_PID, COL_AGE, COL_GENDER, COL_HANDEDNESS, COL_FILE)

#' @name ace_header
Q_ALL_POSSIBLE_DEMOS <- c(Q_COL_BID, Q_COL_PID, Q_COL_AGE, Q_COL_GENDER, Q_COL_HANDEDNESS, Q_COL_TIME, Q_COL_FILE)

#' @name ace_header
ALL_POSSIBLE_EXPLORE_DEMOS = c("updated_at", "o_s_version", "app_id", "build", "client_time_zone",
                               "client_time_zone_offset", "device_model", "device_name",
                               "device_type", "games_map_dialog_stage", "games_play_count",
                               "games_scores", "graphics_device_name", "handedness",
                               "i18n", "install_mode", "processor_count",
                               "processor_frequency", "runtime_platform", "section",
                               "system_memory_size", "times_finished_game", "games_played")

#' @name ace_header
Q_ALL_POSSIBLE_EXPLORE_DEMOS = rlang::syms(ALL_POSSIBLE_EXPLORE_DEMOS)

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
                  # created_at seems to be ACE Explore specific. beware versioning issues
                  new %in% c("time_gameplayed_utc", "created_at") ~ COL_TIME,
                  new == "time_sent_utc" ~ "timesent_utc",
                  new == "id" ~ COL_SUB_ID,
                  new == "details" ~ COL_CONDITION,
                  new == "task_switch_state" ~ "taskswitch_state",
                  TRUE ~ new) # for cross compatibility b/w emailed and pulvinar)
  names(df) = new
  return (df)
}

#' @name ace_header
#' @import dplyr
#' @importFrom lubridate parse_date_time
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @importFrom stringr str_replace

standardize_ace_values <- function(df) {
  # this function handles re-typing of columns
  # especially in emailed data, all columns are necessarily read in as character
  # we SHOULD hard-code expected type of columns by module
  
  # FIRST: re-type non-character columns to their intended types
  # ACROSS TASK I think:
  try({
    df <- df %>%
      mutate(# !!COL_TIME := str_replace(!!Q_COL_TIME, "T", ""), # the T causes parse_date_time to flip out
        # parse_date_time appears to be behaving okay with the T in between the date and time... as of apr 27 2019
             time1 = suppressWarnings(parse_date_time(!!Q_COL_TIME, "ymdHMSz")),
             time2 = suppressWarnings(parse_date_time(!!Q_COL_TIME, "abdyHMSz")),
             !!COL_TIME := coalesce(time1, time2)) %>%
      select(-time1, -time2)
  }, silent = TRUE)
  
  try({
    df <- df %>%
      mutate_at(COL_RT, as.numeric)
  }, silent = TRUE)
  
  try({
    df <- df %>%
    mutate_at(COL_RW, as.numeric)
  }, silent = TRUE)
  
  try({
    df <- df %>%
      mutate_at(COL_AGE, as.numeric)
  }, silent = TRUE)
  
  cols = names(df)
  
  short_rt_cutoff <- 150
   
  
  # First, for ALL tasks, code correct_button with words, not 0 and 1
  
  try({
    df <- df %>%
      mutate(!!COL_CORRECT_BUTTON := dplyr::recode(!!Q_COL_CORRECT_BUTTON, `0` = "incorrect", `1` = "correct"))
  }, silent = TRUE)
  
  if (DEMOS %in% df$module) {
    df <- df %>%
      select(c(COL_MODULE, ALL_POSSIBLE_DEMOS, COL_TIME)) %>%
      mutate_at(COL_GENDER, as.character)
  }
  
  # (mostly) module-general recoding of short RT trials etc
  #In older version, 'no response' trial RTs were replaced with Max Intertrial Interval. 
  #However, the value for Max Intertrial Interval is not always recorded in the data output
  #This line marks trials with an RT that is evenly divisible by 10
  # (i.e. is an integer and divisible by 10) as a 'no response' trial. 
  # RT for the span tasks is handled differently and is left uninterpreted really
  # so don't do all of this recoding of correctness by RT
  if (COL_CORRECT_BUTTON %in% cols & !(SPATIAL_SPAN %in% df$module) & !(BACK_SPATIAL_SPAN %in% df$module)) {
    
    df <- df %>%
      mutate(!!COL_CORRECT_BUTTON := case_when(!!Q_COL_RT < short_rt_cutoff & !!Q_COL_RT > 0 ~ "incorrect",
                                        !!Q_COL_RT == !!Q_COL_RW ~ "no_response",
                                        !!Q_COL_RT %% 10 == 0 & !!Q_COL_RT != 0 ~ "no_response",
                                        is.na(!!Q_COL_RT) ~ "no_response",
                                        TRUE ~ !!Q_COL_CORRECT_BUTTON))
    
  }
  
  if (COL_LATE_RESPONSE %in% cols) {
    # original form of this column is 0/1
    df[[COL_LATE_RESPONSE]] = case_when(df[[COL_RT]] < short_rt_cutoff & df[[COL_RT]] > 0 ~ "short",
                                        df[[COL_RT]] > df[[COL_RW]] ~ "late",
                                        df[[COL_RT]] < df[[COL_RW]] ~ "early",
                                        df[[COL_RT]] > 0 & df[[COL_RT]] == df[[COL_RW]] ~ "no_response",
                                        df[[COL_RT]] %% 10 == 0 & df[[COL_RT]] != 0 ~ "no_response",
                                        is.na(df[[COL_RT]]) ~ "no_response",
                                        TRUE ~ "late")
  }
  
  # Forcible recoding of accuracy and other things for various modules below
  # Most of this is an attempt to reconstruct accuracy as orthogonal to response lateness
  
  if (SAAT %in% df$module) {
    # This fixes a condition naming error in the raw log files. Please remove this functionality if this ever gets fixed in the ACE program.
    df[[COL_CONDITION]] = plyr::mapvalues(df[[COL_CONDITION]], from = c("Impulsive", "Sustained"), to = c("sustained", "impulsive"), warn_missing = FALSE)
    
    #Correct hits and misses. For position is on top, if RT >cutoff and not equal to response window, hit, else miss
    #For position in not on top, if RT == 0, then correct rejection, else false alarm
    #This will also ensure RTs < cutoff are incorrect regardless of condition/button press
    df$trial_accuracy = with(df, case_when(position_is_top == 1 & rt >= short_rt_cutoff & rt != rw ~ "Hit",
                                           position_is_top == 1 & (rt < short_rt_cutoff | rt == rw) ~ "Miss",
                                           position_is_top == 1 & late_response == "no_response" ~ "Miss", # ??? really?
                                           position_is_top == 0 & rt == 0 ~ "Correct Rejection",
                                           position_is_top == 0 & rt != 0 ~ "False Alarm",
                                           TRUE ~ ""))
    df[[COL_CORRECT_BUTTON]] = case_when(df$trial_accuracy %in% c("Hit", "Correct Rejection") ~ "correct",
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
             correct_button = if_else(rt >= short_rt_cutoff & rt != inter_time_interval,
                                      "correct",
                                      correct_button,
                                      missing = correct_button))
  } else if (TNT %in% df$module) {
    #Make sure late trials are marked as late
    df$late_response = if_else(df$rt > df$rw, "late", "early", missing = "late")
    #Correct hits and misses. For is valid cue, if RT >cutoff and not equal to response window, hit, else miss
    #For is not valid cue, if RT == 0, then correct rejection, else false alarm
    #This will also ensure RTs < cutoff are incorrect regardless of condition/button press
    df$trial_accuracy = with(df, case_when(is_valid_cue == 1 & rt >= short_rt_cutoff & rt != rw ~ "Hit",
                                           is_valid_cue == 1 & (rt < short_rt_cutoff | rt == rw) ~ "Miss",
                                           is_valid_cue == 1 & late_response == "no_response" ~ "Miss", # ??? really?
                                           is_valid_cue == 0 & rt == 0 ~ "Correct Rejection",
                                           is_valid_cue == 0 & rt != 0 ~ "False Alarm",
                                           TRUE ~ ""))
    df[[COL_CORRECT_BUTTON]] = case_when(df$trial_accuracy %in% c("Hit", "Correct Rejection") ~ "correct",
                                         df$trial_accuracy %in% c("Miss", "False Alarm") ~ "incorrect",
                                         TRUE ~ "")
  } else if (FILTER %in% df$module) {
    
    # special column re-typing for filter only
    
    df <- df %>%
      mutate(original_orientation = as.numeric(original_orientation),
             degree_of_change = as.numeric(degree_of_change),
             cue_rotated = as.integer(cue_rotated))
    
    # NOW HERE FOR BACKWARDS COMPATIBILITY:
    # in the past (before 2019?), degree_of_change was the meaningful variable of adaptation
    # hence this re-patching is sometimes necessary
    
    if (any(!is.na(df$degree_of_change))) {
      df <- df %>%
        mutate(# 180 degree rotation was incorrectly marked as "change" when there's no visual change
               cue_rotated = if_else(abs(round(degree_of_change, 2)) == 3.14,
                                     0L,
                                     cue_rotated),
               !!COL_CORRECT_BUTTON := case_when(abs(round(degree_of_change, 2)) == 3.14 & correct_button == "correct" ~ "incorrect",
                                                 abs(round(degree_of_change, 2)) == 3.14 & correct_button == "incorrect" ~ "correct",
                                                 TRUE ~ correct_button))
    }
    
    #Add in trial_accuracy labels for Filter. For cue is rotated, if RT >cutoff and not equal to response window, and correct_button is correct, hit, else miss
    #For cue is not rotated, if RT >cutoff and not equal to response window, and correct_button is correct, then correct rejection, else false alarm
    #This will also ensure RTs < cutoff are incorrect regardless of condition/button press
    df <- df %>%
      mutate(trial_accuracy = case_when(cue_rotated == 1 & rt >= short_rt_cutoff & correct_button == "correct" ~ "Hit",
                                        cue_rotated == 1 & rt >= short_rt_cutoff & correct_button == "incorrect" ~ "Miss",
                                        cue_rotated == 0 & rt >= short_rt_cutoff & correct_button == "correct" ~ "Correct Rejection",
                                        cue_rotated == 0 & rt >- short_rt_cutoff & correct_button == "incorrect" ~ "False Alarm",
                                        is.na(rt) ~ "no_response",
                                        rt == rw ~ "no_response",
                                        TRUE ~ ""),
             !!COL_CORRECT_BUTTON := case_when(trial_accuracy %in% c("Hit", "Correct Rejection") ~ "correct",
                                               trial_accuracy %in% c("Miss", "False Alarm") ~ "incorrect",
                                               TRUE ~ ""))
    
  } else if (SPATIAL_SPAN %in% df$module | BACK_SPATIAL_SPAN %in% df$module) {
    df <- df %>%
      mutate(object_count = as.numeric(object_count)) %>%
      mutate_at(vars(starts_with("point_")), as.character)
  }
  
  # needs to be called LAST, after all the other boutique accuracy corrections are complete
  if (COL_CORRECT_BUTTON %in% cols) {
    df <- df %>%
      # needs to be grouped to prevent previous_correct_button from bleeding over between records
      group_by(bid) %>%
      mutate(!!Q_COL_PREV_CORRECT_BUTTON := lag(!!Q_COL_CORRECT_BUTTON),
             !!Q_COL_PREV_CORRECT_BUTTON := paste0("prev_", !!Q_COL_PREV_CORRECT_BUTTON)) %>%
      ungroup()
  }
  
  return (df)
}
