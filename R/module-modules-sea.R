
#' @keywords internal
#' @name sea_procs

module_math_fluency <- function(df) {
  df <- mutate(df,
               condition = dplyr::case_when(
                 grepl("[+]", .data[[COL_QUESTION_TEXT]]) ~ "addition",
                 grepl("[-]", .data[[COL_QUESTION_TEXT]]) ~ "subtraction",
                 grepl("[x]", .data[[COL_QUESTION_TEXT]]) ~ "multiplication",
                 TRUE ~ NA_character_),
               condition = dplyr::if_else(
                 condition == dplyr::lag(condition),
                 "stay", "switch", missing = "stay")
  )
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION)
  cost = multi_subtract(gen, "\\.stay", "\\.switch", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name sea_procs

module_math_fluency <- function(df) {
  df <- mutate(df,
               condition = dplyr::case_when(
                 grepl("[+]", .data[[COL_QUESTION_TEXT]]) ~ "addition",
                 grepl("[-]", .data[[COL_QUESTION_TEXT]]) ~ "subtraction",
                 grepl("[x]", .data[[COL_QUESTION_TEXT]]) ~ "multiplication",
                 TRUE ~ NA_character_),
               condition = dplyr::if_else(
                 condition == dplyr::lag(condition),
                 "stay", "switch", missing = "stay"),
               digit_load = dplyr::case_when(
                 nchar(.data[[COL_QUESTION_TEXT]]) == 5 ~ "low",
                 nchar(.data[[COL_QUESTION_TEXT]]) == 6 ~ "med",
                 nchar(.data[[COL_QUESTION_TEXT]]) == 7 ~ "high",
                 TRUE ~ NA_character_)
  )
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION, FUN = sea_descriptive_statistics)
  cost = multi_subtract(gen, "\\.stay", "\\.switch", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name sea_procs

module_reading_fluency <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, FUN = sea_descriptive_statistics)
  gen = dplyr::select(gen, -dplyr::contains("correct_button_mean"), -dplyr::contains("correct_button_median"))
  time = proc_by_condition(df, "trial_onset", include_overall = F, FUN = sea_task_duration)
  return (left_join(gen, time, by = COL_BID))
}

#' @keywords internal
#' @name sea_procs

module_reading_comprehension <- function(df) {
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, FUN = sea_descriptive_statistics)
  gen = dplyr::select(gen, -dplyr::contains("correct_button_mean"), -dplyr::contains("correct_button_median"))
  time = proc_by_condition(df, "trial_onset", include_overall = F, FUN = sea_task_duration)
  return (left_join(gen, time, by = COL_BID))
}

#' @keywords internal
#' @name sea_procs

module_fractions_lvl_1 <- function(df) {
  df <- mutate(df,
               num_left = as.numeric(str_sub(.data$question_text, start = 1L, end = 1L)),
               num_right = as.numeric(str_sub(.data$question_text, start = -1L, end = -1L)),
               condition = dplyr::recode(correct_response, `Right side` = "right", `Left side` = "left"))
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "matched_value")
  return ()
}

#' @keywords internal
#' @name sea_procs

module_fractions_lvl_2 <- function(df) {
  df <- mutate(df,
           num_left = as.numeric(str_sub(.data$question_text, start = 9L, end = 9L)),
           denom_left = as.numeric(str_sub(.data$question_text, start = 11L, end = 11L)),
           num_right = as.numeric(str_sub(.data$question_text, start = 14L, end = 14L)),
           denom_right = as.numeric(str_sub(.data$question_text, start = 16L, end = 16L)),
           matched_value = dplyr::case_when(
             num_left == num_right ~ "num_matched",
             denom_left == denom_right ~ "denom_matched",
             TRUE ~ NA_character_)
    )
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "matched_value")
  return ()
}

#' @keywords internal
#' @name sea_procs

module_fractions_lvl_3 <- function(df) {
  df <- mutate(df,
               num_left = as.numeric(str_sub(.data$question_text, start = 9L, end = 9L)),
               denom_left = as.numeric(str_sub(.data$question_text, start = 11L, end = 11L)),
               frac_left = num_left / denom_left,
               num_right = as.numeric(str_sub(.data$question_text, start = 14L, end = 14L)),
               denom_right = as.numeric(str_sub(.data$question_text, start = 16L, end = 16L)),
               frac_right = num_right / denom_right,
               num_larger = dplyr::case_when(
                 correct_response == "Right side" & num_right > num_left ~ 1,
                 correct_response == "Left side" & num_left > num_right ~ 1,
                 TRUE ~ 0
               ))
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, "matched_value")
  return ()
}