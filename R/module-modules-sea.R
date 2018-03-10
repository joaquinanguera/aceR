
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
                 "stay", "switch", missing = "stay")
  )
  gen = proc_generic_module(df, COL_CORRECT_BUTTON, COL_CONDITION)
  cost = multi_subtract(gen, "\\.stay", "\\.switch", "\\.cost")
  return (data.frame(gen, cost))
}

#' @keywords internal
#' @name sea_procs

module_reading_fluency <- function(df) {
  return (proc_by_condition(df, COL_RT, COL_CORRECT_BUTTON))
}

#' @keywords internal
#' @name sea_procs

module_reading_comprehension <- function(df) {
  return (proc_by_condition(df, COL_RT, COL_CORRECT_BUTTON))
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
