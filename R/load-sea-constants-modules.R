
#' SEA module constants
#'
#' @keywords internal
#' @name sea_module
NULL

#' @name sea_module
MATH_FLU <- "MATH_FLUENCY"

#' @name sea_module
READ_FLU <- "READING_FLUENCY"

#' @name sea_module
FRAC_1 <- "FRACTIONS_LVL_1"

#' @name sea_module
FRAC_2 <- "FRACTIONS_LVL_2"

#' @name sea_module
FRAC_3 <- "FRACTIONS_LVL_3"

#' @name sea_module
MATH_REC <- "MATH_RECALL"

#' @name sea_module
RUN_MEM_SPAN <- "RUNNING_MEMORY_SPAN"

#' @name sea_module
GROUPITIZE <- "GROUPITIZING"

#' @name sea_module
REL_MATCH <- "RELATIONAL_MATCHING"

#' @name sea_module
ARITHM_VER <- "ARITHMETIC_VERIFICATION"

#' @name sea_module
READ_COMP <- "READING_COMPREHENSION"

#' @name sea_module
ALL_SEA_MODULES = c(MATH_FLU,
                    READ_FLU,
                    FRAC_1,
                    FRAC_2,
                    FRAC_3,
                    MATH_REC,
                    RUN_MEM_SPAN,
                    GROUPITIZE,
                    REL_MATCH,
                    ARITHM_VER,
                    READ_COMP)

#' @keywords internal
#' @importFrom dplyr mutate recode
#' @importFrom stringr str_sub

standardize_sea_module_names <- function (df) {
  df = mutate(df,
              module = toupper(replace_spaces(.data$module, replacement = "_")),
              condition = if_else(grepl("RELATIONAL_MATCHING", module),
                                  paste0("block_", tolower(str_sub(module, start = -3L))),
                                  condition),
              module = recode(module,
                              RELATIONAL_MATCHING_ONE = "RELATIONAL_MATCHING",
                              RELATIONAL_MATCHING_TWO = "RELATIONAL_MATCHING"))
  return (df)
}

#' @keywords internal
#' @importFrom dplyr recode

label_sea_module_conditions <- function (dat) {
  dat[[COL_CONDITION]] <- dplyr::recode(dat[[COL_MODULE]],
                                        "Fractions Lvl 1" = "lvl_1",
                                        "Fractions Lvl 2" = "lvl_2",
                                        "Fractions Lvl 3" = "lvl_3",
                                        .default = "")
  dat[[COL_MODULE]] <- ifelse(startsWith(dat[[COL_MODULE]], "Reading Fluency"), "Reading Fluency", dat[[COL_MODULE]])
  return (dat)
}

#' @keywords internal
#' @importFrom dplyr case_when if_else left_join mutate recode select %>%
#' @importFrom purrr map2_dbl
#' @importFrom stringr str_match_all str_split

append_info <- function (dat, module) {
  if (module == MATH_FLU) {
    out <- dat %>%
      mutate(operation_type = get_math_operation(.data[[COL_QUESTION_TEXT]]),
             condition = detect_stay_switch(operation_type),
             digit_load = case_when(nchar(.data[[COL_QUESTION_TEXT]]) == 5 ~ "low",
                                    nchar(.data[[COL_QUESTION_TEXT]]) == 6 ~ "med",
                                    nchar(.data[[COL_QUESTION_TEXT]]) == 7 ~ "high",
                                    TRUE ~ NA_character_),
             answer_size = nchar(.data[[COL_CORRECT_RESPONSE]]))
  } else if (module == FRAC_1) {
    out <- dat %>%
      mutate(num_left = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 1L, end = 1L)),
             num_right = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = -1L, end = -1L)),
             condition = recode(correct_response, `Right side` = "right", `Left side` = "left"),
             num_size = if_else(pmax(num_left, num_right) <= 5,
                                "small",
                                "large"))
  } else if (module == FRAC_2) {
    out <- dat %>%
      mutate(num_left = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 9L, end = 9L)),
             denom_left = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 11L, end = 11L)),
             num_right = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 14L, end = 14L)),
             denom_right = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 16L, end = 16L)),
             condition = recode(correct_response, `Right side` = "right", `Left side` = "left"),
             matched_value = case_when(num_left == num_right ~ "num_matched",
                                       denom_left == denom_right ~ "denom_matched",
                                       TRUE ~ NA_character_),
             num_distance = abs(num_left / denom_left - num_right / denom_right))
  } else if (module == FRAC_3) {
    out <- dat %>%
      mutate(num_left = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 9L, end = 9L)),
             denom_left = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 11L, end = 11L)),
             frac_left = num_left / denom_left,
             num_right = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 14L, end = 14L)),
             denom_right = as.numeric(str_sub(.data[[COL_QUESTION_TEXT]], start = 16L, end = 16L)),
             frac_right = num_right / denom_right,
             condition = recode(correct_response, `Right side` = "right", `Left side` = "left"),
             congruency = case_when(condition == "right" & num_right > num_left ~ "congruent",
                                    condition == "left" & num_left > num_right ~ "congruent",
                                    TRUE ~ "incongruent"),
             num_distance = abs(frac_left - frac_right))
  } else if (module == MATH_REC) {
    out <- dat %>%
      left_join(append_cols_math_recall %>%
                  select(question_text, carrying),
                by = "question_text") %>%
      mutate(operation_type = get_math_operation(.data[[COL_QUESTION_TEXT]]),
             digit_load = case_when(nchar(.data[[COL_QUESTION_TEXT]]) == 5 ~ "low",
                                    nchar(.data[[COL_QUESTION_TEXT]]) == 6 ~ "med",
                                    nchar(.data[[COL_QUESTION_TEXT]]) == 7 ~ "high",
                                    TRUE ~ NA_character_),
             answer_size = nchar(.data[[COL_CORRECT_RESPONSE]]))
  } else if (module == RUN_MEM_SPAN) {
    # strict trialwise accuracy: count number of positions where the letters match
    # loose trialwise accuracy: count number of letters in correct answer that appear in user answer
    out <- dat %>%
      mutate(block_type = if_else(grepl("Image", question_text),
                                 "spatial",
                                 "letter"),
             list_length = lengths(str_match_all(question_text, " ")) + 1,
             response_split = str_split(response, " "),
             correct_response_split = str_split(correct_response, " "),
             correct_button_strict = map2_dbl(correct_response_split, response_split, ~sum(.x == .y) / length(.x)),
             correct_button_loose = map2_dbl(correct_response_split, response_split, ~sum(.x %in% .y) / length(.x))) %>%
      select(-response_split, -correct_response_split)
  } else if (module == GROUPITIZE) {
    out <- dat %>%
      mutate(arrangement = case_when(as.numeric(correct_response) <= 3 ~ "subitizing",
                                     grepl("random", question_text) ~ "random",
                                     TRUE ~ "group"),
             number_groups = if_else(arrangement == "group",
                                     lengths(str_match_all(question_text, "-")) + 1,
                                     1))
  } else if (module == ARITHM_VER) {
    out <- dat %>%
      left_join(append_cols_arithmetic_verification %>%
                  select(question_id, block_type),
                by = "question_id") %>%
      left_join(append_cols_arithmetic_verification %>%
                  select(question_text, false_type),
                by = "question_text") %>%
      mutate(operation_type = get_math_operation(.data[[COL_QUESTION_TEXT]]),
             condition = if_else(block_type == "Mixed",
                                 detect_stay_switch(operation_type),
                                 NA_character_),
             switch_by_operation_type = paste0(condition, "_", dplyr::lag(operation_type)),
             previous_correct_button = dplyr::lag(correct_button))
  } else {
    out <- dat
  }
  return (out)
}
