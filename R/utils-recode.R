
#' @keywords internal
#' @importFrom dplyr case_when

get_math_operation <- function(x) {
  return (case_when(grepl("[+]", x) ~ "addition",
                    grepl("[-]", x) ~ "subtraction",
                    grepl("[x]", x) ~ "multiplication",
                    TRUE ~ NA_character_))
}

#' @keywords internal
#' @importFrom dplyr if_else

detect_stay_switch <- function(x) {
  return (if_else(x == dplyr::lag(x),
                  "stay",
                  "switch",
                  missing = "stay"))
}

#' @keywords internal

na_if_true <- function (x, condition) {
  x[condition] <- NA
  return (x)
}

#' @keywords internal

make_lagged_col <- function (col) {
  col = paste0("prev_", dplyr::lag(col))
  return (col)
}

#' @keywords internal

recode_brt_condition_dominance <- function (df) {
  
  if (!all(df[[COL_HANDEDNESS]] %in% c("right", "left"))) {
    warning("Nonstandard handedness levels detected.\n",
            "Handedness levels found in data: ",
            paste(unique(df[[COL_HANDEDNESS]]), collapse = " "),
            "\n",
            "Dominant hand recoding may be unknown for these levels")
  }
  
  df <- df %>%
    mutate(!!COL_CONDITION := case_when(
      grepl("right", !!Q_COL_HANDEDNESS) ~ recode(!!Q_COL_CONDITION,
                                                  right = "dominant.index",
                                                  left = "nondominant.index",
                                                  rightindex = "dominant.index",
                                                  leftindex = "nondominant.index",
                                                  rightthumb = "dominant.thumb",
                                                  leftthumb = "nondominant.thumb"),
      grepl("left", !!Q_COL_HANDEDNESS) ~ recode(!!Q_COL_CONDITION,
                                                 left = "dominant.index",
                                                 right = "nondominant.index",
                                                 leftindex = "dominant.index",
                                                 rightindex = "nondominant.index",
                                                 leftthumb = "dominant.thumb",
                                                 rightthumb = "nondominant.thumb"),
      TRUE ~ !!Q_COL_CONDITION))
  
  return (df)
}
