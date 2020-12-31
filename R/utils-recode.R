
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
