
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

set_names <- function (x, these_names) {
  stopifnot(length(x) == length(these_names))
  names(x) <- these_names
  return (x)
}