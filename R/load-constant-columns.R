
#' ACE column name constants
#'
#' @keywords internal
#' @name ace_header
NULL

#' @name ace_header
COL_RT = "rt"

#' @name ace_header
COL_ACC = "acc"

#' @name ace_header

standardize_ace_column_names <- function(df) {
  new = names(df)
  new[new == "response_time"] = COL_RT
  new[new == "correct_response"] = COL_ACC
  names(df) = new
  return (df)
}