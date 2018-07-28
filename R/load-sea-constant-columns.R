
#' SEA column name constants
#' 
#' SEA also uses some ACE column name constants.
#' These are constants which are SEA-specific.
#'
#' @keywords internal
#' @name sea_header
NULL

#' @name sea_header
COL_RESPONSE = "response"

#' @name SEA_header
Q_COL_RESPONSE <- rlang::sym(COL_RESPONSE)

#' @name sea_header
COL_QUESTION_TEXT = "question_text"

#' @name SEA_header
Q_COL_QUESTION_TEXT <- rlang::sym(COL_QUESTION_TEXT)

#' @name sea_header
COL_MODULE = "module"

#' @name SEA_header
Q_COL_MODULE <- rlang::sym(COL_MODULE)

#' @name sea_header
#' @importFrom dplyr recode

standardize_sea_column_names <- function(df) {
  new = dplyr::recode(names(df),
                      response_time = COL_RT,
                      user_answer = COL_RESPONSE,
                      correct_answer = COL_CORRECT_RESPONSE,
                      question_type = COL_MODULE,
                      user_id = COL_PID
                      )
  names(df) = new
  return (df)
}