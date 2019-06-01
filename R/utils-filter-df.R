
#' Remove empty columns from a data frame.
#'
#' @importFrom dplyr select_if funs
#' @importFrom magrittr %>%
#' @keywords internal
#' @param df a data frame
#' @return Returns a data frame where the empty columns have been removed

remove_empty_cols <- function(df) {
  # now removes all columns of blanks AND all columns of true NA
  out <- df %>%
    select_if(funs(!all(is.na(.)))) %>%
    select_if(funs(!all(as.character(na.omit(.)) == "")))
  
  return(out)
}

#' Remove empty rows from a data frame.
#'
#' @keywords internal
#' @inheritParams remove_empty_cols
#' @return Returns a data frame where the empty rows have been removed

remove_empty_rows <- function(df) {
  return (df[!apply(is.na(df) | df == "", 1, all),])
}

#' Remove specified rows from a data frame
#'
#' @keywords internal
#' @inheritParams remove_empty_cols
#' @param rows a vector containing the names of the the rows to remove
#' @param reset reset the row numbers?
#' @return Returns a data frame where the specified rows have been removed

remove_rows <- function(df, rows = c(), reset = TRUE) {
  if (length(rows) == 0) {
    return (df)
  }
  df = df[-rows, ] 
  if (reset) {
    row.names(df) = NULL
  }
  return (df)
}

#' Numeric row names
#'
#' @keywords internal
#' @inheritParams remove_empty_cols
#' @return Returns numeric row names

numeric_row_names <- function(df) {
  rows = row.names(df)
  return (as.numeric(as.character(rows)))
}