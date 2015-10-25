
#' Remove empty columns from a data frame.
#'
#' @export
#' @param dat a data frame
#' @return Returns a data frame where the empty columns have been removed

remove_empty_cols <- function(dat) {
  return(dat[!sapply(dat, function(x) all(x == ""))])
}

#' Remove empty rows from a data frame.
#'
#' @export
#' @param dat a data frame
#' @return Returns a data frame where the empty rows have been removed

remove_empty_rows <- function(dat) {
  return(dat[rowSums(is.na(dat)) != ncol(dat),])
}

#' Remove specified rows from a data frame
#'
#' @export
#' @param dat a data frame
#' @param rows a vector containing the names of the the rows to remove
#' @param reset reset the row numbers?
#' @return Returns a data frame where the specified rows have been removed

remove_rows <- function(dat, rows = c(), reset = TRUE) {
  if (length(rows) == 0) {
    return (dat)
  }
  dat = dat[-rows, ] 
  if (reset) {
    row.names(dat) = NULL
  }
  return (dat)
}

#' Numeric row names
#'
#' @export
#' @param dat a data frame
#' @return Returns numeric row names

numeric_row_names <- function(dat) {
  rows = row.names(dat)
  return (as.numeric(as.character(rows)))
}