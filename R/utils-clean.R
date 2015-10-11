
#' Remove empty columns from a data frame.
#'
#' @export
#' @param dat a data frame
#' @return Returns a data frame where the empty columns 
#'  have been removed

remove_empty_cols <- function(dat) {
  return(dat[!sapply(dat, function(x) all(x == ""))])
}