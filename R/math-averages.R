
#' @keywords internal

ace_mean <- function(x) {
  return (mean(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_median <- function(x) {
  return (median(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_mode <- function(x) {
  return (which.max(table(x))[[1]])
}