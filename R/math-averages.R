
#' @keywords internal

ace_mean <- function(x) {
  return (mean(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_median <- function(x) {
  return (median(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_mean_by_group <- function(x, y) {
  return (ace_apply_by_group(x, y, ace_mean))
}

#' @keywords internal

ace_median_by_group <- function(x, y) {
  return (ace_apply_by_group(x, y, ace_median))
}