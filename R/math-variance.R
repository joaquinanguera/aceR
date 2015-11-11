
#' @keywords internal

ace_sd <- function(x) {
  return (sd(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_se <- function(x) {
  vals = na.omit(to_numeric(x))
  return (sqrt(var(vals)/length(vals)))
} 

#' @keywords internal

ace_sd_by_group <- function(x, y) {
  return (ace_apply_by_group(x, y, ace_sd))
}

#' @keywords internal

ace_se_by_group <- function(x, y) {
  return (ace_apply_by_group(x, y, ace_se))
}