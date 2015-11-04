
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

ace_variance <- function (x) {
  c(sd = ace_sd(x), se = ace_se(x))
}