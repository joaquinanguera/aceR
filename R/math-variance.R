
#' @keywords internal

ace_sd <- function(x) {
  return (sd(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_se <- function(x) {
  vals = na.omit(to_numeric(x))
  return (sqrt(var(vals)/length(vals)))
} 
