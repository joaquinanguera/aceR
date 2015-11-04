
#' @keywords internal

ace_num <- function(x) {
  return (length(x))
}

#' @keywords internal

ace_count <- function(x) {
  y = x[x != ""] 
  return (na.omit(length(y)))
}

#' @keywords internal

ace_summary <- function (x) {
  return (c(num = ace_num(x), count = ace_count(x)))
}