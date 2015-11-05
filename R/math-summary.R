
#' @keywords internal

ace_num <- function(x) {
  return (length(x))
}

#' @keywords internal

ace_count <- function(x) {
  y = x[x != ""] 
  return (na.omit(length(y)))
}