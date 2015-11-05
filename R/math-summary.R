
#' @keywords internal

ace_length <- function(x) {
  return (length(x))
}

#' @keywords internal

ace_count <- function(x) {
  y = x[x != ""] 
  return (na.omit(length(y)))
}