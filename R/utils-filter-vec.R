

#' @keywords internal

filter_vec <- function(vec = c(), pattern = NULL) {
  match = sapply(vec, function(x) return (grepl(pattern, x)))
  return (vec[which(match)])
}

#' @keywords internal

filter_out_vec <- function(vec = c(), pattern = NULL) {
  match = sapply(vec, function(x) return (!grepl(pattern, x)))
  return (vec[which(match)])
}