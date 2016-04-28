

#' @keywords internal

filter_vec <- function(vec = c(), pattern = NULL) {
  match = sapply(vec, function(x) return (grepl(pattern, x)))
  return (vec[which(match)])
}

#' @keywords internal

multi_filter_vec <- function(vec = c(), patterns = c()) {
  out = c()
  for (i in 1:length(patterns)) {
    filtered = filter_vec(vec, patterns[i])
    out = c(out, filtered)
  }
  return (out)
}

#' @keywords internal

filter_out_vec <- function(vec = c(), pattern = NULL) {
  match = sapply(vec, function(x) return (!grepl(pattern, x)))
  return (vec[which(match)])
}

#' @keywords internal

multi_filter_out_vec <- function(vec = c(), patterns = c()) {
  out = vec
  for (i in 1:length(patterns)) {
    out = filter_out_vec(out, patterns[i])
  }
  return (out)
}

#' @keywords internal

all_equal_in_vec <- function (vec = c()) {
  return (length(unique(vec)) == 1)
}