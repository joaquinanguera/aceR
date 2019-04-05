
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @keywords internal

filter_vec <- function(vec = c(), pattern = NULL) {
  # keeps those that contain ANY OF pattern(s); allows pattern to be a character vector
  match <- map(pattern, ~grepl(., vec)) %>%
    as.data.frame() %>%
    rowSums() %>%
    as.logical() # appears to coerce any numeric not equal to 0 to TRUE, which is desired, so okay?
  return (vec[match])
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

#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @keywords internal

filter_out_vec <- function(vec = c(), pattern = NULL) {
  # keeps those that don't contain ANY OF pattern(s)
  match <- map(pattern, ~grepl(., vec)) %>%
    as.data.frame() %>%
    rowSums() %>%
    as.logical()
  return (vec[!match])
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