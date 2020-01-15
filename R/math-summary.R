
#' @keywords internal

ace_length <- function(x) {
  return (length(x))
}

#' @keywords internal

ace_count <- function(x) {
  if (is.character(x)) {
    y = x[!(is.na(x) | x %in% c("", "no_response"))]
  } else if (is.numeric(x)) {
    y = x[!is.na(x)]
  }
  return (na.omit(length(y)))
}

#' @keywords internal

ace_count_by_group <- function(x, y) {
  return (ace_apply_by_group(x, y, ace_count))
}

#' @keywords internal

ace_length_by_group <- function(x, y) {
  return (ace_apply_by_group(x, y, ace_length))
}

#' @keywords internal

ace_max <- function(x) {
  return (max(to_numeric(x), na.rm = T))
}

#' @keywords internal

ace_min <- function(x) {
  return (min(to_numeric(x), na.rm = T))
}
