
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

ace_max <- function(x) {
  x <- to_numeric(x)
  # max and min throw a warning when all NAs
  # this averts the warning via the if statement
  if (all(is.na(x))) {
    return (NA)
  } else {
    return (max(x, na.rm = T))
  }
}

#' @keywords internal

ace_min <- function(x) {
  x <- to_numeric(x)
  if (all(is.na(x))) {
    return (NA)
  } else {
    return (min(x, na.rm = T))
  }
}
