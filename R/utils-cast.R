
#' @keywords internal

to_numeric <- function(x) {
  if (is.numeric(x)) {
    return (x)
  } else {
    # being more type-strict than plyr::mapvalues, this WILL return numeric every time
    vals = dplyr::recode(x,
                         correct = 1L,
                         incorrect = 0L,
                         no_response = NA_integer_,
                         .default = NA_integer_,
                         .missing = NA_integer_)
    return (vals)
  }
}