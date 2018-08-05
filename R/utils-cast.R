
#' @keywords internal

to_numeric <- function(x) {
  if (is.numeric(x)) {
    return (x)
  } else {
    # being more type-strict than plyr::mapvalues, this WILL return numeric every time
    vals = dplyr::recode(x,
                         correct = 1,
                         incorrect = 0,
                         no_response = NA_real_,
                         .default = as.numeric(x),
                         .missing = NA_real_)
    return (vals)
  }
}