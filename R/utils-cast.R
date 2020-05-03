
#' @importFrom magrittr %>%
#' @keywords internal

to_numeric <- function(x) {
  if (is.numeric(x)) {
    vals = x
    vals[vals == -99] = NA
    return (vals)
  } else if ("correct" %in% unique(x) | "incorrect" %in% unique(x)) { # if it's an accuracy column
    # being more type-strict than plyr::mapvalues, this WILL return numeric every time
    vals = x %>%
      dplyr::na_if("") %>%
      dplyr::recode(correct = 1,
                    incorrect = 0,
                    no_response = NA_real_,
                    .missing = NA_real_)
    return (vals)
  } else { # if it's an RT column
    vals = suppressWarnings(as.numeric(x))
    vals[vals == -99] = NA
    return (vals)
  }
}

#' @keywords internal

to_na <- function (x) {
  return (NA)
}
