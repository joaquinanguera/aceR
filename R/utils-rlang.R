
#' Currently only produces a set of relational statements
#' such as those that might be !!!-spliced into a filter call.
#' @importFrom rlang call2 expr sym !!
#' @importFrom purrr map
#' @keywords internal

map_call2_rel <- function (.fn, a, b, .ns = NULL) {
  stopifnot(length(b) == 1)
  out <- map(a, ~call2(.fn, expr(!!sym(.x)), b))
  return (out)
}

#' @importFrom rlang enexprs !!!
#' @keywords internal

filter_qq <- function (.data, ...) {
  dots <- enexprs(...)
  return (dplyr::filter(.data, !!!dots))
}
  