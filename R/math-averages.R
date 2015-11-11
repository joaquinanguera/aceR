
#' @keywords internal

ace_mean <- function(x) {
  return (mean(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_median <- function(x) {
  return (median(to_numeric(x), na.rm = TRUE))
}

#' @keywords internal

ace_mode <- function(x) {
  return (which.max(table(x))[[1]])
}

#' @keywords internal
ace_mean_by_group <- function(x, y) {
  agg = aggregate(list(x), list(y), FUN = ace_mean, simplify = TRUE)
  out = data.frame(t(agg[2]))
  names(out) = unlist(agg[1])
  row.names(out) <- NULL
  return (out)
}