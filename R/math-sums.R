
#' @keywords internal

consecutive_sums <- function(vec) {
  indices = seq(1:length(vec))
  sums = sapply(indices, function(x) {
    return (sum(vec[1:x]))
  })
  return (sums)
}

#' @keywords internal

ace_sum <- function(x) {
  return (sum(to_numeric(x), na.rm = TRUE))
}
