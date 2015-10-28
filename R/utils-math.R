
#' @keywords internal

# TODO: export
consecutive_sums <- function(vec) {
  indices = seq(1:length(vec))
  sums = sapply(indices, function(x) {
    return (sum(vec[1:x]))
  })
  return (sums)
}