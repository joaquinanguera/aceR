
#' Returns the highest value that occurs at least 2x consecutively
#' @keywords internal

ace_span <- function(x) {
  if (all(is.na(x))) return (NA)
  max = 0
  vals = to_numeric(x)
  num = length(vals) - 1
  if (num == 0) {
    return (NA)
  }
  for (i in 1:num) {
    n = vals[i]
    m = vals[i+1]
    if (n == m) {
      if (n > max) {
        max = n
      }
    }
  }
  return (max)
}
