
#' @keywords internal

ace_span <- function(x) {
  max = 0
  vals = to_numeric(x)
  num = length(vals) - 1
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