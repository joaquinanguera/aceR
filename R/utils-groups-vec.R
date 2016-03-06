
#' @keywords internal

group_consecutive_integers <- function(x) {
  breaks = c(0, which(diff(x) != 1), length(x)) 
  consec = sapply(seq(length(breaks) - 1), function(i) x[(breaks[i] + 1):breaks[i+1]])
  return (consec)
}