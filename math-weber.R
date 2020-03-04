
#' @keywords internal

ace_weber_opt <- function(n1, n2, err) {
  ns <- cbind(n1, n2)
  ws <- lapply(seq(0, 1, .01), weber, n1 = n1, n2 = n2)
  diffs = ws - err
  return (min(ws))
}

#' @keywords internal

weber <- function(w, n1, n2) {
  return (0.5 * erfc(n1 - n2) / (sqrt(2) * w * sqrt(n1^2 + n2^2)))
}

#' @keywords internal

erfc <- function(x) {
  return(2 * pnorm(x * sqrt(2), lower = FALSE))
}