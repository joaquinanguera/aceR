
#' Average of the last n "turn around trials" - a correct trial that was preceded by an incorrect trial
#' Do NOT give output if any of the vector of trial outcomes is missing
#'
#' @keywords internal

ace_turns <- function(x, y, n = 3) {
  if (length(x) == 1) return (NA)
  if (any(x == "")) return (NA)
  if (any(y == "")) return (NA)
  indices = identify_turns(to_numeric(y))
  last_n = tail(indices, n)
  vals = x[last_n]
  return (ace_mean(vals))
}

#' Identify turn trials
#'
#' Given an list of 1 & 0's', identify indices where a 1 is preceded by a 0.
#' Patches 0s in place of NAs.
#'
#' @keywords internal
#' @param y a list of 1 & 0s
#' @return Returns the indices of all turn trials.

identify_turns <- function(y) {
  y = dplyr::coalesce(y, 0)
  turns = c()
  for (i in 2:length(y)) {
    n = y[i] # current trial
    m = y[i - 1] # previous trial
    if (n == 1 & m == 0) {
      turns = c(turns, i)
    }
  }
  return (turns)
}
