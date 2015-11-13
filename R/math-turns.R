
#' Average of the last 3 "turn around trials" - a correct trial that was preceded by an incorrect trial
#'
#' @keywords internal

ace_turns <- function(x, y) {
  indices = identify_turns(y)
  last_3 = tail(indices, 3)
  vals = x[last_3]
  return (ace_mean(vals))
}

#' Identify turn trials
#'
#' Given an list of "1" & "0"'s', identify indices where a "1" is preceded by a "0".
#'
#' @keywords internal
#' @param y a list of "1" & "0"s
#' @return Returns the indices of all turn trials.

identify_turns <- function(y) {
  turns = c()
  for (i in 2:length(y)) {
    n = y[i] # current trial
    m = y[i - 1] # previous trial
    if (n == "1" & m == "0") {
      turns = c(turns, i)
    }
  }
  return (turns)
}