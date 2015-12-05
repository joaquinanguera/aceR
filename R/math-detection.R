

#' @keywords internal

identify_correct_rejections <- function(response_window, reaction_time) {
  rws = to_numeric(response_window)
  rts = to_numeric(reaction_time)
  out = c()
  for (i in 1:(length(rts) - 1)) {
    if (is.na(rts[i])) {
      rej = ifelse(rws[i + 1] == rws[i], "correct_rejection", "false_alarm")
    } else {
      rej = ifelse(rws[i + 1] < rws[i], "hit", "miss")
    }
    out = c(out, rej)
  }
  out = c(out, NA) # last trial
  return (out)
}