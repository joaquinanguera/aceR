
#' @export

module_boxed <- function(df) {
  rt = apply_stats(df, c("participant_id", "group"), "response_time", ace_descriptive_statistics)
  acc = apply_stats(df, c("participant_id", "group"), "correct_button", ace_descriptive_statistics)
  proc = list(rt, acc)
  return (reshape::merge_recurse(proc))
}