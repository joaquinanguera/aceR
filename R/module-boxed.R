
#' @keywords internal

module_boxed <- function(df) {
  rt = apply_stats(df, c("participant_id", "group"), "response_time", ace_descriptive_statistics)
  acc = apply_stats(df, c("participant_id", "group"), "correct_button", ace_descriptive_statistics)
  return (apply_stats_transform(list(rt, acc), "group", "participant_id"))
}