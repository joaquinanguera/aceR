
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  rt = apply_stats(df, PARTICIPANT_BY_GROUP, "response_time", ace_descriptive_statistics)
  acc = apply_stats(df, PARTICIPANT_BY_GROUP, "correct_button", ace_descriptive_statistics)
  return (apply_stats_transform(list(rt, acc), "group", "participant_id"))
}