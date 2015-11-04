
#' @export

module_boxed <- function(df) {
  rt_avg = apply_stats(df, c("participant_id", "group"), "response_time", ace_averages)
  rt_var = apply_stats(df, c("participant_id", "group"), "response_time", ace_variance)
  rt_sum = apply_stats(df, c("participant_id", "group"), "response_time", ace_summary)
  acc_avg = apply_stats(df, c("participant_id", "group"), "correct_button", ace_averages)
  acc_var = apply_stats(df, c("participant_id", "group"), "correct_button", ace_variance)
  acc_sum = apply_stats(df, c("participant_id", "group"), "correct_button", ace_summary)
  proc = list(rt_avg, rt_var, rt_sum, acc_avg, acc_var, acc_sum)
  return (reshape::merge_recurse(proc))
}