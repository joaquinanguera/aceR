
#' @keywords internal
#' @name ace_procs

module_brt <- function(df) {
  # standard
  rt = proc_standard(df, COL_RT)
  acc = proc_standard(df, COL_ACC)
  rw = proc_standard(df, COL_RW)
  # specific
  rt_acc = proc_standard_factor(df, COL_RT, COL_ACC, ace_descriptive_statistics_by_group)
  turns = proc_standard_factor(df, COL_RT, COL_ACC, ace_average_turns)
  # merge
  analy = list(rt, acc, rw, rt_acc, turns)
  merged = multi_merge(analy, by = COL_PID)
  return (merged)
}