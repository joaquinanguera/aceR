
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  # standard
  rt = proc_standard(df, COL_RT)
  acc = proc_standard(df, COL_ACC)
  rw = proc_standard(df, COL_RW)
  # specific
  rt_acc = proc_standard_factor(df, COL_RT, COL_ACC, ace_descriptive_statistics_by_group)
  turns = proc_standard_factor(df, COL_RT, COL_ACC, ace_average_turns)
  # merge
  proc = multi_merge(list(rt, acc, rw, rt_acc, turns), by = COL_PID)
  return (proc)
}