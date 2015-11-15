
#' ACE module constants
#'
#' @keywords internal
#' @name ace_module_constants
NULL

#' @name ace_module_constants
PARTICIPANT_BY_PARTICIPANT <- c(COL_PID)

#' @keywords internal

proc_generic_module <- function(df, col_acc, col_condition) {
  # standard
  rt = proc_standard(df, COL_RT, col_condition)
  rw = proc_standard(df, COL_RW, col_condition)
  acc = proc_standard(df, col_acc, col_condition)
  # specific
  rt_acc = proc_standard_factor(df, COL_RT, col_condition, col_acc, ace_descriptive_statistics_by_group)
  turns = proc_standard_factor(df, COL_RT, col_condition, col_acc, ace_average_turns)
  # merge
  analy = list(rt, acc, rw, rt_acc, turns)
  merged = multi_merge(analy, by = COL_PID)
  return (merged)
}

#' @keywords internal

proc_standard <- function(df, variable, col_condition) {
  overall = apply_stats(x = df, y = c(COL_PID), col = variable, FUN = ace_descriptive_statistics, suffix = "overall")
  by_condition = apply_stats(x = df, y = c(COL_PID, col_condition), col = variable, FUN = ace_descriptive_statistics)
  by_condition_transform = stats::reshape(by_condition, timevar = col_condition, idvar = COL_PID, direction = "wide") 
  proc = multi_merge(list(overall, by_condition_transform), by = COL_PID)
  return(proc)
}

#' @keywords internal

proc_standard_factor <- function (df, variable, condition, factor, FUN) {
  proc = apply_stats(x = df, y = c(COL_PID, condition), col = variable, factor = factor, FUN = FUN)
  transform = stats::reshape(proc, timevar = condition, idvar = COL_PID, direction = "wide")
  return (transform)
}