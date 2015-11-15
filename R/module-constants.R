
#' ACE module constants
#'
#' @keywords internal
#' @name ace_module_constants
NULL

#' @name ace_module_constants
PARTICIPANT_BY_PARTICIPANT <- c(COL_PID)

#' @name ace_module_constants
PARTICIPANT_BY_CONDITION <- c(COL_PID, COL_CONDITION)

#' @keywords internal

proc_generic_module <- function(df) {
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

#' @keywords internal

proc_standard <- function(df, variable) {
  overall = apply_stats(x = df, y = PARTICIPANT_BY_PARTICIPANT, col = variable, FUN = ace_descriptive_statistics, suffix = "overall")
  by_condition = apply_stats(x = df, y = PARTICIPANT_BY_CONDITION, col = variable, FUN = ace_descriptive_statistics)
  by_condition_transform = stats::reshape(by_condition, timevar = COL_CONDITION, idvar = COL_PID, direction = "wide") 
  proc = multi_merge(list(overall, by_condition_transform), by = COL_PID)
  return(proc)
}

#' @keywords internal

proc_standard_factor <- function (df, variable, factor, FUN) {
  proc = apply_stats(x = df, y = PARTICIPANT_BY_CONDITION, col = variable, factor = factor, FUN = FUN)
  transform = stats::reshape(proc, timevar = COL_CONDITION, idvar = COL_PID, direction = "wide")
  return (transform)
}