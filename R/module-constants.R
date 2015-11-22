
#' @keywords internal

proc_generic_module <- function(df, col_acc, col_condition, turns_by_factor = TRUE) {
  # overall & broken-down by condition
  rt = proc_by_condition(df, COL_RT, col_condition, FUN = ace_descriptive_statistics)
  rw = proc_by_condition(df, COL_RW, col_condition, FUN = ace_descriptive_statistics)
  acc = proc_by_condition(df, col_acc, col_condition, FUN = ace_descriptive_statistics)
  # RT broken-down by condition & factor  
  rt_acc = proc_standard(df, COL_RT, col_condition, factor = col_acc, FUN = ace_descriptive_statistics_by_group)
  # turns calculations
  if (turns_by_factor) {
    # turns calculation by condition
    turns = proc_standard(df, COL_RT, col_condition, factor = col_acc, FUN = ace_average_turns)
  } else {
    # overall turns calculation
    turns = proc_standard(df, COL_RT, col_condition = NULL, factor = col_acc, FUN = ace_average_turns, y = c(COL_BID))
  }
  # merge
  analy = list(rt, acc, rw, rt_acc, turns)
  merged = multi_merge(analy, by = COL_BID)
  return (merged)
}


#' @keywords internal

proc_by_condition <- function(df, variable, col_condition, FUN) {
  overall = apply_stats(
    x = df, 
    y = c(COL_BID), 
    col = variable, 
    FUN = FUN, 
    suffix = "overall")
  by_condition = apply_stats(
    x = df, 
    y = c(COL_BID, col_condition), 
    col = variable, 
    FUN = FUN)
  by_condition_transform = stats::reshape(by_condition, timevar = col_condition, idvar = COL_BID, direction = "wide") 
  proc = multi_merge(list(overall, by_condition_transform), by = COL_BID)
  names(proc) = tolower(names(proc))
  return(proc)
}

#' @keywords internal

proc_standard <- function (df, variable, col_condition = NULL, y = c(COL_BID, col_condition), FUN, ...) {
  proc = apply_stats(
    x = df, 
    y = y, 
    col = variable, 
    FUN = FUN, 
    ...)
  if (is.null(col_condition)) {
    return (proc)
  }
  transform = stats::reshape(proc, timevar = col_condition, idvar = COL_BID, direction = "wide")
  names(transform) = tolower(names(transform))
  return (transform)
}