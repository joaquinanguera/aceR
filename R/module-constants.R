
#' @keywords internal

proc_generic_module_old <- function(df, col_acc, col_condition, by_factor = TRUE) {
  # overall & broken-down by condition
  rt = proc_by_condition(df, COL_RT, col_condition, FUN = ace_descriptive_statistics)
  rw = proc_by_condition(df, COL_RW, col_condition, FUN = ace_descriptive_statistics)
  acc = proc_by_condition(df, col_acc, col_condition, FUN = ace_descriptive_statistics)
  # RT broken-down by condition & factor  
  rt_acc = proc_standard(df, COL_RT, col_condition, factor = col_acc, FUN = ace_descriptive_statistics_by_group)
  # RW & RT by block half
  rt_block_half = proc_standard(df, COL_RT, NULL, factor = COL_BLOCK_HALF, FUN = ace_descriptive_statistics_by_group)
  rw_block_half = proc_standard(df, COL_RW, NULL, factor = COL_BLOCK_HALF, FUN = ace_descriptive_statistics_by_group)
  # turns calculations
  if (by_factor) {
    # turns calculation by condition
    turns = proc_standard(df, COL_RW, col_condition, factor = col_acc, FUN = ace_average_turns)
    # detection rates by condition
    detection = proc_standard(df, COL_RW, col_condition, factor = COL_RT, FUN = ace_detection_rate)
  } else {
    # overall turns calculation
    turns = proc_standard(df, COL_RW, col_condition = NULL, factor = col_acc, FUN = ace_average_turns, y = c(COL_BID))
    # overall detection rates
    detection = proc_standard(df, COL_RW, col_condition = NULL, factor = COL_RT, FUN = ace_detection_rate, y = c(COL_BID))
  }
  # merge
  analy = list(rt, acc, rw, rt_acc, rt_block_half, rw_block_half, turns, detection)
  merged = multi_merge(analy, by = COL_BID)
  return (merged)
}


#' @keywords internal

proc_generic_module <- function(df, col_acc, col_condition) {
  # df: the data
  # col_acc: name of col containing accuracy
  # col_condition: name of col containing condition/trial type
  
  # overall & broken-down by condition
  # RT broken-down by condition & accuracy  
  rt_acc = proc_by_condition(df, COL_RT, factors = c(col_condition, col_acc), FUN = ace_descriptive_statistics_dplyr)
  # accuracy broken down by condition and response window (early or late?)
  # if late response is not available for the task, don't factor by it
  if (COL_LATE_RESPONSE %in% names(df)) {
    acc = proc_by_condition(df, col_acc, factors = c(col_condition, COL_LATE_RESPONSE), FUN = ace_descriptive_statistics_dplyr)
  } else {
    acc = proc_by_condition(df, col_acc, factors = c(col_condition), FUN = ace_descriptive_statistics_dplyr)
  }
  # RW just broken down by condition
  rw = proc_by_condition(df, COL_RW, factors = col_condition, FUN = ace_descriptive_statistics_dplyr)
  # RW & RT by block half
  rt_block_half = proc_by_condition(df, COL_RT, factors = COL_BLOCK_HALF, include_overall = F, FUN = ace_descriptive_statistics_dplyr)
  rw_block_half = proc_by_condition(df, COL_RW, factors = COL_BLOCK_HALF, include_overall = F, FUN = ace_descriptive_statistics_dplyr)
  # merge
  analy = list(rt_acc, acc, rw, rt_block_half, rw_block_half)
  merged = multi_merge(analy, by = COL_BID)
  return (merged)
}

#' @keywords internal

proc_by_condition_old <- function(df, variable, col_condition, FUN) {
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

proc_by_condition <- function(df, variable, factors, include_overall = TRUE, FUN = ace_descriptive_statistics_dplyr, transform_dir = "wide") {
  # conditions to be "expected" (though this expectation is dangerous, don't hardcode):
  # subtask type ("col_condition")
  # late response (to subset accuracy by late resp)
  # accuracy (to subset RT by acc)
  # conditions to be subsetted should be fed in as a CHAR VECTOR
  overall = apply_stats_dplyr(
    x = df, 
    id_var = COL_BID,
    col = variable, 
    FUN = FUN, 
    suffix = "overall",
    transform_dir = transform_dir)
  # needs to be flexible to handle 0-n number of subsetting conditions, and also NOT to cross them all with each other necessarily
  by_condition = apply_stats_dplyr(
    x = df, 
    id_var = COL_BID,
    col = variable,
    factors = factors,
    FUN = FUN,
    transform_dir = transform_dir)
  
  if (include_overall & exists("by_condition")) {
    proc = left_join(overall, by_condition, by = c("bid" = "bid"))
  } else if (include_overall) {
    proc = overall
  } else {
    proc = by_condition
  }
  names(proc) = tolower(names(proc))
  return(proc)
}

#' @keywords internal

proc_standard <- function (df, variable, col_condition = NULL, col_condition2 = NULL, y = c(COL_BID, col_condition), FUN, transform_dir = "wide", ...) {
  proc = apply_stats(
    x = df, 
    y = y, 
    col = variable, 
    FUN = FUN, 
    ...)
  if (is.null(col_condition)) {
    return (proc)
  }
  if (transform_dir == "wide") {
    transform = stats::reshape(proc, timevar = col_condition, idvar = COL_BID, direction = "wide")
  } else {transform = proc}
  names(transform) = tolower(names(transform))
  return (transform)
}