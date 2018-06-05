
#' @keywords internal
#' @param df data for one module
#' @param col_acc column for accuracy, as string
#' @param col_condition column containing "condition" or trial type, as string

proc_generic_module <- function(df, col_acc, col_condition, FUN = ace_descriptive_statistics_dplyr) {
  
  # overall & broken-down by condition
  # RT broken-down by condition & accuracy  
  rt_acc = proc_by_condition(df, COL_RT, factors = c(col_condition, col_acc), FUN = ace_descriptive_statistics_dplyr)
  # RT by block half
  rt_block_half = proc_by_condition(df, COL_RT, factors = COL_BLOCK_HALF, include_overall = F, FUN = ace_descriptive_statistics_dplyr)
  
  # accuracy broken down by condition and response window (early or late?)
  # if late response is not available for the task, don't factor by it
  if (COL_LATE_RESPONSE %in% names(df)) {
    acc = proc_by_condition(df, col_acc, factors = c(col_condition, COL_LATE_RESPONSE), FUN = ace_descriptive_statistics_dplyr)
  } else {
    acc = proc_by_condition(df, col_acc, factors = c(col_condition), FUN = ace_descriptive_statistics_dplyr)
  }
  # RW just broken down by condition
  # and by block half
  # if RW not available (e.g. SEA data), don't use it
  if (COL_RW %in% names(df)) {
    rw = proc_by_condition(df, COL_RW, factors = col_condition, FUN = ace_descriptive_statistics_dplyr)
    rw_block_half = proc_by_condition(df, COL_RW, factors = COL_BLOCK_HALF, include_overall = F, FUN = ace_descriptive_statistics_dplyr)
    # merge
    analy = list(rt_acc, acc, rw, rt_block_half, rw_block_half)
  } else {
    # merge
    analy = list(rt_acc, acc, rt_block_half)
  }
  
  # TODO: Add version of proc_by_condition using a relabeled acc column where all lates are wrong
  
  merged = multi_merge(analy, by = COL_BID)
  return (merged)
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
  try({
    by_condition = apply_stats_dplyr(
      x = df, 
      id_var = COL_BID,
      col = variable,
      factors = factors,
      FUN = FUN,
      transform_dir = transform_dir)
  }, silent = TRUE)
  
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