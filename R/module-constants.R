
#' @importFrom rlang quo_name
#' @keywords internal
#' @param df data for one module
#' @param col_acc column for accuracy, as string
#' @param col_condition column containing "condition" or trial type, as string

proc_generic_module <- function(df,
                                col_acc = Q_COL_CORRECT_BUTTON,
                                col_condition = Q_COL_CONDITION,
                                col_prev_acc = Q_COL_PREV_CORRECT_BUTTON,
                                FUN = ace_descriptive_statistics) {
  
  # overall & broken-down by condition
  # RT broken-down by condition & accuracy  
  rt_acc = proc_by_condition(df, COL_RT, factors = c(col_condition, col_acc), FUN = FUN)
  # RT by block half
  rt_block_half = proc_by_condition(df, COL_RT, factors = Q_COL_BLOCK_HALF, include_overall = F, FUN = FUN)
  # TODO: RT by previous trial accuracy
  rt_prev_acc = proc_by_condition(df, COL_RT, factors = c(col_condition, col_prev_acc),  include_overall = F, FUN = FUN)
  
  # accuracy broken down by condition and response window (early or late?)
  # if late response is not available for the task, don't factor by it
  if (COL_LATE_RESPONSE %in% names(df)) {
    acc = proc_by_condition(df, quo_name(col_acc), factors = c(col_condition, Q_COL_LATE_RESPONSE), FUN = FUN)
  } else {
    acc = proc_by_condition(df, quo_name(col_acc), factors = col_condition, FUN = FUN)
  }
  # RW just broken down by condition
  # and by block half
  # if RW not available (e.g. SEA data), don't use it
  if (COL_RW %in% names(df)) {
    rw = proc_by_condition(df, COL_RW, factors = col_condition, FUN = FUN)
    rw_block_half = proc_by_condition(df, COL_RW, factors = Q_COL_BLOCK_HALF, include_overall = F, FUN = FUN)
    # merge
    analy = list(rt_acc, acc, rw, rt_prev_acc, rt_block_half, rw_block_half)
  } else {
    # merge
    analy = list(rt_acc, acc, rt_prev_acc, rt_block_half)
  }
  
  # TODO: Add version of proc_by_condition using a relabeled acc column where all lates are wrong
  # again, assume all repeated column names are in fact the same columns
  merged = suppressMessages(plyr::join_all(analy))
  return (merged)
}

#' @importFrom dplyr contains funs rename_all select
#' @importFrom magrittr %>%
#' @keywords internal

proc_by_condition <- function(df, variable, factors, include_overall = TRUE, FUN = ace_descriptive_statistics, transform_dir = "wide") {
  # conditions to be "expected" (though this expectation is dangerous, don't hardcode):
  # subtask type ("col_condition")
  # late response (to subset accuracy by late resp)
  # accuracy (to subset RT by acc)
  # conditions to be subsetted should be fed in as a SYMBOL
  
  overall = apply_stats(
    x = df, 
    id_var = Q_COL_BID,
    col = variable, 
    FUN = FUN, 
    suffix = "overall",
    transform_dir = transform_dir)
  # needs to be flexible to handle 0-n number of subsetting conditions, and also NOT to cross them all with each other necessarily
  try({
    by_condition = apply_stats(
      x = df, 
      id_var = Q_COL_BID,
      col = variable,
      factors = factors,
      FUN = FUN,
      transform_dir = transform_dir)
  }, silent = TRUE)
  
  if (include_overall & exists("by_condition")) {
    proc = left_join(overall, by_condition, by = "bid")
  } else if (include_overall) {
    proc = overall
  } else {
    proc = by_condition
  }
  
  return(proc)
}

#' @importFrom dplyr funs rename_all select
#' @importFrom magrittr %>%
#' @importFrom tidyselect contains ends_with
#' @keywords internal

clean_proc_cols <- function (df) {
  df <- df %>%
    rename_all(funs(tolower(.))) %>%
    rename_all(funs(str_replace(., COL_CORRECT_BUTTON, "acc"))) %>%
    rename_all(funs(str_replace(., COL_CORRECT_RESPONSE, "acc"))) %>%
    select(-contains(".short"), -contains(".no_response"), -contains(".late"),
                  -contains("acc_median"), -contains("acc_sd"),
                  -contains(".NA"), -contains("prev_na"), -contains("prev_no_response"),
                  -contains("rw_count"), -contains("rw_length"),
                  -contains("count.cost"), -contains("length.cost")) %>%
  # grandfathering Jose's patch for invalid cols produced from empty conditions
  select(-ends_with("."))
  
  return (df)
}

#' @keywords internal deprecated

proc_standard <- function (df, variable, col_condition = NULL, col_condition2 = NULL, y = c(Q_COL_BID, col_condition), FUN, transform_dir = "wide", ...) {
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
