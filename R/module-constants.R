
#' @importFrom dplyr if_else mutate rename_with
#' @importFrom magrittr %>%
#' @importFrom rlang !! := quo_name
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
    acc_late.incorrect = df %>% 
      mutate(correct_button = if_else(!!Q_COL_LATE_RESPONSE == "late",
                                             "incorrect",
                                             !!Q_COL_CORRECT_BUTTON),
             !!quo_name(col_condition) := paste0("late_incorrect.", !!col_condition)) %>% 
      proc_by_condition(quo_name(col_acc), factors = col_condition, include_overall = FALSE, FUN = FUN) # %>% 
      #rename_with(~paste0(., ".late_incorrect"), .cols = -(!!COL_BID))
    
    acc = full_join(acc, acc_late.incorrect, by = COL_BID)
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
  
  # Should only activate for ACE Explorer data, where this column is passed through
  # Summarizes # practice rounds completed (already should be one unique value per participant)
  if (COL_PRACTICE_COUNT %in% names(df)) {
    prac = proc_by_condition(df, COL_PRACTICE_COUNT, include_overall = FALSE, FUN = ace_practice_count)
    analy = c(analy, list(prac))
  }
  
  # TODO: Add version of proc_by_condition using a relabeled acc column where all lates are wrong
  # again, assume all repeated column names are in fact the same columns
  merged = suppressMessages(plyr::join_all(analy))
  return (merged)
}

#' @importFrom dplyr left_join
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

#' @importFrom dplyr contains ends_with rename_with select
#' @importFrom magrittr %>%
#' @importFrom tidyselect everything
#' @keywords internal

clean_proc_cols <- function (df) {
  df <- df %>%
    rename_with(tolower, .cols = everything()) %>%
    rename_with(~str_replace(., COL_CORRECT_BUTTON, "acc"), .cols = everything()) %>%
    rename_with(~str_replace(., COL_CORRECT_RESPONSE, "acc"), .cols = everything()) %>%
    select(-contains(".short"), -contains(".no_response"), -contains(".late"),
           -contains("acc_median"), -contains("acc_sd"),
           -contains(".NA"), -contains("prev_na"), -contains("prev_no_response"),
           -contains("rw_count"), -contains("rw_length"),
           -contains("count.cost"), -contains("length.cost")) %>%
    # remove min summary cols for not response window
    select(!(contains("_min") & !contains("rw"))) %>% 
    # grandfathering Jose's patch for invalid cols produced from empty conditions
    select(-ends_with("."))
  
  return (df)
}
