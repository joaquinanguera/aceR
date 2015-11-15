
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  
  # overall
  rw_overall = apply_stats(x = df, y = PARTICIPANT_BY_PARTICIPANT, col = COL_RW, FUN = ace_descriptive_statistics, suffix = "overall")
  rt_overall = apply_stats(x = df, y = PARTICIPANT_BY_PARTICIPANT, col = COL_RT, FUN = ace_descriptive_statistics, suffix = "overall")
  acc_overall = apply_stats(x = df, y = PARTICIPANT_BY_PARTICIPANT, col = COL_ACC, FUN = ace_descriptive_statistics, suffix = "overall")
  
  # by feature
  rw_by_feature = apply_stats(x = df, y = PARTICIPANT_BY_CONDITION, col = COL_RW, FUN = ace_descriptive_statistics, suffix = "overall")
  rt_by_feature = apply_stats(x = df, y = PARTICIPANT_BY_CONDITION, col = COL_RT, FUN = ace_descriptive_statistics, suffix = "overall")
  acc_by_feature = apply_stats(x = df, y = PARTICIPANT_BY_CONDITION, col = COL_ACC, FUN = ace_descriptive_statistics, suffix = "overall")
  turns_by_feature = apply_stats(x = df, y = PARTICIPANT_BY_CONDITION, col = COL_RW, factor = COL_ACC, FUN = ace_average_turns,  suffix = "overall")
  
  # by feature & accuracy
  rt_by_featute_and_acc = apply_stats(x = df, y = PARTICIPANT_BY_CONDITION, col = COL_RT, factor = COL_ACC, FUN = ace_descriptive_statistics_by_group)
  
  # merge
  proc_apply = apply_stats_transform(
    list(rw_by_feature, rt_by_feature, acc_by_feature, rt_by_featute_and_acc, turns_by_feature), COL_CONDITION, COL_PID)
  proc_all = multi_merge(
    list(rw_overall, rt_overall, acc_overall, proc_apply), 
    by = COL_PID)
    
  return (proc_all)
}