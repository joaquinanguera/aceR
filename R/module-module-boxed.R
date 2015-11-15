
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  
  # overall
  rw_overall = apply_stats(x = df, y = c("participant_id"), col = COL_RW, FUN = ace_descriptive_statistics, suffix = "overall")
  rt_overall = apply_stats(x = df, y = c("participant_id"), col = COL_RT, FUN = ace_descriptive_statistics, suffix = "overall")
  acc_overall = apply_stats(x = df, y = c("participant_id"), col = COL_ACC, FUN = ace_descriptive_statistics, suffix = "overall")
  
  # by feature
  rw_by_feature = apply_stats(x = df, y = PBG, col = COL_RW, FUN = ace_descriptive_statistics, suffix = "overall")
  rt_by_feature = apply_stats(x = df, y = PBG, col = COL_RT, FUN = ace_descriptive_statistics, suffix = "overall")
  acc_by_feature = apply_stats(x = df, y = PBG, col = COL_ACC, FUN = ace_descriptive_statistics, suffix = "overall")
  turns_by_feature = apply_stats(x = df, y = PBG, col = COL_RW, factor = COL_ACC, FUN = ace_average_turns,  suffix = "overall")
  
  # by feature & accuracy
  rt_by_featute_and_acc = apply_stats(x = df, y = PBG, col = COL_RT, factor = COL_ACC, FUN = ace_descriptive_statistics_by_group)
  
  # merge
  proc_apply = apply_stats_transform(
    list(rw_by_feature, rt_by_feature, acc_by_feature, rt_by_featute_and_acc, turns_by_feature), 
    "condition", "participant_id")
  proc_all = multi_merge(
    list(rw_overall, rt_overall, acc_overall, proc_apply), 
    by = "participant_id")
    
  return (proc_all)
}