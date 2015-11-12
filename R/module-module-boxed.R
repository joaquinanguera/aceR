
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  # overall
  rt_overall = apply_stats(x = df, y = c("participant_id"), col = COL_RT, FUN = ace_descriptive_statistics, suffix = "overall")
  acc_overall = apply_stats(x = df, y = c("participant_id"), col = COL_ACC, FUN = ace_descriptive_statistics, suffix = "overall")
  # by feature
  rt_by_feature = apply_stats(x = df, y = PBG, col = COL_RT, FUN = ace_descriptive_statistics, suffix = "overall")
  acc_by_feature = apply_stats(x = df, y = PBG, col = COL_ACC, FUN = ace_descriptive_statistics, suffix = "overall")
  # by feature & accuracy
  rt_by_featute_and_acc = apply_stats(x = df, y = PBG, col = COL_RT, factor = COL_ACC, FUN = ace_descriptive_statistics_by_group)
  # merge
  proc_apply = apply_stats_transform(list(rt_by_feature, acc_by_feature, rt_by_featute_and_acc), "group", "participant_id")
  proc_all = data.frame(rt_overall, acc_overall, proc_apply)
  return (proc_all)
}