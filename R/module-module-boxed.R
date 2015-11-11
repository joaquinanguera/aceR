
#' @keywords internal
#' @name ace_procs

module_boxed <- function(df) {
  rt = apply_stats(x = df, y = PBG, col = "response_time", FUN = ace_descriptive_statistics, suffix = "overall")
  acc = apply_stats(x = df, y = PBG, col = "correct_button", FUN = ace_descriptive_statistics, suffix = "overall")
  rt_by_acc = apply_stats(x = df, y = PBG, col = "response_time", factor = "correct_button", FUN = ace_descriptive_statistics_by_group)
  return (apply_stats_transform(list(rt, acc, rt_by_acc), "group", "participant_id"))
}