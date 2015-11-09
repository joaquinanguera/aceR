
#' @export

proc_by_module <- function(df) {
  all_mods = subset_by_col(df, "module")
  all_names = names(all_mods)
  out = data.frame()
  for (i in 1:length(all_mods)) {
    name = all_names[i]
    mod = all_mods[i][[1]]
    if (grepl(BOXED, name)) {
      proc = module_boxed(mod)
    } else if (grepl(BRT, name)) {
      proc = data.frame() # TODO: proc BRT
    } else if (grepl(DISCRIMINATION, name)) {
      proc = data.frame() # TODO: proc DISCRIMINATION
    } else if (grepl(FLANKER, name)) {
      proc = data.frame() # TODO: proc FLANKER
    } else if (grepl(SAAT, name)) {
      proc = data.frame() # TODO: proc SAAT
    } else if (grepl(SPATIAL_SPAN, name)) {
      proc = data.frame() # TODO: proc SPATIAL_SPAN
    } else if (grepl(STROOP, name)) {
      proc = data.frame() # TODO: proc STROOP
    } else if (grepl(TASK_SWITCH, name)) {
      proc = data.frame() # TODO: proc TASK_SWITCH
    } else if (grepl(TNT, name)) {
      proc = data.frame() # TODO: proc TNT
    } else {
      # TBD: warning? error?
    }
    out = plyr::rbind.fill(out, proc)
  }
  return (out)
}