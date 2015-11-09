
#' @export

proc_by_module <- function(df) {
  all_mods = subset_by_col(df, "module")
  all_names = names(all_mods)
  out = data.frame()
  for (i in 1:length(all_mods)) {
    name = all_names[i]
    mod = all_mods[i][[1]]
    fun = paste("module", tolower(name), sep = "_")
    tryCatch({
      proc = eval(call(fun, mod))
      out = plyr:::rbind.fill(out, proc)
    }, error = function(e) {
      warning(e)
    })
  }
  return (out)
}