
#' ACE module procs
#'
#' Methods for processing user data by \emph{group}.
#'
#' @keywords internal
#' @name ace_procs
NULL

#' Process ACE data by module
#'
#' Applies corresponding \code{\link{ace_procs}} to every unique module.
#'
#' @section Assumptions:
#' Assumes the column \emph{module} exists in the \code{\link{data.frame}}.
#'
#' @export
#' @param df a \code{\link{data.frame}} containing formatted ACE data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{read_raw_csv_in_directory}}
#' }
#' @return Returns summary statistics for every unique module included in the 
#'  data. Throws warnings for modules with undefined methods. 
#'  See \code{\link{ace_procs}} for a list of supported modules.

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