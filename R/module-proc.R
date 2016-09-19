
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
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' @param verbose logical. Print details? Defaults to \code{FALSE}.
#'
#' @return Returns summary statistics for every unique module included in the 
#'  data as a list. Throws warnings for modules with undefined methods. 
#'  See \code{\link{ace_procs}} for a list of supported modules.

proc_by_module <- function(df, verbose = FALSE) {
  all_mods = subset_by_col(df, "module")
  all_names = names(all_mods)
  out = list()
  for (i in 1:length(all_mods)) {
    name = all_names[i]
    mod = all_mods[i][[1]]
    fun = paste("module", tolower(name), sep = "_")
    tryCatch({
      if (verbose) print(paste("processing", name, sep = " "))
      proc = eval(call(fun, mod))
      names(proc) = standardized_proc_column_names(proc)
      info = plyr::ddply(mod, c(COL_BID), 
        function(x) {
          return (c(
            x[, COL_PID][[1]],
            x[, COL_AGE][[1]],
            x[, COL_GRADE][[1]],
            x[, COL_GENDER][[1]],
            x[, COL_TIME][[1]], 
            x[, COL_FILE][[1]]))
        })
      names(info)[2:length(info)] = c(COL_PID, COL_AGE, COL_GRADE, COL_GENDER, COL_TIME, COL_FILE)
      all = merge(info, proc, by = COL_BID)
      all$module = name
      # UGLY MONKEY PATCH: get rid of invalid columns
      # One example where this happens is when we apply stats to a variable by a grouping column 
      # but the grouping variable is blank for a given row. We end up with columns that end in "."
      # just removing them for now here.
      valid_cols = sapply(names(all), function(x) stringr::str_sub(x, start = -1) != ".")
      valid = all[valid_cols]
      out[[name]] = valid
    }, error = function(e) {
      warning(e)
    })
  }
  return (out)
}

#' @keywords internal

PROC_COL_OLD = c(COL_CORRECT_BUTTON, COL_CORRECT_RESPONSE)

#' @keywords internal

PROC_COL_NEW = c("acc", "acc")

#' @keywords internal

standardized_proc_column_names <- function(df) {
  return (multi_gsub(PROC_COL_OLD, PROC_COL_NEW, names(df)))
}