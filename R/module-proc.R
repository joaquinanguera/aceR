
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
#' @param conditions vector of strings. If data contains multiple study conditions
#' (e.g. pre & post), specify their labels here. Case insensitive.
#' @param verbose logical. Print details? Defaults to \code{FALSE}.
#'
#' @return Returns summary statistics for every unique module included in the 
#'  data as a list. Throws warnings for modules with undefined methods. 
#'  See \code{\link{ace_procs}} for a list of supported modules.

proc_by_module <- function(df, conditions = NULL, verbose = FALSE) {
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
      # scrubbing instances of data with too few trials (likely false starts)
      if (!(name %in% c(SPATIAL_SPAN, BACK_SPATIAL_SPAN))) {
        proc = proc[proc$rt_length.overall > .75 * median(proc$rt_length.overall), ]
      }
      # TODO: clean up implementation of this... should do this in a tryCatch
      if(!is.null(conditions)) {
        all = get_proc_info(mod, proc, conditions)
      } else {
        all = get_proc_info(mod, proc)
      }
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

get_proc_info <- function(mod, proc, conditions) {
  if (COL_GRADE %in% names(mod)) {
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
  } else if (COL_AGE %in% names(mod)){
    info = plyr::ddply(mod, c(COL_BID),
                       function(x) {
                         return (c(
                           x[, COL_PID][[1]],
                           x[, COL_AGE][[1]],
                           x[, COL_GENDER][[1]],
                           x[, COL_TIME][[1]], 
                           x[, COL_FILE][[1]]))
                       })
    names(info)[2:length(info)] = c(COL_PID, COL_AGE, COL_GENDER, COL_TIME, COL_FILE)
  } else if (COL_GENDER %in% names(mod)){
    info = plyr::ddply(mod, c(COL_BID),
                       function(x) {
                         return (c(
                           x[, COL_PID][[1]],
                           x[, COL_GENDER][[1]],
                           x[, COL_TIME][[1]], 
                           x[, COL_FILE][[1]]))
                       })
    names(info)[2:length(info)] = c(COL_PID, COL_GENDER, COL_TIME, COL_FILE)
  } else { # if NO demos included
    info = plyr::ddply(mod, c(COL_BID),
                       function(x) {
                         return (c(
                           x[, COL_PID][[1]],
                           x[, COL_TIME][[1]], 
                           x[, COL_FILE][[1]]))
                       })
    names(info)[2:length(info)] = c(COL_PID, COL_TIME, COL_FILE)
  }
  if (!missing(conditions)) {info = label_study_conditions(info, conditions)}
  return (merge(info, proc, by = COL_BID))
}

#' @keywords internal

label_study_conditions = function(info, conditions) {
    info$study_condition = NA
    for (cond in 1:length(conditions)) {
      info[grepl(conditions[cond], info[, COL_FILE], ignore.case = T), COL_STUDY_COND] = tolower(conditions[cond])
    }
    return (info)
}

#' @keywords internal

PROC_COL_OLD = c(COL_CORRECT_BUTTON, COL_CORRECT_RESPONSE)

#' @keywords internal

PROC_COL_NEW = c("acc", "acc")

#' @keywords internal

standardized_proc_column_names <- function(df) {
  return (multi_gsub(PROC_COL_OLD, PROC_COL_NEW, names(df)))
}