
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
#' @importFrom stats aggregate median na.omit qnorm sd time var
#' @param df a \code{\link{data.frame}} containing formatted ACE data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' @param modules character vector. Specify the names of modules (proper naming convention!)
#' to output data for. Defaults to all modules detected in data.
#' @param output string indicating preferred output format. Can be \code{"wide"} (default),
#' where one dataframe is output containing cols with data from all modules, or \code{"list"},
#' where a list is output, with each element containing a dataframe with one module's data.
#' @param scrub_short logical. Remove subjects with <1/2 of trials? Defaults to \code{TRUE}
#' @param conditions character vector. If data contains multiple study conditions
#' (e.g. pre & post), specify their labels here. Case insensitive.
#' @param verbose logical. Print details? Defaults to \code{FALSE}.
#' @return Returns summary statistics for every unique module included in the 
#'  data as a list. Throws warnings for modules with undefined methods. 
#'  See \code{\link{ace_procs}} for a list of supported modules.

proc_by_module <- function(df, modules = "all", output = "wide", scrub_short = TRUE, conditions = NULL, verbose = FALSE) {
  # TODO: Create param to allow user to specify threshold for within-subject weird RT trial scrubbing
  # TODO: Add module_SEA for all SEA modules. Should be able to call proc_generic_module for these
  all_mods = subset_by_col(df, "module")
  if (any(modules != "all")) {
    modules = toupper(modules)
    if (any(!(modules %in% c(BOXED, BRT, FLANKER, SAAT, SPATIAL_SPAN, STROOP, TASK_SWITCH, TNT, FILTER, BACK_SPATIAL_SPAN, ISHIHARA, SPATIAL_CUE)))) {
      warning("Modules improperly specified! Check spelling?")
      return (data.frame())
    }
    all_mods = all_mods[modules]
  }
  all_names = names(all_mods)
  out = list()
  wide = data.frame()

  for (i in 1:length(all_mods)) {
    name = all_names[i]
    mod = all_mods[i][[1]]
    fun = paste("module", tolower(name), sep = "_")
    tryCatch({
      if (verbose) print(paste("processing", name, sep = " "))
      proc = eval(call(fun, mod))
      names(proc) = standardized_proc_column_names(proc)
      # scrubbing instances of data with too few trials (likely false starts)
      # scrub_short controls whether this occurs
      if (scrub_short & !(name %in% c(SPATIAL_SPAN, BACK_SPATIAL_SPAN, FILTER, ISHIHARA))) {
        proc = proc[proc$rt_length.overall > .5 * median(proc$rt_length.overall), ]
      } else if (name == FILTER) {
        proc = proc[proc$rt_length.overall.2 > .5 * median(proc$rt_length.overall.2), ]
      }
      
      # TODO: clean up implementation of this... should do this in a tryCatch
      if (!is.null(conditions)) {
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

    # Prepare wide dataframe for output
    # Get at me, but recursive full_join() calls seem like the way to go here
    # Need full_join() so we can pad out Ss that are missing data for various tasks    
    
    # REMOVE BID, file, time from valid demo cols because they're different between modules for the same subject (tasks are administered sequentially)
    valid = select_(valid, paste0("-", COL_BID), paste0("-", COL_FILE), paste0("-", COL_TIME), "-module")
    valid_demos = get_valid_demos(valid)
    
    # For all cols that AREN'T demographics, prepend the module name to the colname
    names(valid)[!(names(valid) %in% c(valid_demos, COL_STUDY_COND))] = paste(name, names(valid)[!(names(valid) %in% c(valid_demos, COL_STUDY_COND))], sep = ".")
    
    if (i == 1) {
      wide = valid
    } else if (!is.null(conditions)) {
      wide = full_join(wide, valid, by = c(valid_demos, COL_STUDY_COND))
    } else {
      wide = full_join(wide, valid, by = valid_demos)
    }
      
  }
    if (output == "wide") {
      return (wide)
    } else if (output == "list") {
      return (out)
    }
}

#' @keywords internal

get_proc_info <- function(mod, proc, conditions) {
  
  valid_demos = get_valid_demos(mod)
  info = mod[, valid_demos]
  info = distinct(info)
  
  if (!missing(conditions)) {info = label_study_conditions(info, conditions)}
  return (merge(info, proc, by = COL_BID))
}

#' @keywords internal

get_valid_demos = function(df) {
  all_possible_demos = c(COL_BID, COL_PID, COL_AGE, COL_GRADE, COL_GENDER, COL_TIME, COL_FILE)
  return (names(df)[names(df) %in% all_possible_demos])
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