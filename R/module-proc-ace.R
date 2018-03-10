
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
#' @param df a \code{\link{data.frame}} containing formatted trialwise ACE data. 
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
#' @param rm_outlier_rts_sd numeric. Remove within-subject RTs further than this many SD from
#' within-subject mean RT? Enter as one number. Specify either this or \code{rm_outlier_rts_range},
#' but not both. If both specified, will use SD cutoff. Defaults to \code{FALSE}.
#' @param rm_outlier_rts_range numeric vector, length 2. Remove within-subject RTs outside of
#' this specified range? Enter min and max accepted RTs as a vector length 2. If min or max
#' not specified, enter that value as NA in the vector. Specify either this or \code{rm_outlier_rts_range},
#' but not both. If both specified, will use SD cutoff. Defaults to \code{FALSE}.
#' @param rm_short_subs logical. Remove subjects with <1/2 of trials? Defaults to \code{TRUE}
#' @param conditions character vector. If data contains multiple study conditions
#' (e.g. pre & post), specify their labels here. Case insensitive.
#' @param verbose logical. Print details? Defaults to \code{FALSE}.
#' @return Returns summary statistics for every unique module included in the 
#'  data as a list. Throws warnings for modules with undefined methods. 
#'  See \code{\link{ace_procs}} for a list of supported modules.

proc_ace_by_module <- function(df, modules = "all", output = "wide",
                           rm_outlier_rts_sd = FALSE,
                           rm_outlier_rts_range = FALSE,
                           rm_short_subs = TRUE, conditions = NULL, verbose = FALSE) {
  all_mods = subset_by_col(df, "module")
  if (any(modules != "all")) {
    modules = toupper(modules)
    if (any(!(modules %in% ALL_MODULES))) {
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
    # optionally scrubbing trials with "outlier" RTs
    if ((rm_outlier_rts_sd != FALSE | rm_outlier_rts_range != FALSE) & !(name %in% c(SPATIAL_SPAN, BACK_SPATIAL_SPAN, ISHIHARA))) {
      df = df %>%
        group_by_(COL_BID) %>%
        mutate(rt = remove_rts(rt, sd.cutoff = rm_outlier_rts_sd, range.cutoff = rm_outlier_rts_range)) %>%
        ungroup()
    }
    tryCatch({
      if (verbose) print(paste("processing", name, sep = " "))
      proc = eval(call(fun, mod))
      names(proc) = standardized_proc_column_names(proc)
      # scrubbing instances of data with too few trials (likely false starts)
      # rm_short_subs controls whether this occurs
      if (rm_short_subs & !(name %in% c(SPATIAL_SPAN, BACK_SPATIAL_SPAN, FILTER, ISHIHARA))) {
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
    names(valid)[!(names(valid) %in% valid_demos)] = paste(name, names(valid)[!(names(valid) %in% valid_demos)], sep = ".")
    
    if (i == 1) {
      wide = valid
    } else if (name == FILTER) {
      held_out_filter = valid
    } else {
      wide = full_join(wide, valid, by = valid_demos)
    }
    
  }
  if (output == "wide" & FILTER %in% all_names) {
    held_out_filter = full_join(held_out_filter, wide[, valid_demos], by = valid_demos)
    return (list("ALL_OTHER_DATA" = wide,
                 "FILTER" = held_out_filter))
  } else if (output == "wide") {
    return (wide)
  } else if (output == "list") {
    return (out)
  }
}

#' @keywords internal
#' Expects a vector (of RTs)

remove_rts <- function(vec, sd.cutoff, range.cutoff) {
  if (sd.cutoff != FALSE & range.cutoff != FALSE) {
    warning("Both SD and range specified for within-subj outlier RT scrubbing, using SD cutoff.")
    range = FALSE
  }
  if (is.character(vec)) vec = as.numeric(vec)
  if (sd.cutoff != FALSE) vec[abs(scale(vec)) > sd.cutoff] = NA
  else if (range.cutoff != FALSE) {
    if (!is.na(range.cutoff[1])) vec[vec < range.cutoff[1]] = NA
    if (!is.na(range.cutoff[2])) vec[vec > range.cutoff[2]] = NA
  }
  return(vec)
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
  all_possible_demos = c(COL_BID, COL_PID, COL_AGE, COL_GRADE, COL_GENDER, COL_TIME, COL_FILE, COL_STUDY_COND)
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