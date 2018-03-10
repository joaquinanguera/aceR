
#' SEA module procs
#'
#' Methods for processing user data by \emph{group}.
#'
#' @keywords internal
#' @name sea_procs
NULL

#' Process SEA data by module
#'
#' Applies corresponding \code{\link{sea_procs}} to every unique module.
#'
#' @section Assumptions:
#' Assumes the column \emph{module} exists in the \code{\link{data.frame}}.
#'
#' @export
#' @importFrom stats aggregate median na.omit qnorm sd time var
#' @param df a \code{\link{data.frame}} containing formatted trialwise SEA data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_sea_file}}
#'   \item \code{\link{load_sea_bulk}}
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

proc_sea_by_module <- function(df, modules = "all", output = "wide",
                           rm_outlier_rts_sd = FALSE,
                           rm_outlier_rts_range = FALSE,
                           rm_short_subs = TRUE, conditions = NULL, verbose = FALSE) {
  # TODO: Add module_SEA for all SEA modules. Should be able to call proc_generic_module for these
  all_mods = subset_by_col(df, "module")
  if (any(modules != "all")) {
    if (any(!(modules %in% ALL_SEA_MODULES))) {
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
    fun = paste("module", tolower(replace_spaces(name, replacement = "_")), sep = "_")
    # optionally scrubbing trials with "outlier" RTs
    if ((rm_outlier_rts_sd != FALSE | rm_outlier_rts_range != FALSE)) {
      df = df %>%
        group_by(!! rlang::sym(UQ(COL_BID))) %>%
        mutate(rt = remove_rts(.data$rt, sd.cutoff = rm_outlier_rts_sd, range.cutoff = rm_outlier_rts_range)) %>%
        ungroup()
    }
    tryCatch({
      if (verbose) print(paste("processing", name, sep = " "))
      proc = eval(call(fun, mod))
      names(proc) = standardized_proc_column_names(proc)
      # scrubbing instances of data with too few trials (likely false starts)
      # rm_short_subs controls whether this occurs
      if (rm_short_subs) {
        proc = proc[proc$rt_length.overall > .5 * median(proc$rt_length.overall), ]
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
