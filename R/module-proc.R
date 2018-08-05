
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
#' @import dplyr
#' @importFrom stringr str_replace
#' @importFrom purrr map map2 map_int pmap reduce
#' @importFrom stats aggregate median na.omit qnorm sd time var
#' @importFrom tidyr nest
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
#' where one dataframe is output containing cols with data from all modules, or \code{"long"},
#'  where a dataframe is output, with a list-column containing dataframes with each module's data.
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

proc_by_module <- function(df, modules = "all", output = "wide",
                               rm_outlier_rts_sd = FALSE,
                               rm_outlier_rts_range = FALSE,
                               rm_short_subs = FALSE, conditions = NULL, verbose = FALSE) {
  # if data now comes in as list-columns of separate dfs per module, subset_by_col is deprecated
  all_mods = df
  
  # select some modules to process out of all present, if specified
  if (any(modules != "all")) {
    if (any(!(modules %in% c(ALL_MODULES, ALL_SEA_MODULES)))) {
      warning("Modules improperly specified! Check spelling?")
      return (data.frame())
    }
    all_mods <- all_mods %>%
      filter(module %in% modules)
  }
  
  all_procs <- all_mods %>%
    mutate(demos = map(data, ~.x %>%
                         select(one_of(ALL_POSSIBLE_DEMOS)) %>%
                         select(-time) %>%
                         distinct()),
           # this should extract between-subject study conditions from file names
           demos = map(demos, function(x) {
             if (!is.null(conditions)) {
               return (label_study_conditions(x, conditions))
             } else {
               return (x)
             }
           }),
           # rename "correct_button" etc to "acc"
           proc = pmap(list(data, module, verbose), function(a, b, c) {
             attempt_module(a, b, verbose = c) %>%
               as_tibble() %>%
               rename_all(funs(str_replace(., COL_CORRECT_BUTTON, "acc"))) %>%
               rename_all(funs(str_replace(., COL_CORRECT_RESPONSE, "acc")))
           }),
           # scrubbing instances of data with too few trials (likely false starts)
           # rm_short_subs controls whether this occurs
           proc = map2(proc, module, function(x, y) {
             if (rm_short_subs) {
               if (y == RUN_MEM_SPAN) {
                 return (filter(x, acc_strict_length.letter > .5 * median(acc_strict_length.letter)))
               } else {
                 return (filter(x, rt_length.overall > .5 * median(rt_length.overall)))
               }
             } else {
               return (x)
             }
           }),
           # grandfathering Jose's patch for invalid cols produced from empty conditions
           proc = map(proc, ~select(.x, -dplyr::ends_with(".")))) %>%
    # removing any modules that failed to process to allow the remaining ones to bind properly
    filter(map_int(proc, ~nrow(.)) > 0)
  
  # prepare for output
  if (output == "wide") {
    out <- all_procs %>%
      select(module, demos, proc) %>%
      mutate(demos = map(demos, ~.x %>%
                           select(-file) %>%
                           distinct()),
             proc = map2(proc, module, ~.x %>%
                           select(bid, everything()) %>%
                           rename_at(-1, funs(paste0(toupper(.y), ".", .)))),
             proc = map2(proc, demos, ~full_join(.y, .x, by = "bid")))
    valid_demos = get_valid_demos(out$proc[[1]])
    
    # TODO: add functionality to widen filter so everything can go in one wide boy
    return (reduce(out$proc, dplyr::full_join, by = valid_demos))
  } else if (output == "long") {
    out <- all_procs %>%
      select(module, demos, proc) %>%
      mutate(proc = map2(proc, demos, ~full_join(.y, .x, by = "bid")),
             proc = set_names(proc, module)) %>%
      select(-demos)
    return (out)
  }
}

#' @details Expects a vector of RTs
#' @keywords internal

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

#' @keywords internal deprecated

get_proc_info <- function(mod, proc, conditions) {
  
  valid_demos = get_valid_demos(mod)
  info = mod[, valid_demos]
  info = distinct(info)
  
  if (!missing(conditions)) {info = label_study_conditions(info, conditions)}
  return (merge(info, proc, by = COL_BID))
}

#' @keywords internal

get_valid_demos = function(df) {
  return (names(df)[names(df) %in% c(ALL_POSSIBLE_DEMOS, COL_STUDY_COND)])
}

#' @keywords internal

label_study_conditions = function(info, conditions) {
  info$study_condition = NA
  for (cond in 1:length(conditions)) {
    info[grepl(conditions[cond], info[, COL_FILE], ignore.case = T), COL_STUDY_COND] = tolower(conditions[cond])
  }
  return (info)
}

#' @keywords internal deprecated

PROC_COL_OLD = c(COL_CORRECT_BUTTON, COL_CORRECT_RESPONSE)

#' @keywords internal deprecated

PROC_COL_NEW = c("acc", "acc")

#' @keywords internal deprecated

standardized_proc_column_names <- function(x) {
  return (multi_gsub(PROC_COL_OLD, PROC_COL_NEW, x))
}
