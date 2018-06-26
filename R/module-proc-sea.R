
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
#' @import dplyr
#' @importFrom stats aggregate median na.omit qnorm sd time var
#' @importFrom stringr str_replace
#' @importFrom purrr map map2 pmap reduce
#' @importFrom tidyr nest
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
#' where one dataframe is output containing cols with data from all modules, or \code{"long"},
#' where a dataframe is output, with a list-column containing a dataframe with each module's data.
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
#'  See \code{\link{sea_procs}} for a list of supported modules.

proc_sea_by_module <- function(df, modules = "all", output = "long",
                               rm_outlier_rts_sd = FALSE,
                               rm_outlier_rts_range = FALSE,
                               rm_short_subs = FALSE, conditions = NULL, verbose = FALSE) {
  
  # optionally scrubbing trials with "outlier" RTs
  if ((rm_outlier_rts_sd != FALSE | rm_outlier_rts_range != FALSE)) {
    all_mods = df %>%
      group_by(bid, module) %>%
      mutate(rt = remove_rts(.data$rt, sd.cutoff = rm_outlier_rts_sd, range.cutoff = rm_outlier_rts_range)) %>%
      group_by(module) %>%
      nest() 
  } else {
    all_mods = df %>%
      group_by(module) %>%
      nest()
  }
  
  # select some modules to process out of all present, if specified
  if (any(modules != "all")) {
    if (any(!(modules %in% ALL_SEA_MODULES))) {
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
           proc = map(proc, function(x) {
             if (rm_short_subs) {
               return (filter(x, rt_length.overall > .5 * median(rt_length.overall)))
             } else {
               return (x)
             }
           }),
           # grandfathering Jose's patch for invalid cols produced from empty conditions
           proc = map(proc, ~select(.x, -dplyr::ends_with(".")))) %>%
    # removing any modules that failed to process to allow the remaining ones to bind properly
    filter(map(proc, ~nrow(.)) > 0)
    
  
  if (output == "wide") {
    out <- all_procs %>%
      select(module, demos, proc) %>%
      mutate(proc = map2(proc, module, ~.x %>%
                           select(bid, everything()) %>%
                           rename_at(-1, funs(paste0(tolower(.y), ".", .)))),
             proc = map2(proc, demos, ~full_join(.y, .x, by = "bid")))
    valid_demos = get_valid_demos(out$proc[[1]])
    return (reduce(out$proc, full_join, by = valid_demos))
  } else if (output == "long") {
    out <- all_procs %>%
      select(module, demos, proc) %>%
      mutate(proc = map2(proc, demos, ~full_join(.y, .x, by = "bid")),
             proc = set_names(proc, module)) %>%
      select(-demos)
    return (out)
  }
}
