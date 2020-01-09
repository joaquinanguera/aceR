
#' ACE/SEA module procs
#'
#' Methods for processing user data by \emph{group}.
#'
#' @keywords internal
#' @name ace_procs
NULL

#' Process ACE/SEA data by module
#'
#' Applies corresponding \code{\link{ace_procs}} to every unique module.
#'
#' @section Assumptions:
#' Assumes the \code{\link{data.frame}} is nested, with two columns:
#' \code{module} (character) and \code{data} (list, each containing a \code{\link{data.frame}}).
#'
#' @export
#' @import dplyr
#' @importFrom stringr str_replace
#' @importFrom purrr map map2 map_int pmap reduce rerun
#' @importFrom rlang !!
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
#' @param conditions character vector. If data contains multiple study conditions
#' (e.g. pre & post), specify their labels here. Case insensitive.
#' @param verbose logical. Print details? Defaults to \code{TRUE}.
#' @return Returns summary statistics for every unique module included in the 
#'  data as a list. Throws warnings for modules with undefined methods. 
#'  See \code{\link{ace_procs}} for a list of supported modules.

proc_by_module <- function(df, modules = "all", output = "wide",
                               conditions = NULL, verbose = TRUE) {
  
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
  
  # need this for proper specification of which demos and such to pull
  is_ace = if_else(all(all_mods$module %in% ALL_MODULES), TRUE, FALSE)
  
  if (is_ace) {
    all_these_demos = ALL_POSSIBLE_DEMOS
  } else {
    all_these_demos = ALL_POSSIBLE_SEA_DEMOS
  }
  
  if (is_ace) {
    all_procs <- all_mods %>%
      # Put demos in another column, wide-ish, so it's next to every other module
      mutate(demos = rerun(n(), .$data[.$module == DEMOS][[1]])) %>%
      filter(module != DEMOS) %>%
      # patch handedness from demos directly into brt data
      mutate(data = pmap(list(data, module, demos), function(a, b, c) {
        if (b == BRT) {
          reconstruct_pid(a, c) %>%
            left_join(c %>%
                        select(COL_PID, COL_HANDEDNESS),
                      by = COL_PID)
        } else {a}
      }))
  } else {
    all_procs <- all_mods %>%
      mutate(demos = map(data, ~.x %>%
                           select(one_of(all_these_demos)) %>%
                           select(-!!Q_COL_TIME) %>%
                           distinct()))
  }
  
  all_procs <- all_procs %>%
    mutate(# this should extract between-subject study conditions from file names
           demos = map(demos, function(x) {
             if (!is.null(conditions)) {
               return (label_study_conditions(x, conditions))
             } else {
               return (x)
             }
           }),
           # remove any demo cols that appear to contain no info
           demos = map(demos, ~remove_empty_cols(.x)),
           # rename "correct_button" etc to "acc"
           proc = pmap(list(data, module, verbose), function(a, b, c) {
             attempt_module(a, b, verbose = c) %>%
               as_tibble() %>%
               rename_all(funs(str_replace(., COL_CORRECT_BUTTON, "acc"))) %>%
               rename_all(funs(str_replace(., COL_CORRECT_RESPONSE, "acc")))
           }),
           # grandfathering Jose's patch for invalid cols produced from empty conditions
           proc = map(proc, ~select(.x, -dplyr::ends_with(".")))) %>%
    # removing any modules that failed to process to allow the remaining ones to bind properly
    filter(map_int(proc, ~nrow(.)) > 0)
  
  # prepare for output
  
  
  if (is_ace) {
    # Try this: Ace Explore has demos collected at a separate date/time,
    # so BID will _basically never_ match up. Use PID to bind
    demo_merge_col = COL_PID
    all_procs <- all_procs %>%
      mutate(proc = map2(proc, demos, ~reconstruct_pid(.x, .y)),
             demos = map(demos, ~select(.x, -!!Q_COL_BID, -!!Q_COL_TIME)))
  } else {
    demo_merge_col = COL_BID
  }
  
  if (output == "wide") {
    
    out <- all_procs %>%
      select(module, demos, proc) %>%
      mutate(demos = map(demos, ~.x %>%
                           select(-file) %>%
                           distinct()))
    
    if (is_ace) {
      out <- out %>%
        mutate(proc = map2(proc, module, ~.x %>%
                             select(bid, pid, everything()) %>%
                             rename_at(-(1L:2L), funs(paste(toupper(.y), ., sep = ".")))))
    } else {
      out <- out %>%
        mutate(proc = map2(proc, module, ~.x %>%
                             select(bid, everything()) %>%
                             rename_at(-1L, funs(paste(toupper(.y), ., sep = ".")))))
    }
    
    # ACE explorer data:
    # DO join by full bid. now, with times gameplayed as the session identfier,
    # all modules from a single session should have the same times gameplayed stamp
    if (is_ace) {
      out <- out %>%
        mutate(proc = pmap(list(proc, demos, module), function (a, b, c) {
          right_join(b, a, by = demo_merge_col) %>%
            return()
        }))
    } else {
      out <- out %>%
        mutate(proc = map2(proc, demos, ~right_join(.y, .x, by = demo_merge_col)))
    }
    
    valid_demos = get_valid_demos(out$proc[[1]], is_ace)
    
    out$proc %>%
      reduce(dplyr::full_join, by = valid_demos) %>%
      select(valid_demos, everything()) %>%
      return()
    
  } else if (output == "long") {
    out <- all_procs %>%
      select(module, demos, proc) %>%
      mutate(proc = map2(proc, demos, ~right_join(.y, .x, by = demo_merge_col)),
             proc = rlang::set_names(proc, module)) %>%
      select(-demos)
    return (out)
  }
}

#' @keywords internal

get_valid_demos = function(df, is_ace) {
  if (is_ace) {
    return (names(df)[names(df) %in% c(ALL_POSSIBLE_DEMOS, ALL_POSSIBLE_EXPLORE_DEMOS, COL_STUDY_COND)])
  } else {
    return (names(df)[names(df) %in% c(ALL_POSSIBLE_SEA_DEMOS, COL_STUDY_COND)])
  }
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
#' @importFrom dplyr mutate select everything
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr
#' @importFrom rlang !! :=
#' @importFrom stringr str_split

reconstruct_pid <- function (proc, demo) {
  
  # This SHOULD truncate at the last character before the times finished game portion of the bid
  proc %>% mutate(!!COL_PID := str_split(!!Q_COL_BID, pattern = "[.]"),
                  !!COL_PID := map_chr(!!Q_COL_PID, 1L)) %>%
    select(COL_BID, COL_PID, everything()) %>%
    return()
}

#' @keywords internal deprecated

get_proc_info <- function(mod, proc, conditions) {
  
  valid_demos = get_valid_demos(mod)
  info = mod[, valid_demos]
  info = distinct(info)
  
  if (!missing(conditions)) {info = label_study_conditions(info, conditions)}
  return (merge(info, proc, by = COL_BID))
}

#' @keywords internal deprecated

PROC_COL_OLD = c(COL_CORRECT_BUTTON, COL_CORRECT_RESPONSE)

#' @keywords internal deprecated

PROC_COL_NEW = c("acc", "acc")

#' @keywords internal deprecated

standardized_proc_column_names <- function(x) {
  return (multi_gsub(PROC_COL_OLD, PROC_COL_NEW, x))
}
