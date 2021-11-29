
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
#' Assumes the column \emph{module} exists in the \code{\link{data.frame}}.
#'
#' @export
#' @import dplyr
#' @importFrom stringr str_replace
#' @importFrom purrr map map2 map_int pmap reduce rerun
#' @importFrom rlang !!
#' @importFrom stats aggregate median na.omit qnorm sd time var
#' @importFrom tidyr nest
#' @importFrom tidyselect any_of
#' @param df a \code{\link{data.frame}} containing formatted trialwise ACE data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' @param app_type character. What app type produced this data? One of
#' \code{c("classroom", "explorer", "sea")}. Must be specified.
#' @param modules character vector. Specify the names of modules (proper naming convention!)
#' to output data for. Defaults to all modules detected in data.
#' @param output string indicating preferred output format. Can be \code{"wide"} (default),
#' where one dataframe is output containing cols with data from all modules, or \code{"long"},
#'  where a dataframe is output, with a list-column containing dataframes with each module's data.
#' @param conditions character vector. If data contains multiple study conditions
#' (e.g. pre & post), specify their labels here. Case insensitive.
#' @param verbose logical. Print details? Defaults to \code{FALSE}.
#' @return Returns summary statistics for every unique module included in the 
#' data as a list. Throws warnings for modules with undefined methods. 
#' See \code{\link{ace_procs}} for a list of supported modules.

proc_by_module <- function(df,
                           app_type = c("classroom", "explorer", "sea"),
                           modules = "all",
                           output = "wide",
                           conditions = NULL, verbose = TRUE) {
  stopifnot(length(app_type) == 1)
  # if data now comes in as list-columns of separate dfs per module, subset_by_col is deprecated
  
  # select some modules to process out of all present, if specified
  if (any(modules != "all")) {
    if (check_module_misspelling(modules)) {
      return (data.frame())
    }
    df <- df %>%
      filter(module %in% modules)
  }
  
  if (any(df$module == "unknown")) {
    warning(crayon::yellow("Unsupported modules found. They will not be processed."))
    df <- df %>%
      filter(module != "unknown")
  }
  
  # need this for proper specification of which demos and such to pull
  is_ace = app_type != "sea"
  
  if (app_type == "classroom") {
    all_these_demos = ALL_POSSIBLE_DEMOS
  } else if (app_type == "explorer") {
    # TODO: If you want ALL_POSSIBLE_EXPLORE_DEMOS, it goes in here with ALL_POSSIBLE_DEMOS
    # But maybe this functionality should wait until the device stuff is faithfully only in the task data
    all_these_demos = ALL_POSSIBLE_DEMOS
  } else {
    all_these_demos = ALL_POSSIBLE_SEA_DEMOS
  }
  
  if (app_type != "sea") {
    # If ACE Explorer, basically

    if (app_type == "explorer") {
      out <- df %>%
        # Put demos in another column, wide-ish, so it's next to every other module
        mutate(demos = map(1:n(), ~df$data[df$module == DEMOS][[1]])) %>%
        filter(module != DEMOS)
    } else {
      out <- df
    }
    
    out <- out %>%
      # patch handedness from demos directly into brt data
      mutate(data = pmap(list(data, module, demos), function(a, b, c) {
        if (b == BRT) {
          reconstruct_pid(a) %>%
            left_join(c %>%
                        select(COL_PID, COL_HANDEDNESS),
                      by = COL_PID)
        } else {a}
      }))
    
  } else {
    # This is now here for SEA compatibility
    out <- df %>%
      mutate(demos = map(data, ~.x %>%
                           select(any_of(all_these_demos)) %>%
                           select(-!!Q_COL_TIME) %>%
                           distinct()))
  }
  
  out <- out %>%
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
           proc = pmap(list(data, module, verbose), function(a, b, c) {
             attempt_module(a, b, app_type = app_type, verbose = c)
           })) %>%
    # removing any modules that failed to process to allow the remaining ones to bind properly
    filter(map_int(proc, ~nrow(.)) > 0)
  
  # prepare for output
  
  if (app_type == "explorer") {
    # Try this: Ace Explore has demos collected at a separate date/time,
    # so BID will _basically never_ match up. Use PID to bind demos to proc
    demo_merge_col = COL_PID
    out <- out %>%
      mutate(proc = map(proc, ~reconstruct_pid(.x)),
             demos = map(demos, ~select(.x, -!!Q_COL_BID, -!!Q_COL_TIME, -!!Q_COL_FILE)))
  } else {
    demo_merge_col = COL_BID
  }
  
  if (output == "wide") {
    
    out <- out %>%
      select(module, demos, proc) %>%
      mutate(demos = map(demos, ~.x %>%
                           select(-any_of(COL_FILE)) %>%
                           distinct()))
    
    if (app_type == "explorer") {
      out <- out %>%
        mutate(proc = map2(proc, module, ~.x %>%
                             select(!!COL_BID, !!COL_PID, everything()) %>%
                             rename_with(.fn = paste_module_colname,
                                         module = .y,
                                         .cols = -c(!!COL_BID, !!COL_PID))))
    } else {
      out <- out %>%
        mutate(proc = map2(proc, module, ~.x %>%
                             select(!!COL_BID, everything()) %>%
                             rename_with(.fn = paste_module_colname,
                                         module = .y, .cols = -(!!COL_BID))))
    }
    
    if (app_type == "classroom") {
      # do not join module to module by full BID if ACE Classroom data
      # because diff modules from same subj's session have diff timestamps
      # disambiguate full bids from diff modules by prepending module name
      out <- out %>%
        mutate(proc = pmap(list(proc, demos, module), function (a, b, c) {
          full_join(b, a, by = demo_merge_col) %>%
            rename_with(.fn = paste_module_colname,
                        module = c,
                        .cols = any_of(c(COL_BID, COL_TIME)))
        }))
    } else if (app_type == "explorer") {
      # ACE explorer data:
      # join demos to proc with pid
      # BUT, DO join module to module by full bid. now, with times gameplayed as the session identfier,
      # all modules from a single session should have the same times gameplayed stamp
      out <- out %>%
        mutate(proc = pmap(list(proc, demos, module), function (a, b, c) {
          right_join(b, a, by = demo_merge_col) %>%
            return()
        }))
    } else {
      # DO join by full BID if ACE Explorer data
      # bc all modules from one subj's session should have same times-finished-game stamps
      # and for SEA data, we forcibly assume data from one subject are all from one session?
      out <- out %>%
        mutate(proc = map2(proc, demos, ~full_join(.y, .x, by = demo_merge_col)))
    }
    
    valid_demos = get_valid_demos(out$proc[[1]], is_ace)
    
    out$proc %>%
      reduce(dplyr::full_join, by = valid_demos) %>%
      select(valid_demos, everything()) %>%
      return()
    
  } else if (output == "long") {
    out <- out %>%
      select(module, demos, proc) %>%
      mutate(proc = map2(proc, demos, ~full_join(.y, .x, by = demo_merge_col)),
             proc = rlang::set_names(proc, module)) %>%
      select(-demos)
    return (out)
  }
}

#' @keywords internal

check_module_misspelling = function(modules) {
  if (any(!(modules %in% c(ALL_MODULES, ALL_SEA_MODULES)))) {
    warning(crayon::red("Modules improperly specified! Check spelling?"))
    return (TRUE)
  } else {
    return (FALSE)
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
#' @importFrom rlang !! :=

reconstruct_pid <- function (proc) {
  # This SHOULD truncate at the last character before the session-unique portion of the bid
  # safe for time and times finished game bc the period should only appear once
  proc %>% mutate(!!COL_PID := stringr::str_split(!!Q_COL_BID, pattern = "[.]"),
                  !!COL_PID := purrr::map_chr(!!Q_COL_PID, 1L)) %>%
    select(!!COL_BID, !!COL_PID, everything()) %>%
    return()
}

#' @keywords internal

paste_module_colname <- function (col, module) {
  return (paste(toupper(module), col, sep = "."))
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
