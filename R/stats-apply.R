
#' @keywords internal
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang as_string sym syms quo_name UQ UQS !! !!!
#' @import tidyr

apply_stats <- function(x, id_var, col, FUN, factors = NULL, suffix = "", transform_dir = "wide", ...) {
  # id_var: name of column containing subject ID
  # col: name of column containing outcome var of interest to be summarized
  # FUN: name of convenience function to apply all the stats
  # factors: char vector of additional task/trial conditions on which to calculate subsetted stats
  if (length(id_var) > 2) {
    stop("y must be of equal or less than length of 2")
  }
  
  if (length(col) > 1) {
    # assume that the only FUN that takes a vector for col is the rcs calculator
    col_out = "rcs"
    col_prefix = "rcs."
  } else if (col == "trial_accuracy") {
    # this should catch the dprime & trial counts calculator
    if (identical(FUN, ace_dprime_dplyr)) {
      col_out = "sdt"
      col_prefix = ""
    } else if (identical(FUN, ace_wm_prek_dplyr)) {
      col_out = "k"
      col_prefix = "k."
    }
  } else if (col == "test_delay_window") {
    col_out = "max_delay_time"
    col_prefix = "max_delay_time."
  } else {
    col_out = col
    col_prefix = ""
  }
  
  by_factor = !missing(factors)
  if (!by_factor) {
    z = x %>%
      group_by(!!id_var) %>%
      FUN(col) %>%
      ungroup()
    
  } else {
    if (length(factors) == 2) {
      if (transform_dir == "wide") {
        z = vector("list", 3)
        # if there are two factors, put out THREE: one just by factor 1, one just by factor 2, and one crossed
        z[[1]] = x %>%
          filter_invalid_stats_levels(factors[[1]]) %>% 
          group_by(!!!c(id_var, factors[[1]])) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(id_cols = !!id_var,
                      names_from = !!factors[[1]],
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
        
        z[[2]] = x %>%
          filter_invalid_stats_levels(factors[[2]]) %>% 
          group_by(!!!c(id_var, factors[2])) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(id_cols = !!id_var,
                      names_from = !!factors[[2]],
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
        
        z[[3]] = x %>%
          filter_invalid_stats_levels(factors[[1]]) %>% 
          filter_invalid_stats_levels(factors[[2]]) %>% 
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          ungroup() %>%
          complete(!!!c(id_var, factors)) %>%
          unite(cond, !!!factors[2:1], sep = ".") %>%
          pivot_wider(id_cols = !!id_var,
                      names_from = cond,
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
        
        z = multi_merge(z, by = quo_name(id_var))
        
      } else if (transform_dir == "long") {
        # if long, ONLY long by condition/trial type, NOT by the other subsetting factor
        z = vector("list", 2)
        # if there are two factors, put out THREE: one just by factor 1, one just by factor 2, and one crossed
        z[[1]] = x %>%
          filter_invalid_stats_levels(factors[[1]]) %>% 
          group_by(!!!c(id_var, factors[[1]])) %>%
          FUN(col) %>%
          ungroup()
        
        z[[2]] = x %>%
          filter_invalid_stats_levels(factors[[1]]) %>% 
          filter_invalid_stats_levels(factors[[2]]) %>% 
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(names_from = !!factors[[2]],
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
        
        
        # using full_join here instead of multi_merge bc easily accepts multiple joining vars
        # to silence error, hard-join by ID var and first factor (which is kept long)
        z = purrr::reduce(z, full_join, by = c(as_string(id_var), as_string(factors[[1]])))
      }
    } else {
      if (transform_dir == "wide") {
        # if only one factor, only put out 1
        if (is.list(factors)) factors = factors[[1]]
        z = x %>%
          filter_invalid_stats_levels(factors) %>% 
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(names_from = !!factors,
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
      } else if (transform_dir == "long") {
        z = x %>%
          filter_invalid_stats_levels(factors) %>% 
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          ungroup()
      }
    }
  }
  
  if (by_factor) suffix = ""
  if (suffix != "") z = rename_with(z, ~paste(., suffix, sep = "."), -(!!COL_BID))
  
  return(z)
}

#' @keywords internal
#' @importFrom dplyr filter
#' @importFrom rlang !! as_string

filter_invalid_stats_levels <- function (x, this_factor) {
  this_factor_str <- as_string(this_factor)
  
  if (this_factor_str %in% c(COL_CORRECT_BUTTON, COL_CORRECT_RESPONSE, COL_LATE_RESPONSE, COL_PREV_CORRECT_BUTTON, "cue_rotated")) {
    return (filter(x, !endsWith(!!this_factor, "no_response"), !is.na(!!this_factor), !!this_factor != "prev_NA"))
  } else {
    return (x)
  }
}
