
#' @keywords internal
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang sym syms quo_name UQ UQS !! !!!
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
    # this should catch the dprime calculator
    col_out = "dprime"
    col_prefix = "dprime."
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
          group_by(!!!c(id_var, factors[[1]])) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(id_cols = !!id_var,
                      names_from = !!factors[[1]],
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
        
        z[[2]] = x %>%
          group_by(!!!c(id_var, factors[2])) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(id_cols = !!id_var,
                      names_from = !!factors[[2]],
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
        
        z[[3]] = x %>%
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
          group_by(!!!c(id_var, factors[[1]])) %>%
          FUN(col) %>%
          ungroup()
        
        z[[2]] = x %>%
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(names_from = !!factors[[2]],
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
        
        
        # using join_all here instead of multi_merge bc easily accepts multiple joining vars
        # join across all common vars, we are assuming common named vars are in fact the same measurement
        z = plyr::join_all(z)
      }
    } else {
      if (transform_dir == "wide") {
        # if only one factor, only put out 1
        if (is.list(factors)) factors = factors[[1]]
        z = x %>%
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          ungroup() %>%
          pivot_wider(names_from = !!factors,
                      values_from = starts_with(!!col_out),
                      names_prefix = col_prefix,
                      names_sep = ".")
      } else if (transform_dir == "long") {
        z = x %>%
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

#' @keywords internal deprecated

ace_apply_by_group <- function(x, y, FUN) {
  group = replace_blanks(y, NA)
  agg = aggregate(list(x), list(group), FUN = FUN, simplify = TRUE)
  out = data.frame(t(agg[2]))
  names(out) = unlist(agg[1])
  row.names(out) <- NULL
  return (out)
}
