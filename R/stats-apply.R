
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
  by_factor = !missing(factors)
  if (!by_factor) {
    z = x %>%
      group_by(!!id_var) %>%
      FUN(col) %>%
      rename_at(-1, funs(paste0(col, "_", .))) %>%
      ungroup()

  } else {
    if (length(factors) == 2) {
      if (transform_dir == "wide") {
        z = vector("list", 3)
        # if there are two factors, put out THREE: one just by factor 1, one just by factor 2, and one crossed
        z[[1]] = x %>%
          group_by(!!!c(id_var, factors[[1]])) %>%
          FUN(col) %>%
          # need to add the name prefix here so gather can call the columns easily
          rename_at(-(1:2), funs(paste0(col, "_", .))) %>%
          ungroup() %>%
          super_spread(!!factors[[1]], starts_with(!!col), name_order = "value_first", sep = ".")
        
        z[[2]] = x %>%
          group_by(!!!c(id_var, factors[2])) %>%
          FUN(col) %>%
          # need to add the name prefix here so gather can call the columns easily
          rename_at(-(1:2), funs(paste0(col, "_", .))) %>%
          ungroup() %>%
          super_spread(!!factors[[2]], starts_with(!!col), name_order = "value_first", sep = ".")
        
        z[[3]] = x %>%
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          rename_at(-(1:3), funs(paste0(col, "_", .))) %>%
          ungroup() %>%
          complete(!!!c(id_var, factors)) %>%
          unite(cond, !!!factors[2:1], sep = ".") %>%
          super_spread(cond, starts_with(!!col), name_order = "value_first", sep = ".")
        
        z = multi_merge(z, by = quo_name(id_var))
        
      } else if (transform_dir == "long") {
        # if long, ONLY long by condition/trial type, NOT by the other subsetting factor
        z = vector("list", 2)
        # if there are two factors, put out THREE: one just by factor 1, one just by factor 2, and one crossed
        z[[1]] = x %>%
          group_by(!!!c(id_var, factors[[1]])) %>%
          FUN(col) %>%
          rename_at(-(1:2), funs(paste0(col, "_", .))) %>%
          ungroup()
        
        z[[2]] = x %>%
          group_by(!!!c(id_var, factors)) %>%
          FUN(col) %>%
          rename_at(-(1:3), funs(paste0(col, "_", .))) %>%
          gather("metric", "value", starts_with(!!col)) %>%
          unite("key", !!!c(sym("metric"), factors[[2]]), sep = ".") %>%
          spread(key = "key", value = "value") %>%
          ungroup()
        
        # using join_all here instead of multi_merge bc easily accepts multiple joining vars
        # join across all common vars, we are assuming common named vars are in fact the same measurement
        z = plyr::join_all(z)
      }
    } else {
      # if only one factor, only put out 1
      z = x %>%
        group_by(!!!c(id_var, factors)) %>%
        FUN(col) %>%
        rename_at(-(1:2), funs(paste0(col, "_", .))) %>%
        ungroup() %>%
        super_spread(!!factors, starts_with(!!col), name_order = "value_first", sep = ".")
    }
  }
  
  if (by_factor) suffix = ""
  if (suffix != "") z = rename_at(z, -1, funs(paste(., suffix, sep = ".")))

  return(z)
}

#' @keywords internal deprecated

apply_stats_transform <- function(proc_list, timevar, idvar, ...) {
  proc_merge = reshape::merge_recurse(proc_list)
  proc_reshaped = stats::reshape(proc_merge, timevar = timevar, idvar = idvar, direction = "wide", ...) 
  return (proc_reshaped)
}

#' @keywords internal

ace_apply_by_group <- function(x, y, FUN) {
  group = replace_blanks(y, NA)
  agg = aggregate(list(x), list(group), FUN = FUN, simplify = TRUE)
  out = data.frame(t(agg[2]))
  names(out) = unlist(agg[1])
  row.names(out) <- NULL
  return (out)
}

