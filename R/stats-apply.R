
#' @keywords internal

apply_stats <- function(x, y, col, FUN, factor = NULL, suffix = "", ...){
  # TODO: rewrite ddply call using data.table or dplyr for speed boost
  if (length(y) > 2) {
    stop("y must be of equal or less than length of 2")
  }
  by_factor = !missing(factor)
  z = plyr::ddply(x, y, .fun = function(xx) { 
    yy = xx[ ,col]
    if (!by_factor) {
      return (FUN(yy, ...))
    } else {
      gg = xx[ ,factor]
      calc = FUN(yy, gg, ...)
      out = as.data.frame(as.list(calc))
      return (out)
    }
  })
  ind = (length(y) + 1):length(z)
  if (suffix != "") suffix = paste0(".", suffix)
  if (by_factor) suffix = ""
  names(z)[ind] = sapply(names(z)[ind], function (n) paste0(col, "_", n, suffix))
  return(z)
}
#' @keywords internal
#' 
apply_stats_dplyr <- function(x, id_var, col, FUN, factors = NULL, suffix = "", transform_dir = "wide", ...) {
  # id_var: name of column containing subject ID
  # col: name of column containing outcome var of interest to be summarized
  # FUN: name of convenience function to apply all the stats
  # factors: char vector of additional task/trial conditions on which to calculate subsetted stats
  if (length(id_var) > 2) {
    stop("y must be of equal or less than length of 2")
  }
  by_factor = !missing(factors)
  if (!by_factor) {
    z = FUN(dplyr::group_by_(x, id_var), col)
    names(z)[2:length(z)] = paste0(col, "_", names(z)[2:length(z)])
    z = dplyr::ungroup(z)
  } else {
    if (length(factors) == 2) {
      if (transform_dir == "wide") {
        z = vector("list", 3)
        # if there are two factors, put out THREE: one just by factor 1, one just by factor 2, and one crossed
        z[[1]] = FUN(dplyr::group_by_(x, id_var, factors[1]), col)
        # need to add the name prefix here so gather can call the columns easily
        names(z[[1]])[3:length(z[[1]])] = paste0(col, "_", names(z[[1]])[3:length(z[[1]])])
        z[[1]] = tidyr::gather(z[[1]], "metric", "value", dplyr::starts_with(col))
        z[[1]] = tidyr::unite_(z[[1]], "key", c("metric", factors[1]), sep = ".")
        z[[1]] = tidyr::spread_(z[[1]], key_col = "key", value_col = "value")
        z[[1]] = dplyr::ungroup(z[[1]])
        
        z[[2]] = FUN(dplyr::group_by_(x, id_var, factors[2]), col)
        names(z[[2]])[3:length(z[[2]])] = paste0(col, "_", names(z[[2]])[3:length(z[[2]])])
        z[[2]] = tidyr::gather(z[[2]], "metric", "value", dplyr::starts_with(col))
        z[[2]] = tidyr::unite_(z[[2]], "key", c("metric", factors[2]), sep = ".")
        z[[2]] = tidyr::spread_(z[[2]], key_col = "key", value_col = "value")
        z[[2]] = dplyr::ungroup(z[[2]])
        
        z[[3]] = FUN(dplyr::group_by_(x, id_var, factors[1], factors[2]), col)
        names(z[[3]])[4:length(z[[3]])] = paste0(col, "_", names(z[[3]])[4:length(z[[3]])])
        z[[3]] = tidyr::unite_(z[[3]], "cond", c(factors[2], factors[1]), sep = ".")
        z[[3]] = tidyr::gather(z[[3]], "metric", "value", dplyr::starts_with(col))
        z[[3]] = tidyr::unite_(z[[3]], "key", c("metric", "cond"), sep = ".")
        z[[3]] = tidyr::spread_(z[[3]], key_col = "key", value_col = "value")
        z[[3]] = dplyr::ungroup(z[[3]])
        
        z = multi_merge(z, by = id_var)
      } else if (transform_dir == "long") {
        # if long, ONLY long by condition/trial type, NOT by the other subsetting factor
        z = vector("list", 2)
        # if there are two factors, put out THREE: one just by factor 1, one just by factor 2, and one crossed
        z[[1]] = FUN(dplyr::group_by_(x, id_var, factors[1]), col)
        # need to add the name prefix here so gather can call the columns easily
        names(z[[1]])[3:length(z[[1]])] = paste0(col, "_", names(z[[1]])[3:length(z[[1]])])
        z[[1]] = dplyr::ungroup(z[[1]])
        
        z[[2]] = FUN(dplyr::group_by_(x, id_var, factors[1], factors[2]), col)
        names(z[[2]])[4:length(z[[2]])] = paste0(col, "_", names(z[[2]])[4:length(z[[2]])])
        z[[2]] = tidyr::gather(z[[2]], "metric", "value", dplyr::starts_with(col))
        z[[2]] = tidyr::unite_(z[[2]], "key", c("metric", factors[2]), sep = ".")
        z[[2]] = tidyr::spread_(z[[2]], key_col = "key", value_col = "value")
        z[[2]] = dplyr::ungroup(z[[2]])
        
        # using join_all here instead of multi_merge bc easily accepts multiple joining vars
        # join across all common vars, we are assuming common named vars are in fact the same measurement
        z = plyr::join_all(z)
      }
    } else {
      # if only one factor, only put out 1
      z = FUN(dplyr::group_by_(x, id_var, factors), col)
      names(z)[3:length(z)] = paste0(col, "_", names(z)[3:length(z)])
      z = tidyr::gather(z, "metric", "value", dplyr::starts_with(col))
      z = tidyr::unite_(z, "key", c("metric", factors), sep = ".")
      z = tidyr::spread_(z, key_col = "key", value_col = "value")
      z = dplyr::ungroup(z)
    }
  }
  
  if (suffix != "") suffix = paste0(".", suffix)
  if (by_factor) suffix = ""
  if (suffix != "") names(z)[2:length(z)] = paste0(names(z)[2:length(z)], suffix)
  return(z)
}

#' @keywords internal

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