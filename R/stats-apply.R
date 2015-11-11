
#' @keywords internal

apply_stats <- function(x, y, col, FUN, factor = NULL, suffix = "", ...){
  if (length(y) != 2) {
    stop("y must be length of 2")
  }
  by_factor = !missing(factor)
  z = plyr::ddply(x, y, .fun = function(xx) { 
    yy = xx[ ,col]
    if (!by_factor) {
      return (FUN(yy, ...))
    } else {
      gg = xx[ ,factor]
      return (as.data.frame(FUN(yy, gg, ...)))
    }
  })
  ind = (length(y) + 1):length(z)
  if (suffix != "") suffix = paste0(".", suffix)
  if (by_factor) suffix = ""
  names(z)[ind] = sapply(names(z)[ind], function (n) paste0(col, "_", n, suffix))
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