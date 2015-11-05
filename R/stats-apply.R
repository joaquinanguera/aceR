
#' @keywords internal

apply_stats <- function(x, y, col, FUN, ...){
  z = plyr::ddply(x, y, .fun = function(xx) { 
    yy = xx[ ,col]
    return (FUN(yy, ...))
  })
  ind = (length(y) + 1):length(z)
  names(z)[ind] = sapply(names(z)[ind], function (n) paste(n, col, sep = "_"))
  return(z)
}

#' @keywords internal

apply_stats_transform <- function(proc_list, timevar, idvar, ...) {
  proc_merge = reshape::merge_recurse(proc_list)
  proc_reshaped = reshape::reshape(proc_merge, timevar = timevar, idvar = idvar, direction = "wide", ...) 
  return (proc_reshaped)
}