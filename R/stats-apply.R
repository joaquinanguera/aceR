
#' @export

apply_stats <- function(x, y, col, FUN, ...){
  z = plyr::ddply(x, y, .fun = function(xx) { 
    yy = xx[ ,col]
    return (FUN(yy, ...))
  })
  ind = (length(y) + 1):length(z)
  names(z)[ind] = sapply(names(z)[ind], function (n) paste(n, col, sep = "_"))
  return(z)
}
