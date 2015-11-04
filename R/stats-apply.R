
#' @export

apply_stats <- function(x, y, col, FUN, ...){
  z = plyr::ddply(x, y, .fun = function(xx) { 
    yy = xx[ ,col]
    return (FUN(yy, ...))
  })
  return(z)
}
