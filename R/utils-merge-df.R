
#' @keywords internal

multi_merge <- function(df_list = list(), ...) {
  return (Reduce(function(x, y) merge(x, y, all = TRUE, ...), df_list))
}
