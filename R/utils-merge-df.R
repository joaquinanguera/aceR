
#' @keywords internal

multi_merge <- function(df_list = list(), ...) {
  return (Reduce(function(x, y) merge(x, y, all = TRUE, ...), df_list))
}

#' @keywords internal

flatten_df_list <- function (df_list = list()) {
  out = data.frame()
  for (df in df_list) {
    out = plyr:::rbind.fill(out, df)
  }
  return (out)
}