
#' @keywords internal

multi_merge <- function(df_list = list(), ...) {
  return (Reduce(function(x, y) merge(x, y, all = TRUE, ...), df_list))
}

#' @keywords internal

flatten_df_list <- function (df_list = list(), keep_prefix = FALSE) {
  out = data.frame()
  df_names = names(df_list)
  for (i in 1:length(df_list)) {
    df = df_list[[i]]
    if (keep_prefix) {
      names(df) = sapply(names(df), function(x) {
        paste(df_names[i], x, sep = ".")
      })
    }
    out = plyr::rbind.fill(out, df)
  }
  return (out)
}