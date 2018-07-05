
#' @keywords internal

multi_merge <- function(df_list = list(), ...) {
  return (Reduce(function(x, y) merge(x, y, all = TRUE, ...), df_list))
}

#' @importFrom dplyr funs rename_all
#' @importFrom purrr map2
#' @keywords internal

flatten_df_list <- function (df_list = list(), keep_prefix = FALSE) {
  if (keep_prefix) {
    df_list = map2(df_list, names(df_list), ~rename_all(.x, funs(paste(.y, ., sep = "."))))
  }
  return (plyr::rbind.fill(df_list))
}