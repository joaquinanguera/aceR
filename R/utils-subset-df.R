
#' @keywords internal

subset_by_col <- function(df, col) {
  unique_groups = unique(df[col])[[1]]
  out = list()
  for (i in 1:length(unique_groups)) {
    group = unique_groups[i]
    match = which(df[col] == group)
    out[[i]] = df[match, ]
    out[[i]] = replace_nas(out[[i]], "")
    out[[i]] = remove_empty_cols(out[[i]])
  }
  names(out) = unique_groups
  # TODO: improve efficiency of replace_nas, this is driving speed decrement of entire function
  # out = replace_nas(out, "")
  # out = remove_empty_cols(out)
  return (out)
}