
#' @keywords internal

subset_by_col <- function(df, col) {
  unique_groups = unique(df[col])[[1]]
  out = list()
  for (i in 1:length(unique_groups)) {
    group = unique_groups[i]
    match = which(df[col] == group)
    out[[i]] = df[match, ]
    # do these inside for loop on df input, not list input, for efficiency
    out[[i]] = replace_nas(out[[i]], "")
    out[[i]] = remove_empty_cols(out[[i]])
  }
  names(out) = unique_groups
  return (out)
}