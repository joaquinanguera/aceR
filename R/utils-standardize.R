
#' @keywords internal

standardize_names <- function (df) {
  new_names = names(df)
  new_names = tolower(new_names)
  new_names = remove_special_characters(new_names)
  new_names = replace_spaces(new_names, "_")
  return (new_names)
}

#' @keywords internal

remove_special_characters <- function (x) {
  return (gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x))
}

#' @keywords internal

replace_spaces <- function (x, replacement) {
  return (gsub('([[:punct:]])|\\s+', replacement, x))
}

#' @keywords internal

replace_blanks <- function (x, replacement = NA) {
  x[x == ""] = replacement
  return (x)
}

#' @keywords internal

replace_nas <- function(df, replacement) {
  df[is.na(df)] = replacement
  df[df == "N/A"] = replacement # ACE artifact
  return (df)
}

#' @keywords internal

na_locf <- function(df, cols) {
  fill_last = sapply(df[cols], function(x) {
    x[x == ""] = NA
    return (zoo::na.locf(x))
  })
  return(fill_last)
}

#' @keywords internal

multi_gsub <- function(pattern, replacement, x, ...) {
  if (length(pattern) != length(replacement)) {
    stop("pattern and replacement do not have the same length")
  }
  result = x
  for (i in 1:length(pattern)) {
    result = gsub(pattern[i], replacement[i], result, ...)
  }
  return (result)
}