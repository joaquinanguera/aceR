
#' @keywords internal

standardize_names <- function (df) {
  new_names = names(df)
  new_names = tolower(new_names)
  new_names = remove_special_characters(new_names)
  new_names = replace_spaces(new_names, "_")
  return (new_names)
}

#'  Replaces empty values in one column with the values of another column
#'
#' @keywords internal

replace_empty_values <- function(df, column, replacement) {
  if (replacement %in% colnames(df)) {
    missing_vals = unique(c(which(is.na(df[, column])), which(df[, column] == "")))
    if (length(missing_vals) > 0) {
      df[missing_vals, column] = df[missing_vals, replacement]
    }
  }
  return (df)
}

#' @keywords internal

remove_special_characters <- function (x, repacement = "") {
  return (gsub("[^[:alnum:][:blank:]+?&/\\-]", repacement, x))
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
to_snake_case <- function (x) {
  sc = gsub("([a-z])([A-Z])", "\\1_\\L\\2", x, perl = TRUE)
  return (sub("^(.[a-z])", "\\L\\1", sc, perl = TRUE))
}

#' @keywords internal

to_title_case <- function(x) {
  cap = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
  return (cap)
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

#' @keywords internal

first_number <- function(x) {
  return (ifelse(is.na(x), NA, str_extract(x, "(^|\\s)([0-9]+)($|\\s)")))
}

#' @keywords internal

remove_whitespace <- function(x) {
  return (ifelse(is.na(x), NA, gsub("[[:space:]]", "", x)))
}

