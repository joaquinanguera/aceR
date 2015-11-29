
#' @keywords internal

multi_subtract <- function (df, first_pattern, second_pattern, suffix) {
  out = df[stringr:::str_detect(names(df), first_pattern)] - df[stringr:::str_detect(names(df), second_pattern)]
  names(out) = sapply(names(out), function(x) gsub(first_pattern, suffix, x))
  return (out)
}