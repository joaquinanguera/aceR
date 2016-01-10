
#' @keywords internal
# TODO: this needs work

multi_fun <- function(df, first_pattern, second_pattern, suffix, FUN) {
  p1 = df[stringr::str_detect(names(df), first_pattern)]
  p2 = df[stringr::str_detect(names(df), second_pattern)]
  indices = seq(1:length(p1))
  out = sapply(indices, function (x) {
    return (apply(data.frame(p1[, x], p2[, x]), 1, FUN))
  })
  if (is.vector(out)) {
    out = as.data.frame(t(out))
  } else {
    out = as.data.frame(out)
  }
  names(out) = sapply(names(p1), function(x) gsub(first_pattern, suffix, x))
  return(out)
}