
#' @keywords internal

to_numeric <- function(x) {
  vals = plyr::mapvalues(x, from = c("incorrect", "correct"), to = c(0, 1), warn_missing = FALSE)
  return(as.numeric(as.character(vals)))
}