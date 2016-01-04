
#' Read & Load all ACE csv & xls files in a directory
#'
#' Wrapper function around \code{\link{load_ace_file}()} to read & parse 
#'  all ACE csv files in a directory.
#'
#' @export
#' @inheritParams base::list.files
#' @param verbose logical. Print details? Defaults to \code{TRUE}
#' @param exclude a list of patterns to exclude
#' @return Returns a data.frame containing the content of every file in the
#'  specified \code{path}.

load_ace_bulk <- function(path = ".", verbose = TRUE, recursive = TRUE, exclude = c(), pattern = NULL) {
  csv = list.files(path = path, pattern = ".csv", recursive = recursive)
  xls = list.files(path = path, pattern = ".xls", recursive = recursive)
  files = sort(c(csv, xls))
  for (ex in exclude) {
    files = filter_out_vec(files, ex)
  }
  if (!is.null(pattern)) {
    files = filter_vec(files, pattern)
  }
  if (length(files) == 0) {
    stop("no matching files", call. = TRUE)
  }
  out = data.frame()
  for (file in files) {
    if (verbose) {
      print(file)
    }
    dat = load_ace_file(file)
    out = plyr::rbind.fill(out, dat)
  }
  out = replace_nas(out, "")
  return(out)
}