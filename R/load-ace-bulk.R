
#' List the files in a directory/folder
#'
#' Wrapper around \code{\link{list.files}()}
#'
#' @export
#' @inheritParams base::list.files
#' @return Returns a character vector containing the names of the files 
#'   in the specified directory.

files_in_directory <- function(path = ".", pattern = ".csv", recursive = TRUE) {
  files = list.files(path = path, pattern = pattern, recursive = recursive)
  return (files)
}

#' Read & Load all ACE csv & xls files in a directory
#'
#' Wrapper function around \code{\link{read_ace_file}()}. to read & parse 
#'  all ACE csv files in a directory.
#'
#' @export
#' @inheritParams base::list.files
#' @param verbose. logical. Print details? Defaults to \code{TRUE}
#' @return Returns a data.frame containing the content of every file in the
#'  specified \code{path}.

load_ace_bulk <- function(path = ".", verbose = TRUE, recursive = TRUE) {
  csv = list.files(path = path, pattern = ".csv", recursive = recursive)
  xls = list.files(path = path, pattern = ".xls", recursive = recursive)
  files = sort(c(csv, xls))
  if (length(files) == 0) {
    stop("no matching files", call. = TRUE)
  }
  out = data.frame()
  for (file in files) {
    if (verbose) {
      print(file)
    }
    dat = read_ace_file(file)
    out = plyr::rbind.fill(out, dat)
  }
  out = replace_nas(out, "")
  return(out)
}