
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

#' Read & Load all ACE csv files in a directory
#'
#' Wrapper function around \code{\link{read_ace_file}()}. to read & parse 
#'  all ACE csv files in a directory.
#'
#' @export
#' @inheritParams base::list.files
#' @param verbose. logical. Print details? Defaults to \code{TRUE}
#' @return Returns a data.frame containing the content of every file in the
#'  specified \code{path}.

load_ace_bulk <- function(path = ".", pattern = NULL, verbose = TRUE, recursive = TRUE) {
  files = list.files(path = path, pattern = pattern, recursive = recursive)
  if (length(files) == 0) {
    stop("no matching files", call. = TRUE)
  }
  valid_files = filter_vec(files, ".csv")
  out = data.frame()
  for (file in valid_files) {
    if (verbose) {
      print(file)
    }
    dat = read_ace_file(file)
    out = plyr::rbind.fill(out, dat)
  }
  out = replace_nas(out, "")
  return(out)
}