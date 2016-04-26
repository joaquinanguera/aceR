
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

#' Load all files in a directory as a data frame
#'
#' Load all files in a directory as one data frame
#'
#' @section Warning:
#' Use \code{\link{load_ace_bulk}} for raw formatted ACE data.
#'
#' @export
#' @inheritParams base::list.files
#' @return all files in a directory as one data frame

load_files <- function (path = ".", verbose = FALSE, ...) {
  files = list.files(path, ...)
  out = data.frame()
  for (i in files) {
    if (path != ".") {
      file = paste(path, i, sep = "/")
    } else {
      file = i
    }
    if (verbose) {
      print(paste("loading", file, sep = " "))
    }
    df = read.table(file, header = TRUE, sep = ",")
    out = plyr::rbind.fill(out, df)
  }
  return (out)
}