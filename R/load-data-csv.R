
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

#' @export

block_info <- function(dat) {
  first_elements = head(dat[1], n = 10L)[dat[1] == ""]
  raw_file_header = dat[1:length(first_elements), ]
  file_header = remove_empty_cols(raw_file_header)
  colnames(file_header) = NULL
  file_header = as.data.frame(t(file_header))
  names(file_header) = as.character(unlist(file_header[1, ]))
  return (file_header[-1, ])
}