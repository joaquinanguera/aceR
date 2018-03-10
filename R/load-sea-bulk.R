
#' Read & Load all SEA csv files in a directory
#'
#' Wrapper function around \code{\link{load_sea_file}()} to read & parse 
#'  all SEA csv files in a directory.
#'
#' @export
#' @importFrom dplyr bind_rows
#' @inheritParams base::list.files
#' @param verbose logical. Print details? Defaults to \code{TRUE}
#' @param recursive logical. Load files in subfolders also? Defaults to \code{TRUE}
#' @param exclude a list of patterns to exclude
#' @return Returns a data.frame containing the content of every file in the
#'  specified \code{path}.

load_sea_bulk <- function(path = ".",
                          verbose = TRUE,
                          recursive = TRUE,
                          exclude = c(),
                          pattern = NULL) {
  csv = list.files(path = path, pattern = ".csv", recursive = recursive)
  files = sort(csv)
  for (ex in exclude) {
    files = filter_out_vec(files, ex)
  }
  if (!is.null(pattern)) {
    files = filter_vec(files, pattern)
  }
  if (length(files) == 0) {
    stop("no matching files", call. = TRUE)
  }
  
  if (path != ".") {
    files = paste(path, files, sep = "/")
  }
  
  # If this can be vectorized, why not? I live for speed
  dat = lapply(files, load_sea_file, verbose = verbose)
  out = dplyr::bind_rows(dat)
  
  # coarse duplicate rejection
  # assumes duplicate rows will be the same in every way, EXCEPT logfile of origin
  out = dplyr::distinct(out, !!! rlang::syms(names(out)[names(out) != COL_FILE]), .keep_all = TRUE)
  
  out = replace_nas(out, "")
  return(out)
}