
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
  # out =
  if (length(files) > 50) {
    out = list()
    module_out = data.frame()
    temp_out = data.frame()
    sorted_files = sort_files_by_module(files) # files LISTED by module type
    for (i in 1:length(sorted_files)) {
      for (j in 1:length(sorted_files[[i]])) {
        file = sorted_files[[i]][j]
        if (verbose) {
          print(file)
        }
        if (path != ".") {
          file = paste(path, file, sep = "/")
        }
        dat = load_ace_file(file)
        temp_out = plyr::rbind.fill(temp_out, dat)
        if (i %% 50 == 0 | i == length(files)) {
          module_out = plyr::rbind.fill(module_out, temp_out)
          temp_out = data.frame()
        }
      }
      out[[i]] = module_out
    }
    out = rbind.fill(out) # again, trying to save rbind.filling for the end in the hella emailed files case
  } else {
    out = data.frame()
    for (i in 1:length(files)) {
      file = files[i]
      if (verbose) {
        print(file)
      }
      if (path != ".") {
        file = paste(path, file, sep = "/")
      }
      dat = load_ace_file(file)
      out = plyr::rbind.fill(out, dat)
    }
  }
  out = replace_nas(out, "")
  return(out)
}

#' @keywords internal

sort_files_by_module <- function(files, modules = c(BOXED, BRT, FLANKER, SAAT, SPATIAL_SPAN, STROOP, TASK_SWITCH, TNT)) {
  sorted_files = list()
  for (module in modules) {
    sorted_files[[module]] = files[grepl(module, files, ignore.case = T)]
  }
  return (sorted_files)
}