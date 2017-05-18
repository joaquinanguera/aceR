
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

load_ace_bulk <- function(path = ".",
                          verbose = TRUE,
                          recursive = TRUE,
                          exclude = c(),
                          pattern = NULL,
                          pid_stem = "ADMIN-UCSF-",
                          force_pid_name_match = F,
                          pulvinar = F) {
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
      these_files = sorted_files[[i]]
      existing_pids = NULL
      for (j in 1:length(these_files)) {
        file = these_files[j]
        if (verbose) {
          print(file)
        }
        if (path != ".") {
          file = paste(path, file, sep = "/")
        }
        dat = load_ace_file(file, pulvinar = pulvinar)
        if (force_pid_name_match) {
          # for Ss where PID was incorrectly duplicated from another S but the name is DIFFERENT, repair the PID
          this_pid = unique(dat[, COL_PID])
          this_name = unique(dat[, COL_NAME])
          if (tolower(substr(this_pid, nchar(pid_stem) + 1, nchar(this_pid))) != tolower(this_name)) dat[, COL_PID] = paste0(pid_stem, dat[, COL_NAME])
        }
        
        # coarse duplicate rejection; should only activate in cases where the data is emailed
        # and it can be assumed (for better or worse) every version of the data w/ same PID is same data
        # if the PIDs in the current extracted data are in the previously extracted data, remove the dupe data from the current extract
        if (!pulvinar) dat = remove_email_dupes(dat, existing_pids)
        
        temp_out = plyr::rbind.fill(temp_out, dat)
        existing_pids = c(existing_pids, temp_out[, COL_PID])
        
        if (j %% 50 == 0 | j == length(these_files)) {
          module_out = plyr::rbind.fill(module_out, temp_out)
          temp_out = data.frame()
        }
      }
      out[[i]] = module_out
      module_out = data.frame()
    }
    out = plyr::rbind.fill(out) # again, trying to save rbind.filling for the end in the hella emailed files case
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
      dat = load_ace_file(file, pulvinar = pulvinar)
      out = plyr::rbind.fill(out, dat)
    }
  }
  out = replace_nas(out, "")
  return(out)
}

#' @keywords internal

sort_files_by_module <- function(files, modules = c(BOXED, BRT, FLANKER, SAAT, SPATIAL_SPAN, STROOP, TASK_SWITCH, TNT, BACK_SPATIAL_SPAN, FILTER)) {
  files_nospace = gsub(" ", "", files, fixed = T) # strip spaces from module names for searching ONLY!
  sorted_files = list()
  for (module in modules) {
    sorted_files[[module]] = files[grepl(module, files_nospace, ignore.case = T)] # actually grab unadulterated file names at appropriate indices
    if (module == SPATIAL_SPAN) sorted_files[[module]] = sorted_files[[module]][!grepl(BACK_SPATIAL_SPAN, sorted_files[[module]], ignore.case = T)]
  }
  return (Filter(length, sorted_files)) # remove empty fields
}

#' @keywords internal

remove_email_dupes <- function(dat, existing_pids = c(temp_out[, COL_PID], module_out[, COL_PID])) {
  these_pids = unique(dat[, COL_PID])
  dat = dplyr::filter(dat, !(pid %in% existing_pids))
  return (dat)
}