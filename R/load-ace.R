
#' Reads raw ACE data from a file.
#'
#' Reads, parses, and converts an ACE csv or xls into an R \code{\link{data.frame}}.
#'
#' @export
#' @importFrom purrr map2
#' @importFrom utils read.table read.csv write.csv head tail count.fields
#' 
#' @param file The name of the file which the data is to be read from.
#' @param pid_stem The string stem of the ID in the "PID" field. Defaults to "ADMIN-UCSF-".
#' @return Returns the file's content as an R \code{\link{data.frame}}.

load_ace_file <- function(file, pid_stem = "ADMIN-UCSF-") {
  
  if (is_excel(file)) {
    warning (file, " is Excel format, currently not supported\n")
    return (data.frame())
    # raw_dat = load_excel(file)
  } 
  
  raw_dat = load_csv(file)
  return (transform_raw(file, raw_dat))
}

#' @keywords internal

is_excel <- function (filename) {
  # Remove this if you ever get Excel functionality back!
  return (grepl("xls", filename))
}

#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @keywords internal

transform_raw <- function (file, dat) {
  if (nrow(dat) == 0) return (data.frame())
  # standardize output
  dat = dat %>%
    standardize_names() %>%
    mutate(file = file,
           # for faster performance bc each file should only contain one module
           module = identify_module(file[1])) %>%
    standardize_ace_column_names() %>%
    # force lowercase everything to cover for weird capitalization diffs bw files
    mutate(!!Q_COL_PID := stringr::str_replace_all(tolower(!!Q_COL_PID), "[^a-zA-Z0-9]+", ""),
           # make block id from pid & time
           !!Q_COL_BID := paste(!!Q_COL_PID, !!Q_COL_TIME, sep = ".")) %>%
    standardize_ace_values() %>%
    # make short block id from pid and date only
    mutate(!!Q_COL_BID_SHORT := paste(!!Q_COL_PID,
                                      lubridate::floor_date(!!Q_COL_TIME, unit = "days"),
                                      sep = ".")) %>%
    group_by(!!Q_COL_BID) %>%
    mutate(!!COL_BLOCK_HALF := plyr::mapvalues(make_half_seq(n()), from = c(1, 2), to = c("first_half", "second_half"))) %>%
    ungroup()
  
  # Don't do "half" labeling for demos, which should only have one row per subject
  if (dat$module[1] == DEMOS) {
    dat = dat %>%
      select(-!!COL_BLOCK_HALF)
  }

  # DEPRECATED I THINK: COL_NAME should not be a thing in ACE Explorer
  if (COL_NAME %in% names(dat) & grepl("ADMIN-UCSF", dat[1, COL_PID])) { # this function expects a "name" column by which to do the matching
    dat = remove_nondata_rows_pulvinar(dat)
  }
  return (dat)
}

#' @keywords internal

guess_pid <- function(x) {
  file = basename(x)
  # maybe_pid = stringr::str_extract(file, "^[a-zA-Z0-9]*")
  maybe_pid = unique(na.omit(as.numeric(unlist(strsplit(unlist(file), "[^0-9]+")))))[1]
  return (maybe_pid)
}