
#' @keywords internal

load_csv <- function(file) {
  df = dplyr::as_tibble(data.table::fread(file, header = T, na.strings = c("NA", "N/A", "")))
  return (df)
}

#' Because the "Reading Fluency" module has some cells with unquoted commas,
#' They are causing the usual read.csv delimiter guessing to split
#' one cell into two, creating too many columns for just a few rows
#' 
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim
#' @importFrom utils read.table
#' @keywords internal

load_sea_csv <- function(file) {
  # this quote arg SHOULD work to exclude apostrophes but allow double quotes
  dat <- read.table(file, sep = ",", quote = "\"", header = F, stringsAsFactors = F,
                    col.names = paste0("V", 1:32), fill = T) %>%
    # first, remove any "extra" cols (dat is loaded in with padding cols if necessary)
    remove_empty_cols()
  
  # this returns true for cols which do NOT have a header, but DO have data in some rows
  # if ANY of these are true, these are the cols which need to get pushed back into place
  bad_cols <- purrr::map_lgl(dat, ~(is.na(.[1]) | .[1] == "") & !(all(unique(.) == "") | all(is.na(.))))
  
  # if none of bad_cols are true, no preprocessing necessary
  if (all(!bad_cols)) {
    names(dat) <- str_trim(dat[1, ])
    # need second JUST IN CASE some of them have duplicated cols (e.g. "Semester")
    dat <- dat[2:nrow(dat), !duplicated(colnames(dat))]
    return (dat)
  }
  
  names(dat) <- c(str_trim(dat[1, !bad_cols]), paste0("junk", 1:sum(bad_cols)))
  dat <- dat[2:nrow(dat), !duplicated(colnames(dat))]
  
  if (!("junk2" %in% names(dat))) {
    # if only one extra col of bad data
    bad_rows <- which(dat$junk1 != "")
    dat[bad_rows, "Question Text"] <- paste(dat[bad_rows, "Question Text"],
                                            dat[bad_rows, "Question Type"], sep = ",")
    # Moving these bad rows' data after the bad delimiter one column to the left
    dat[bad_rows, 6:(length(dat) - 1)] <- dat[bad_rows, 7:length(dat)]
    # removing bad cols
    dat <- dat[, 1:(length(dat) - 1)]
  } else {
    # if two extra cols of bad data
    # need this because future stuff expects the empty cols to be empty strings, not NAs
    dat$junk2 <- dplyr::coalesce(as.character(dat$junk2), "")
    bad_rows1 <- which(dat$junk1 != "" & dat$junk2 == "")
    dat[bad_rows1, "Question Text"] <- paste(dat[bad_rows1, "Question Text"],
                                             dat[bad_rows1, "Question Type"], sep = ",")
    # Moving these bad rows' data after the bad delimiter one column to the left
    dat[bad_rows1, 6:(length(dat) - 2)] <- dat[bad_rows1, 7:(length(dat) - 1)]
    
    bad_rows2 <- which(dat$junk2 != "")
    dat[bad_rows2, "Question Text"] <- paste(dat[bad_rows2, "Question Text"],
                                             dat[bad_rows2, "Question Type"],
                                             dat[bad_rows2, "User Answer"], sep = ",")
    # Moving these bad rows' data after the bad delimiters two columns to the left
    dat[bad_rows2, 6:(length(dat) - 2)] <- dat[bad_rows2, 8:length(dat)]
    # removing bad cols
    dat <- dat[, 1:(length(dat) - 2)]
  }
  return (dat)
}

#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom rlang !!
#' @importFrom tidyr separate
#' @keywords internal deprecated

remove_nondata_rows_pulvinar <- function(dat) {
  # using data.table internally for speed, generally
  # using bare colnames here because we can assume these cols will exist under this name and data.table doesn't like quoted varnames
  # split pid col for comparing short identifier in pid column with identifier in name column
  dat <- dat %>%
    separate(!!Q_COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F) %>%
    mutate(name = tolower(name)) %>%
    # keep only those whose PID/name isn't just a 4 digit number (testers)
    # TODO: add back a way to silence warnings?
    filter(is.na(as.numeric(pid_num)),
           is.na(as.numeric(name)),
           # keep only those that have a 5 character PID where the last 3 chars are numbers
           # also passes through stem-only PIDs (these cannot be fixed here, must be patched later)
           pid_num == "" | (nchar(pid_num) == 5 & !is.na(as.numeric(substr(pid_num, 3, 5)))),
           # scrubbing truly empty datasets (no PID no name)
           !(name == "" & pid_num == ""))
  
  return (select(dat, -pid_stem, -pid_num))
}

#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @keywords internal deprecated

fix_blank_pids <- function(dat) {
  dat = dat %>%
    tidyr::separate_(COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F) %>%
  # repair PIDs for those where PID was left blank but there is something in the name field
    mutate(pid_num = ifelse(pid == pid_stem, name, pid_num),
           pid = ifelse(pid == pid_stem, paste0(pid_stem, name), pid)) %>%
    select(-c(pid_stem, pid_num))

  return (dat)
}

#' @keywords internal deprecated

make_half_seq <- function(num) {
  len = ifelse(num %% 2 == 0, num, num + 1)
  hseq = rep(1:2, len = len, each = len/2)
  return (hseq[1:num])
}