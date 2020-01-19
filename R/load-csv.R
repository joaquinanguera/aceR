
#' @keywords internal

load_csv <- function(file, pulvinar = FALSE) {
  if (pulvinar) {
    df = dplyr::as_tibble(data.table::fread(file, header = T, na.strings = c("NA", "N/A", "")))
  } else {
    # Need to use the old school one bc of the fill argument, and probably other stuff
    # The emailed files are so funny shaped
    num_cols = max(count.fields(file, sep = ','), na.rm = TRUE)
    df = read.csv(file, header = FALSE, row.names = NULL, col.names = seq_len(num_cols), fill = TRUE, stringsAsFactors = FALSE)
  }
  return (df)
}

#' @keywords internal

breakup_by_user <- function(raw) {
  subs = which(grepl("USER ID:|PARTICIPANT ID:", raw[, 2])) - 1
  if (length(subs) !=  0) {
    out = split(raw, cumsum(1:nrow(raw) %in% subs))
    out = lapply(out, "rownames<-", NULL)
  } else {
    out = raw
  }
  return (out)
}

#' @keywords internal

standardize_raw_csv_data <- function(dat) {
  dat[is.na(dat)] = ""
  dat = remove_empty_cols(dat)
  dat = remove_empty_rows(dat)
  row.names(dat) = NULL
  return (dat)
}

#' @keywords internal

standardize_session_info <- function(dat) {
  incomplete_rows = identify_incomplete_session_info(dat)
  null_trial_rows = identify_null_trials(dat)
  
  dat[incomplete_rows, 2] = "TIME:"
  dat[null_trial_rows, 1] = "0" # need this to avoid having null trials accidentally detected as new data subsets
  
  return(dat)
}

#' @keywords internal

identify_grouping_rows <- function(dat) {
  matching_rows = dat[dat[1] != "" & dat[2] == "", ]
  return (numeric_row_names(matching_rows))
}

#' @keywords internal

identify_nondata_rows <- function(dat) {
  possible_groups = identify_grouping_rows(dat)
  consec = group_consecutive_integers(possible_groups)
  # for consecutive 'non-data' rows we're assuming the last value is the grouping value and everything is throwaway
  non_data = sapply(consec, function(x) {
    if (length(x) > 1) {
      return (head(x, -1))
    } else {
      return (c())
    }
  })
  throw_away = dat[dat[1] != "" & dat[2] != "" & dat[3] == "", ]
  vals = c(unlist(non_data), numeric_row_names(throw_away))
  vals = sort(unique(vals))
  if (length(vals) < 1) { # MT 9/18/16 changing to < to throw away single rows
    return (c())
  } else {
    return (vals)
  }
}

#' @keywords internal

identify_incomplete_session_info <- function(dat) {
  # for some reason this doesn't return a logical vector if using %in%
  incomplete_info_rows = dat[dat[[1]] == "" & dat[[2]] == "" & dat[[3]] != "" & dat[[3]] != "0" , ] # if 0 is in the 3rd col then it is probably REAL DATA where S failed to respond
  return (numeric_row_names(incomplete_info_rows))
}

#' @keywords internal

identify_null_trials <- function(dat) {
  null_trials = dat[dat[[1]] == "" & dat[[2]] == "" & dat[[3]] == "0" , ] # if 0 is in the 3rd col then it is probably REAL DATA where S failed to respond
  return (numeric_row_names(null_trials))
}

#' @keywords internal

remove_nondata_rows <- function(raw_dat) {
  rows = identify_nondata_rows(raw_dat)
  return (remove_rows(raw_dat, rows))
}

#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom rlang !! :=
#' @importFrom tidyr separate
#' @keywords internal

remove_nondata_rows_pulvinar <- function(dat) {
  # using data.table internally for speed, generally
  # using bare colnames here because we can assume these cols will exist under this name and data.table doesn't like quoted varnames
  # split pid col for comparing short identifier in pid column with identifier in name column
  
  # Do NOT run this function if it's not an adminucsf PID
  if (any(startsWith(dat[[COL_PID]], "adminucsf"))) return(dat)
  
  dat <- dat %>%
    separate(!!Q_COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F) %>%
    mutate(!!COL_NAME := tolower(!!Q_COL_NAME)) %>%
    # keep only those whose PID/name isn't just a 4 digit number (testers)
    # TODO: add back a way to silence warnings?
    filter(is.na(as.numeric(pid_num)),
           is.na(as.numeric(!!Q_COL_NAME)),
           # keep only those that have a 5 character PID where the last 3 chars are numbers
           # also passes through stem-only PIDs (these cannot be fixed here, must be patched later)
           pid_num == "" | (nchar(pid_num) == 5 & !is.na(as.numeric(substr(pid_num, 3, 5)))),
           # scrubbing truly empty datasets (no PID no name)
           !(name == "" & pid_num == ""))
  
  return (select(dat, -pid_stem, -pid_num))
}

#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' #' @importFrom rlang !! :=
#' @keywords internal deprecated

fix_blank_pids <- function(dat) {
  
  # Do NOT run this function if it's not an adminucsf PID
  if (any(startsWith(dat[[COL_PID]], "adminucsf"))) return(dat)
  
  dat = dat %>%
    tidyr::separate_(COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F) %>%
  # repair PIDs for those where PID was left blank but there is something in the name field
    mutate(pid_num = ifelse(!!Q_COL_PID == pid_stem, !!Q_COL_NAME, pid_num),
           !!COL_PID := ifelse(!!Q_COL_PID == pid_stem, paste0(pid_stem, !!Q_COL_NAME), !!Q_COL_PID)) %>%
    select(-c(pid_stem, pid_num))

  return (dat)
}

#' @keywords internal

transform_grouping_rows <- function(dat) {
  rows = identify_grouping_rows(dat)
  new_col = length(dat) + 1
  dat[[new_col]] = NA
  for (row in rows) {
    group = dat[row, 1]
    dat[row + 1, new_col] = "CONDITION:" # so we can identify "new" columns
    dat[row + 2, new_col] = as.character(group)
  }
  dat = remove_rows(dat, rows)
  # In cases where an improper header section is triggered,
  # e.g. task was only run for one of 2 blocked conditions,
  # scrub rows of only NA that are introduced bc of bad grouping column appending
  dat = remove_rows(dat, which(is.na(dat[[1]]) & is.na(dat[[length(dat) - 1]])))
  
  # Removing this to avoid accidentally triggering a new "header" section
  # when the first column of real data has NAs (aka some spatial span cases)
  # dat = replace_nas(dat, "")
  return (dat)
}

#' @keywords internal

transform_session_info <- function(dat) {
  rles = rle(dat[, 1] == "")
  sums = consecutive_sums(rles$lengths)
  max_len = max(rles$lengths[rles$values])
  dat_len = length(dat)
  new_cols = (dat_len + 1) : (dat_len + max_len + 1)
  dat[, new_cols] = NA
  rem = c()
  for (i in 1:length(sums)) {
    is_sec = rles$values[i] == TRUE
    if (is_sec) {
      end = sums[i]
      first = (end - rles$lengths[i]) + 1
      rows = seq(first, end)
      sec_header = dat[rows, c(2, 3)]
      sec_cols = (dat_len + 1) : (dat_len + length(rows))
      dat[end + 1, sec_cols] = as.vector(sec_header[, 1])
      dat[end + 2, sec_cols] = as.vector(sec_header[, 2])
      rem = c(rem, rows)
    }
  }
  dat = remove_rows(dat, rem)
  return (standardize_raw_csv_data(dat))
}

#' @keywords internal

parse_subsections <- function(dat) {
  subs = which(dat[, 1] ==  dat[1, 1])
  len = length(subs)
  out = data.frame()
  new_cols = c()
  for (i in 1:len) {
    header = subs[i]
    body = (header + 1):(ifelse(i == len, nrow(dat), subs[i + 1] - 1))
    sub = as.data.frame(dat[body, ])
    col_names = unname(unlist(dat[header, ]))
    valid_cols = which(col_names != "")
    valid = sub[, valid_cols]
    names(valid) = col_names[valid_cols]
    is_new_col = sapply(names(valid), function(x) return(grepl(":", x)))
    new_cols = c(new_cols, names(valid)[is_new_col])
    clean = remove_empty_rows(replace_blanks(valid, NA))
    clean[[COL_SUB_ID]] = i
    clean[[COL_BLOCK_HALF]] = plyr::mapvalues(make_half_seq(nrow(clean)), from = c(1, 2), to = c("first_half", "second_half"))
    out = plyr::rbind.fill(out, clean)
  }
  new_cols = unique(new_cols)
  out[new_cols] = na_locf(out, new_cols)
  return (out)
}

#' @keywords internal

make_half_seq <- function(num) {
  len = ifelse(num %% 2 == 0, num, num + 1)
  hseq = rep(1:2, len = len, each = len/2)
  return (hseq[1:num])
}