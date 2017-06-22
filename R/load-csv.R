
#' @keywords internal

load_csv <- function(file, pulvinar = FALSE) {
  if (pulvinar) {
    df = as.data.frame(data.table::fread(file, header = T))
  } else {
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
  for (row in incomplete_rows) {
    dat[row, 2] = "TIME:"
  }
  for (row in null_trial_rows) {
    dat[row, 1] = "0" # need this to avoid having null trials accidentally detected as new data subsets
  }
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
  incomplete_info_rows = dat[dat[1] == "" & dat[2] == "" & dat[3] != "" & dat[3] != "0" , ] # if 0 is in the 3rd col then it is probably REAL DATA where S failed to respond
  return (numeric_row_names(incomplete_info_rows))
}

#' @keywords internal

identify_null_trials <- function(dat) {
  null_trials = dat[dat[1] == "" & dat[2] == "" & dat[3] == "0" , ] # if 0 is in the 3rd col then it is probably REAL DATA where S failed to respond
  return (numeric_row_names(null_trials))
}

#' @keywords internal

remove_nondata_rows <- function(raw_dat) {
  rows = identify_nondata_rows(raw_dat)
  return (remove_rows(raw_dat, rows))
}

#' @keywords internal

remove_nondata_rows_pulvinar <- function(dat) {
  # using data.table internally for speed & reduction of with() calls
  # using bare colnames here because we can assume these cols will exist under this name and data.table doesn't like quoted varnames
  # split pid col for comparing short identifier in pid column with identifier in name column
  dat = tidyr::separate_(dat, COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F)
  dat[, name := tolower(name)]
  # keep only those whose PID/name isn't just a 4 digit number (testers)
  suppressWarnings({dat = dat[is.na(as.numeric(pid_num))]})
  suppressWarnings({dat = dat[is.na(as.numeric(name))]})
  # keep only those that have a 5 character PID where the last 3 chars are numbers
  # also passes through stem-only PIDs (these cannot be fixed here, must be patched later)
  suppressWarnings({dat = dat[pid_num == "" | (nchar(pid_num) == 5 & !is.na(as.numeric(substr(pid_num, 3, 5))))]})
  # return data.table object (since will only be used internally to another data.table using function)
  dat = dat[!(name == "" & pid_num == "")] # scrubbing truly empty datasets (no PID no name)
  return (dat[, c("pid_stem", "pid_num") := NULL])
}

#' @keywords internal
#' 
fix_blank_pids <- function(dat) {
  dat = as.data.table(tidyr::separate_(dat, COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F))
  # repair PIDs for those where PID was left blank but there is something in the name field
  dat[, pid_num := ifelse(pid == pid_stem, name, pid_num)]
  dat[, pid := ifelse(pid == pid_stem, paste0(pid_stem, name), pid)]
  return (dat[, c("pid_stem", "pid_num") := NULL])
}

#' @keywords internal

transform_grouping_rows <- function(dat) {
  rows = identify_grouping_rows(dat)
  new_col = length(dat) + 1
  dat[new_col] = NA
  for (row in rows) {
    group = dat[row, 1]
    dat[row + 1, new_col] = "CONDITION:" # so we can identify "new" columns
    dat[row + 2, new_col] = as.character(group)
  }
  dat = remove_rows(dat, rows)
  dat = replace_nas(dat, "")
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
    clean[, COL_SUB_ID] = i
    clean[, COL_BLOCK_HALF] = plyr::mapvalues(make_half_seq(nrow(clean)), from = c(1, 2), to = c("first_half", "second_half"))
    out = plyr::rbind.fill(out, clean)
  }
  new_cols = unique(new_cols)
  out[new_cols] = na_locf(out, new_cols)
  return (out)
}

#' @keywords internal

parse_subsections_pulvinar <- function(dat) {
  subs = unique(dat[, COL_PID]) # use "subid" bc is unique for each data submission (2 submissions by same PID will have diff subids)
  dat = data.table::as.data.table(dat)
  len = length(subs)
  out = data.frame()
  if ("name" %in% names(dat) & grepl("ADMIN-UCSF", dat[1, COL_PID])) { # this function expects a "name" column by which to do the matching
    dat = remove_nondata_rows_pulvinar(dat)
  }
  # STRICT DATASET REJECTION HERE, too many submitted datasets where pid and name don't match, so ONLY including ones where they do bc can't be confident about demographic data otherwise
  cat("Stage 1 PID search: only matching datasets\n")
  pb = txtProgressBar(min = 0, max = len, style = 3) # progress bar for this for loop
  for (i in 1:len) {
    sub = subs[i]
    if ("name" %in% names(dat)) {
      clean = dat[pid == sub & name == substr(sub, 12, 17), ] # grab only data where name and pid match, for now
    }
    clean = dat[pid == sub, ] # currently data doesn't include the name field
    # TODO: here is where automatic detection of pre and post can be implemented
    for (this_time in unique(clean[, COL_TIME])) {
      this_clean = clean[COL_TIME == this_time, ] # in this subset, delimit by time gameplayed to collect pre and post data
      if (nrow(this_clean) > 0) {
        this_clean[, COL_BLOCK_HALF] = plyr::mapvalues(make_half_seq(nrow(this_clean)), from = c(1, 2), to = c("first_half", "second_half"))
      }
      out = plyr::rbind.fill(out, this_clean)
    }
    # } else if (!(clean[1, COL_PID] %in% unique(out[, COL_PID]))) { # in general, only append sub if not duplicate
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if ("name" %in% names(dat) & nrow(dat[pid == "ADMIN-UCSF-"]) > 0) { # can only run if there ARE blank PIDs and a name column
    # stage 2: patch blank PIDs, then append any subs where they ONLY existed as a blank pid
    cat("Stage 2 PID search: empty PID datasets\n")
    dat_empty_pid = dat[pid == "ADMIN-UCSF-"]
    dat_empty_pid = fix_blank_pids(dat_empty_pid)
    subs = unique(dat_empty_pid[, pid])
    len = length(subs)
    pb = txtProgressBar(min = 0, max = len, style = 3) # progress bar for this for loop
    for (i in 1:len) {
      sub = subs[i]
      clean = dat_empty_pid[pid == sub & name == substr(sub, 12, 17), ] # grab only data where name and pid match, for now
      for (this_time in unique(clean[, COL_TIME])) {
        this_clean = clean[COL_TIME == this_time, ] # in this subset, delimit by time gameplayed to collect pre and post data
        if (nrow(this_clean) > 0) {
          this_clean[, COL_BLOCK_HALF] = plyr::mapvalues(make_half_seq(nrow(this_clean)), from = c(1, 2), to = c("first_half", "second_half"))
        }
        if (!(clean[1, COL_PID] %in% unique(out[, COL_PID]))) { # in general, only append sub if not duplicate
          out = plyr::rbind.fill(out, clean)
        }
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  
  if ("name" %in% names(dat)) { # can only run if there is a name column
    # stage 3: append any subs where the PID ONLY exists w/ mismatched name
    # and said name also exists w/ matched PID
    cat("Stage 3 PID search: orphaned mis-named PID datasets level 1\n")
    clean_subnames = unique(out[, COL_NAME]) # recording subs who survived at stage 1 & 2 for comparison in stage 3
    dat_mismatched_pid = tidyr::separate_(dat, COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F)
    dat_mismatched_pid[, name := tolower(name)]
    dat_mismatched_pid = dat_mismatched_pid[pid_num != name]
    subs = unique(dat_mismatched_pid[, pid])
    len = length(subs)
    if (len > 0) {
      pb = txtProgressBar(min = 0, max = len, style = 3) # progress bar for this for loop
      for (i in 1:len) {
        sub = subs[i]
        clean = dat_mismatched_pid[pid == sub & name %in% clean_subnames] # grab only data where name matches an already matched PID
        for (this_time in unique(clean[, COL_TIME])) {
          this_clean = clean[COL_TIME == this_time, ] # in this subset, delimit by time gameplayed to collect pre and post data
          if (nrow(this_clean) > 0) {
            this_clean[, COL_BLOCK_HALF] = plyr::mapvalues(make_half_seq(nrow(this_clean)), from = c(1, 2), to = c("first_half", "second_half"))
          }
          if (!(clean[1, COL_PID] %in% unique(out[, COL_PID]))) { # in general, only append sub if not duplicate
            out = plyr::rbind.fill(out, clean)
          }
        }
        setTxtProgressBar(pb, i)
      }
      close(pb)
    }
    
    # stage 4: append any subs where the PID ONLY exists w/ mismatched name
    # and said name also exists w/ a stage 3 orphan-matched PID
    cat("Stage 4 PID search: orphaned mis-named PID datasets level 2\n")
    clean_subnames = unique(out[, COL_NAME]) # recording subs who survived at stage 1-3 for comparison in stage 3
    dat_mismatched_pid = tidyr::separate_(dat, COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F)
    dat_mismatched_pid[, name := tolower(name)]
    dat_mismatched_pid = dat_mismatched_pid[pid_num != name]
    subs = unique(dat_mismatched_pid[, pid])
    len = length(subs)
    if (len > 0) {
      pb = txtProgressBar(min = 0, max = len, style = 3) # progress bar for this for loop
      for (i in 1:len) {
        sub = subs[i]
        clean = dat_mismatched_pid[pid == sub & name %in% clean_subnames] # grab only data where name matches an already matched PID
        for (this_time in unique(clean[, COL_TIME])) {
          this_clean = clean[COL_TIME == this_time, ] # in this subset, delimit by time gameplayed to collect pre and post data
          if (nrow(this_clean) > 0) {
            this_clean[, COL_BLOCK_HALF] = plyr::mapvalues(make_half_seq(nrow(this_clean)), from = c(1, 2), to = c("first_half", "second_half"))
          }
          if (!(clean[1, COL_PID] %in% unique(out[, COL_PID]))) { # in general, only append sub if not duplicate
            out = plyr::rbind.fill(out, clean)
          }
        }
        setTxtProgressBar(pb, i)
      }
      close(pb)
    }
    dat[, c("pid_stem", "pid_num") := NULL]
  }
  return (as.data.frame(out))
}

#' @keywords internal

make_half_seq <- function(num) {
  len = ifelse(num %% 2 == 0, num, num + 1)
  hseq = rep(1:2, len = len, each = len/2)
  return (hseq[1:num])
}