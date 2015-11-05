
#' Read raw csv data
#'
#' Reads, parses, and converts an ACE csv into an R \code{\link{data.frame}}.
#'
#' @export
#' @param file The name of the file which the data is to be read from.
#' @return Returns the file's content as an R \code{\link{data.frame}}.

read_raw_csv <- function(file) {
  # read raw csv file
  num_cols = max(count.fields(file, sep = ','))
  raw_dat = read.csv(file, header = FALSE, row.names = NULL, col.names = seq_len(num_cols), fill = TRUE, stringsAsFactors = FALSE)
  # standardize raw csv data
  raw_dat = standardize_raw_csv_data(raw_dat)
  # remove nondata rows
  dat = remove_nondata_rows(raw_dat)
  # move grouping rows into column
  dat = transform_grouping_rows(dat)
  # standardize session info
  dat = standardize_session_info(dat)
  # transform session info into columns
  dat = transform_session_info(dat)
  # parse subsections
  dat = parse_subsections(dat)
  # standardize output
  names(dat) = standardize_names(dat)
  dat$file = file
  dat$module = identify_module(file)
  return (dat)
}

#' @keywords internal

standardize_raw_csv_data <- function(dat) {
  dat[is.na(dat)] = ""
  dat = remove_empty_cols(dat)
  dat = remove_empty_rows(dat)
  return (dat)
}

#' @keywords internal

standardize_session_info <- function(dat) {
  incomplete_rows = identify_incomplete_session_info(dat)
  for (row in incomplete_rows) {
    dat[row, 2] = "TIME:"
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
  grouping = identify_grouping_rows(dat)
  nondata = dat[dat[1] != "" & dat[2] != "" & dat[3] == "", ]
  vals = c(grouping, numeric_row_names(nondata))
  vals = sort(unique(vals))
  if (length(vals) == 0) {
    return (c())
  }
  num = length(vals) - 1
  out = c()
  for (i in 1:num) {
    n = vals[i]
    m = vals[i + 1]
    if (diff(c(n, m)) == 1) {
      out = c(out, c(n, m))
    }
  }
  return (unique(out))
}

#' @keywords internal

identify_incomplete_session_info <- function(dat) {
  incomplete_info_rows = dat[dat[1] == "" & dat[2] == "" & dat[3] != "", ]
  return (numeric_row_names(incomplete_info_rows))
}

#' @keywords internal

remove_nondata_rows <- function(raw_dat) {
  rows = identify_nondata_rows(raw_dat)
  return (remove_rows(raw_dat, rows))
}

#' @keywords internal

transform_grouping_rows <- function(dat) {
  rows = identify_grouping_rows(dat)
  new_col = length(dat) + 1
  dat[new_col] = NA
  for (row in rows) {
    group = dat[row, 1]
    dat[row + 1, new_col] = "GROUP:" # so we can identify "new" columns
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
    valid$subid = i
    out = plyr::rbind.fill(out, valid)
  }
  new_cols = unique(new_cols)
  out[new_cols] = na_locf(out, new_cols)
  return (out)
}