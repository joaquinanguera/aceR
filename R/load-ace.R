
#' Reads raw ace data from a file.
#'
#' Reads, parses, and converts an ACE csv or xls into an R \code{\link{data.frame}}.
#'
#' @export
#' @param file The name of the file which the data is to be read from.
#' @return Returns the file's content as an R \code{\link{data.frame}}.

load_ace_file <- function(file) {
  # read raw csv file
  if (is_filtered(file)) {
    return (load_ace_filtered_file(file))
  }
  if (is_excel(file)) {
    raw_dat = load_excel(file)
  } else {
    raw_dat = load_csv(file)   
  }
  if (is.vector(raw_dat)) {
    out = data.frame()
    dfs = names(raw_dat)
    for (i in 1:length(dfs)) {
      name = paste(file, dfs[i], sep = "-")
      df = attempt_transform(name, raw_dat[[i]])
      out = plyr::rbind.fill(out, df)
    }
    return (out)
  } else {
    return (attempt_transform(file, raw_dat))
  }
}

#' @keywords internal

load_ace_filtered_file <- function(file) {
  if (is_excel(file)) {
    df = openxlsx::read.xlsx(file, sheet = 1)
  } else {
    df = read.csv(file, header = TRUE, sep = ",")
  }
  cols = names(df)
  pid_col = cols[grep("id", cols)[1]]
  names(df)[names(df) == pid_col] = COL_PID
  names(df) = sapply(names(df), function(x) to_snake_case(x))
  df = standardize_ace_column_names(df)
  df$file = file
  df$module = identify_module(file)
  df = replace_nas(df, "")
  df[, COL_TIME] = df$time_gameplayed_utc
  df[, COL_CONDITION] = df$details
  df[, COL_BID] = paste(df[, COL_PID], df[, COL_TIME])
  return (df)
}

#' @keywords internal

is_filtered <- function (filename) {
  return (grepl("filtered", filename))
}

#' @keywords internal

attempt_transform <- function(file, raw_dat) {
  # transform data to data frame
  df = tryCatch ({
    transformed = transform_raw(file, raw_dat)
    # test if data is usable
    st = paste(transformed, collapse = "")
    if (grepl("}", st)) {
      warning(file, " contains invalid data ")
      return (data.frame())
    }
    # technically a 'valid' file, BUT contains no data.
    if (nrow(transformed) < 2) {
      warning(file, "is valid, but contains no data!")
      return (data.frame())
    }
    return (transformed)
  }, error = function(e) {
    warning("unable to load ", file)
    return (data.frame())
  })
  return (df)
}

#' @keywords internal

transform_raw <- function (file, raw_dat) {
  if (nrow(raw_dat) == 0) return (data.frame())
  # standardize raw data
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
  dat = standardize_ace_column_names(dat)
  cols = names(dat)
  if (!(COL_TIME) %in% cols) {
    # make "time" column from subid & filename if file doesn't contain time
    dat[, COL_TIME] = paste(dat$file, dat[, COL_SUB_ID], sep = ".")
  } else {
    dat[, COL_TIME] = as.vector(dat[, COL_TIME])
  }
  if (COL_PID %in% cols) {
    # make block id from pid & time
    dat[, COL_BID] = paste(dat[, COL_PID], dat[, COL_TIME], sep = ".")
  } else {
    # make block id from file name & time if file doesn't contain PID
    dat[, COL_BID] = paste(dat$file, dat[, COL_TIME], sep = ".")
    dat[, COL_PID] = NA
  }
  dat = standardize_ace_values(dat)
  return (dat)
}