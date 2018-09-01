
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
#' @param pulvinar logical. Expect raw data in Pulvinar format? Defaults to \code{FALSE}
#' @return Returns the file's content as an R \code{\link{data.frame}}.

load_ace_file <- function(file, pid_stem = "ADMIN-UCSF-", pulvinar = FALSE) {
  # read raw csv file
  if (is_filtered(file)) {
    return (load_ace_filtered_file(file))
  }
  if (is_excel(file)) {
    warning (file, " is Excel format, currently not supported ")
    return (data.frame())
    # raw_dat = load_excel(file)
  } 
  if (is_pulvinar(file) | pulvinar) { # TODO: can we get a common phrase in all pulvinar export filenames? 
    raw_dat = load_csv(file, pulvinar = T)
    return (transform_pulvinar(file, raw_dat))
  } else if (!is_excel(file)) { # only if it hasn't already been loaded
    raw_dat = load_csv(file)
    raw_dat = breakup_by_user(raw_dat)
  }
  if (is.vector(raw_dat)) {
    ortho_names = paste(file, names(raw_dat), sep = "-")
    dfs = map2(ortho_names, raw_dat, ~attempt_transform(.x, .y))
    out = plyr::rbind.fill(dfs)
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
  df[, COL_PID] = as.character(df[, COL_PID])
  df$file = file
  df$module = identify_module(file)
  df = standardize_ace_values(df)
  df = replace_nas(df, "")
  df[, COL_TIME] = df$time_gameplayed_utc
  df[, COL_CONDITION] = df$details
  by_block = tryCatch({
    df[, COL_BID] = paste(df[, COL_PID], df[, COL_TIME])
    return (add_block_half(subset_by_col(df, "bid")))
  }, error = function(e) {
    return (add_block_half(subset_by_col(df, "pid")))
  })
  out = flatten_df_list(by_block)
  return (out)
}

#' @keywords internal

add_block_half = function(x) {
  df_list = lapply(x, function(x) {
    df = x
    df[, COL_BLOCK_HALF] =  plyr::mapvalues(make_half_seq(nrow(x)), from = c(1, 2), to = c("first_half", "second_half"))
    return(df)
  })
  return (df_list)
}

#' @keywords internal

is_filtered <- function (filename) {
  return (grepl("filtered", filename))
}

#' @keywords internal

is_excel <- function (filename) {
  # Remove this if you ever get Excel functionality back!
  return (grepl("xls", filename))
}

#' @keywords internal

is_pulvinar <- function (filename) {
  return (grepl("pulvinar", filename, ignore.case = T))
}

#' @keywords internal

attempt_transform <- function(file, raw_dat) {
  # transform data to data frame
  df = tryCatch ({
    transformed = transform_raw(file, raw_dat)
    # test if data is usable
    st = paste(transformed, collapse = "")
    if (grepl("}", st) & !grepl(SPATIAL_SPAN, file, ignore.case = TRUE)) { # hacky workaround: newest edition of spatial span data DOES contain braces in "okay" datasets
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

#' @importFrom magrittr %>%
#' @keywords internal

transform_raw <- function (file, raw_dat) {
  if (nrow(raw_dat) == 0) return (data.frame())
  
  dat <- raw_dat %>%
    # standardize raw data
    standardize_raw_csv_data() %>%
    # remove nondata rows
    remove_nondata_rows() %>%
    # move grouping rows into column
    transform_grouping_rows() %>%
    # standardize session info
    standardize_session_info() %>%
    # transform session info into columns
    transform_session_info() %>%
    # parse subsections
    parse_subsections() %>%
    # standardize output
    standardize_names() %>%
    dplyr::mutate(file = file,
                  module = identify_module(file[1])) %>%
    standardize_ace_column_names()
  
  cols = names(dat)
  
  if (!(COL_TIME %in% cols)) {
    # make "time" column from subid & filename if file doesn't contain time
    # DANGEROUS: if constructing time from filename, this will cause de-duplication to fail silently
    # because duplicated records have different filenames
    dat[[COL_TIME]] = paste(dat[[COL_FILE]], dat[[COL_SUB_ID]], sep = ".")
  }
  
  if (COL_PID %in% cols) {
    # very band-aid: attempt to repair PID using name field if PID is empty stem or otherwise filler
    if (unique(dat[[COL_PID]]) %in% c("ADMIN-UCSF-", "ADMIN-UCSF-0", "ADMIN-UCSF-0000")) {
      dat[[COL_PID]] = paste0("ADMIN-UCSF-", dat[[COL_NAME]])
    }
    # make block id from pid & time
    dat[[COL_BID]] = paste(dat[[COL_PID]], dat[[COL_TIME]], sep = ".")
    # make short block id using only pid and date (to allow for less granular matching of records between diff modules)
    dat[[COL_BID_SHORT]] = paste(dat[[COL_PID]],
                                 lubridate::floor_date(lubridate::parse_date_time(dat[[COL_TIME]], "ymdHMSz"), unit = "days"),
                                 sep = ".")
  } else {
    # make block id from file name & time if file doesn't contain PID
    dat[[COL_BID]] = paste(dat[[COL_FILE]], dat[[COL_TIME]], sep = ".")
    # make short block id using only pid and date (to allow for less granular matching of records between diff modules)
    dat[[COL_BID_SHORT]] = paste(dat[[COL_FILE]],
                                 lubridate::floor_date(lubridate::parse_date_time(dat[[COL_TIME]], "ymdHMSz"), unit = "days"),
                                 sep = ".")
    dat[[COL_PID]] = guess_pid(dat[[COL_FILE]])
  }
  
  try({ # so will fail silently if gender isn't in data
    # this patch to propagate gender down has to be done for OLD files where gender was called "age1"
    if (length(unique(dat[[COL_GENDER]])) > 1) {
      if ("FEMALE" %in% unique(dat[[COL_GENDER]])) { 
        this_gender = "FEMALE"
      } else if ("MALE" %in% unique(dat[[COL_GENDER]])) {
        this_gender = "MALE"
      } else {this_gender = "OTHER"}
      dat[[COL_GENDER]] = this_gender
    }
  }, silent = TRUE)
  # replace all text "NA"s with real NA
  dat = replace_nas(dat, NA)
  dat = standardize_ace_values(dat)
  return (dat)
}

#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @keywords internal

transform_pulvinar <- function (file, dat) {
  if (nrow(dat) == 0) return (data.frame())
  # standardize output
  dat = dat %>%
    standardize_names(pulvinar = T) %>%
    mutate(file = file,
           # for faster performance bc each pulvinar file should only contain one module
           module = identify_module(file[1])) %>%
    standardize_ace_column_names() %>%
    # make block id from pid & time
    mutate(!!Q_COL_BID := paste(!!Q_COL_PID, !!Q_COL_TIME, sep = "."),
           # make short block id from pid and date only
           !!Q_COL_BID_SHORT := paste(!!Q_COL_PID,
                                      lubridate::floor_date(lubridate::parse_date_time(!!Q_COL_TIME, "ymdHMSz"), unit = "days"),
                                      sep = ".")) %>%
    standardize_ace_values() %>%
    group_by(!!Q_COL_BID) %>%
    mutate(!!COL_BLOCK_HALF := plyr::mapvalues(make_half_seq(n()), from = c(1, 2), to = c("first_half", "second_half"))) %>%
    ungroup()

  if (COL_NAME %in% names(dat) & grepl("ADMIN-UCSF", dat[1, COL_PID])) { # this function expects a "name" column by which to do the matching
    dat = remove_nondata_rows_pulvinar(dat)
  }
  return (dat)
}

#' @keywords internal deprecated

clean_invalid_subs <- function(dat) {
  # leaving this function here for now in case it becomes necessary later.
  # currently this function is made obsolete by remove_nondata_rows_pulvinar() and the rejection criterion of only matching PID-names in parse_subsections_pulvinar()
  # using data.table internally for speed & reduction of with() calls
  # using bare colnames here because we can assume these cols will exist under this name and data.table doesn't like quoted varnames
  # repairing those with valid but not matching pid_num and name
  dat = as.data.table(tidyr::separate_(dat, COL_PID, into = c("pid_stem", "pid_num"), sep = 11, remove = F))
  dat_good = dat[pid_num == name]
  dat_bad = dat[pid_num != name]
  subs = unique(dat_bad[[COL_PID]])
  for (sub in subs) {
    this_name = unique(dat_bad[pid == sub, name])
    # if pid_num already exists as matching data, change this pid_num to name
    if (dim(dat_good[pid_num == this_name])[1] > 0) {
      dat_bad[, pid_num := ifelse(pid == sub, name, pid_num)]
      dat_bad[, pid := ifelse(pid == sub, paste0(pid_stem, name), pid)]
    } else if (dim(dat_good[name == this_name])[1] > 0) {
      # else if name already exists as matching data, change this name to pid_num
      dat_bad[, name := ifelse(pid == sub, pid_num, name)]
    }
  }
  clean = rbind(dat_good, dat_bad)
  return(as.data.frame(clean[, c("pid_stem", "pid_num") := NULL]))
}

#' @keywords internal

guess_pid <- function(x) {
  file = basename(x)
  # maybe_pid = stringr::str_extract(file, "^[a-zA-Z0-9]*")
  maybe_pid = unique(na.omit(as.numeric(unlist(strsplit(unlist(file), "[^0-9]+")))))[1]
  return (maybe_pid)
}