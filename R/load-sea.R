
#' Reads raw SEA data from a file.
#'
#' Reads, parses, and converts an SEA csv into an R \code{\link{data.frame}}.
#'
#' @export
#' @importFrom dplyr case_when group_by if_else lag mutate mutate_if ungroup %>%
#' @importFrom purrr map2 possibly
#' @importFrom stringr str_replace_all str_trim
#' @importFrom tidyr nest unite unnest
#' @importFrom rlang !! parse_expr sym
#' @param file The name of the file which the data is to be read from.
#' @param verbose Print file name (as progress checker)? Defaults to \code{FALSE}.
#' @return Returns the file's content as an R \code{\link{data.frame}}.

load_sea_file <- function (file, verbose = FALSE) {
  dat <- read_sea_csv(file)
  if (verbose) print(file)
  if (nrow(dat) == 0) {
    warning(paste0(file, " contains no data!"))
    return (data.frame())
  }
  
  dat <- dat %>%
    # standardize case convention in colnames
    standardize_names() %>%
    mutate(file = file)
  
  if ("current_year" %in% names(dat)) {
    dat <- dat %>%
      unite(time, current_date, current_year, current_time, sep = "")
  } else {
    dat <- dat %>%
      mutate(time = str_replace_all(current_date, ",", ""))
  }
  
  dat <- dat %>%
    mutate_if(is.character, stringr::str_trim) %>%
    standardize_sea_column_names() %>%
    label_sea_module_conditions() %>%
    standardize_sea_module_names() %>%
    standardize_sea_values() %>%
    group_by(!!Q_COL_PID) %>%
    # todo: when retyping is complete (and all imported cols aren't char)
    # re-implement coercing of time col to datetime format
    # until then, it's causing string parsing warnings all over the place so not doing it
    mutate(bid = paste(!!Q_COL_PID, .data[[COL_TIME]][1]),
           bid_short = paste(!!Q_COL_PID, lubridate::floor_date(lubridate::parse_date_time(.data[[COL_TIME]][1], "mdyHMS"), unit = "days")),
           correct_button = if_else(tolower(!!Q_COL_RESPONSE) == tolower(!!Q_COL_CORRECT_RESPONSE),
                                   "correct", "incorrect"),
           correct_button = if_else(tolower(!!Q_COL_RESPONSE) == "no_response",
                                            "no_response",
                                            correct_button)) %>%
    group_by(!!Q_COL_MODULE, !!Q_COL_BID_SHORT) %>%
    mutate(previous_correct_button = lag(correct_button),
           half = dplyr::recode(make_half_seq(n()), `1` = "first_half", `2` = "second_half")) %>%
    group_by(!!Q_COL_MODULE) %>%
    nest() %>%
    mutate(data = map2(data, module, ~append_info(.x, module = .y))) %>%
    unnest()
  return (dat)
}

#' Because the "Reading Fluency" module has some cells with unquoted commas,
#' They are causing the usual read.csv delimiter guessing to split
#' one cell into two, creating too many columns for just a few rows
#' 
#' @importFrom magrittr %>%
#' @importFrom stringr str_trim
#' @importFrom utils read.table
#' @keywords internal

read_sea_csv <- function(file) {
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