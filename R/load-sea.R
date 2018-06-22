
#' Reads raw SEA data from a file.
#'
#' Reads, parses, and converts an SEA csv into an R \code{\link{data.frame}}.
#'
#' @export
#' @importFrom dplyr case_when group_by if_else lag mutate mutate_if ungroup %>%
#' @importFrom purrrr nest unnest map2
#' @importFrom stringr str_trim
#' @importFrom tidyr unite
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
  # standardize case convention in colnames
  names(dat) <- standardize_names(dat)
  
  dat <- dat %>%
    mutate(file = file) %>%
    unite(time, current_date, current_year, current_time, sep = " ") %>%
    mutate_if(is.character, stringr::str_trim) %>%
    standardize_sea_column_names() %>%
    label_sea_module_conditions() %>%
    standardize_sea_module_names() %>%
    group_by(!! sym(COL_PID)) %>%
    mutate(bid = paste(.data[[COL_PID]], .data[[COL_TIME]][1]),
           correct_button = ifelse(tolower(.data[[COL_RESPONSE]]) == tolower(.data[[COL_CORRECT_RESPONSE]]),
                                   "correct", "incorrect"),
           half = dplyr::recode(make_half_seq(n()), `1` = "first_half", `2` = "second_half")) %>%
    group_by(!! sym(COL_MODULE)) %>%
    nest() %>%
    mutate(data = map2(data, module, ~append_info(.x, module = .y))) %>%
    unnest()
  return (dat)
}

#' Because the "Reading Fluency" module has some cells with unquoted commas,
#' They are causing the usual read.csv delimiter guessing to split
#' one cell into two, creating too many columns for just a few rows
#' 
#' @importFrom stringr str_trim
#' @importFrom utils read.table
#' @keywords internal

read_sea_csv <- function(file) {
  dat <- read.table(file, sep = ",", header = F, stringsAsFactors = F,
                    col.names = paste0("V", 1:32), fill = T)
  
  if (all(unique(dat[, 31]) == "") | all(is.na(dat[, 31]))) return (dat)
  
  names(dat) <- c(str_trim(dat[1, 1:30]), "junk", "junk2")
  dat <- dat[2:nrow(dat), ]
  if (all(is.na(dat$junk2))) {
    bad_rows <- which(dat$junk != "")
    dat[bad_rows, "Question Text"] <- paste(dat[bad_rows, "Question Text"],
                                            dat[bad_rows, "Question Type"], sep = ",")
    # Moving these bad rows' data after the bad delimiter one column to the left
    dat[bad_rows, 6:30] <- dat[bad_rows, 7:31]
  } else {
    bad_rows1 <- which(dat$junk != "" & dat$junk2 == "")
    dat[bad_rows1, "Question Text"] <- paste(dat[bad_rows1, "Question Text"],
                                             dat[bad_rows1, "Question Type"], sep = ",")
    # Moving these bad rows' data after the bad delimiter one column to the left
    dat[bad_rows1, 6:30] <- dat[bad_rows1, 7:31]
    
    bad_rows2 <- which(dat$junk2 != "")
    dat[bad_rows2, "Question Text"] <- paste(dat[bad_rows2, "Question Text"],
                                             dat[bad_rows2, "Question Type"],
                                             dat[bad_rows2, "User Answer"], sep = ",")
    # Moving these bad rows' data after the bad delimiters two columns to the left
    dat[bad_rows2, 6:30] <- dat[bad_rows2, 8:32]
  }
  return (dat[, 1:30])
}