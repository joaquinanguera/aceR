
#' Reads raw SEA data from a file.
#'
#' Reads, parses, and converts an SEA csv into an R \code{\link{data.frame}}.
#'
#' @export
#' @importFrom dplyr across case_when group_by if_else lag mutate ungroup select
#' @importFrom magrittr %>%
#' @importFrom purrr map2 possibly
#' @importFrom stringr str_replace_all str_trim
#' @importFrom tidyr nest unite unnest
#' @import tidyselect
#' @importFrom rlang !! parse_expr sym
#' @param file The name of the file which the data is to be read from.
#' @param verbose Print file name (as progress checker)? Defaults to \code{FALSE}.
#' @param data_type character What app data export type produced this data? One of
#' \code{c("nexus", "pulvinar")}. Must be specified.
#' @return Returns the file's content as an R \code{\link{data.frame}}.

load_sea_file <- function (file, verbose = FALSE, data_type = c("nexus", "pulvinar")) {
  if (data_type == "nexus") {
    dat <- load_csv(file) %>% 
      select(-any_of(c("SessionID", "session_id")))
  } else if (data_type == "pulvinar") {
    dat <- load_sea_csv(file)
  }
  if (verbose) print(file)
  if (nrow(dat) == 0) {
    warning(paste0(file, " contains no data!"))
    return (data.frame())
  }
  
  dat <- dat %>%
    # standardize case convention in colnames
    standardize_names(data_type = data_type) %>%
    mutate(file = file)
  
  if ("current_year" %in% names(dat)) {
    dat <- dat %>%
      unite(time, current_date, current_year, current_time, sep = "")
  } else {
    dat <- dat %>%
      mutate(time = str_replace_all(current_date, ",", ""))
  }
  
  dat <- dat %>%
    mutate(across(where(is.character), stringr::str_trim)) %>%
    standardize_sea_column_names() %>%
    label_sea_module_conditions() %>%
    standardize_sea_module_names() %>%
    standardize_sea_values() %>%
    standardize_sea_ids(data_type = data_type) %>% 
    group_by(!!Q_COL_PID) %>%
    # todo: when retyping is complete (and all imported cols aren't char)
    # re-implement coercing of time col to datetime format
    # until then, it's causing string parsing warnings all over the place so not doing it
    mutate(bid = paste(!!Q_COL_PID, .data[[COL_TIME]][1]),
           # for emailed SEA data, must use this hacky thing because the timestamp to the second changes with every trial
           bid_short = paste(!!Q_COL_PID, lubridate::floor_date(lubridate::parse_date_time(.data[[COL_TIME]][1], "mdyHMS"), unit = "days")),
           # but for nexus SEA data, can do it as it's done with ACE
           correct_button = if_else(tolower(!!Q_COL_RESPONSE) == tolower(!!Q_COL_CORRECT_RESPONSE),
                                    "correct", "incorrect"),
           correct_button = if_else(tolower(!!Q_COL_RESPONSE) == "no_response",
                                    "no_response",
                                    correct_button)) %>%
    group_by(!!Q_COL_MODULE, !!Q_COL_BID_SHORT) %>%
    mutate(previous_correct_button = lag(correct_button),
           half = dplyr::recode(make_half_seq(n()), `1` = "first_half", `2` = "second_half")) %>%
    ungroup() %>%
    nest(data = -!!Q_COL_MODULE) %>%
    mutate(data = map2(data, module, ~append_info(.x, module = .y))) %>%
    unnest(data)
  return (dat)
}
