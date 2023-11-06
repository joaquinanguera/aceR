
#' Reads raw ACE data from a file.
#'
#' Reads, parses, and converts an ACE csv or xls into an R \code{\link{data.frame}}.
#'
#' @export
#' @importFrom purrr map2
#' @importFrom utils read.table read.csv write.csv head tail count.fields
#' 
#' @param file The name of the file which the data is to be read from.
#' @param data_type character What app data export type produced this data? One of
#' \code{c("nexus", "explorer")}. Must be specified.
#' @return Returns the file's content as an R \code{\link{data.frame}}.

load_ace_file <- function(file, data_type) {

  raw_dat <- load_csv(file)
  
    out <- raw_dat %>%
      transform_mid(file = file, data_type = data_type)

    return (out)

}

#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @keywords internal

transform_mid <- function (dat, file, data_type) {
  if (nrow(dat) == 0) return (data.frame())
  # This chunk same between email and pulvinar
  # standardize output

  dat <- dat %>%
    standardize_names(data_type = data_type) %>%
    mutate(file = file)
  
  dat <- dat %>%
    standardize_ace_column_names()
  
  # module is now already in nexus data... EXCEPT demographics files
  # so this needs to run for that, and for legacy explorer data
  if (!(COL_MODULE %in% names(dat))) {
    # assumes each file should only contain one module
    dat <- dat %>% 
      mutate(!!COL_MODULE := identify_module(file[1]))
  }
  
  if (!(COL_TIME %in% names(dat))) {
    # make "time" column from subid & filename if file doesn't contain time
    # DANGEROUS: if constructing time from filename, this will cause de-duplication to fail silently
    # because duplicated records have different filenames
    dat[[COL_TIME]] = paste(dat[[COL_FILE]], dat[[COL_SUB_ID]], sep = ".")
  }
  
  dat <- dat %>%
    # replace all text "NA"s with real NA
    replace_nas(NA) %>%
    standardize_ace_column_types() %>%
    # clean, standardize, possibly construct PID, BID, short BID
    standardize_ace_ids() %>% 
    standardize_ace_values(data_type = data_type) %>% 
    # appends condition to module name for SAAT only
    # should not modify other modules
    # must be done after standardize_ace_values
    # because that one fixes SAAT flipped condition labels
    module_split_saat()

  
  if (COL_PRACTICE %in% names(dat) & dat[[COL_MODULE]][1] != ISHIHARA) {
    dat <- dat %>%
      summarize_practice()
  }
  
  # Should only activate for explorer demos modules
  if (dat[[COL_MODULE]][1] != DEMOS) {
    if (COL_CONDITION %in% names(dat)) {
      dat <- dat %>%
        group_by(!!Q_COL_BID, !!Q_COL_CONDITION) 
    } else {
      dat <- dat %>%
        group_by(!!Q_COL_BID)
    }
    
    # for backward compatibility
    # should only activate for occasional ACE Classroom files
    # that don't include trial number automatically
    if (!(COL_TRIAL_NUM %in% names(dat))) {
      dat <- dat %>%
        mutate(!!COL_TRIAL_NUM := 0:(n()-1))
    }
    
    dat <- dat %>%
      mutate(!!COL_BLOCK_HALF := plyr::mapvalues(make_half_seq(n()), from = c(1, 2), to = c("first_half", "second_half"))) %>%
      ungroup()
  }
  
  return (dat)
}

