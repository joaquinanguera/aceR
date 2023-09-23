
#' Reads raw ACE data from a file.
#'
#' Reads, parses, and converts an ACE csv or xls into an R \code{\link{data.frame}}.
#'
#' @export
#' @importFrom purrr map2
#' @importFrom utils read.table read.csv write.csv head tail count.fields
#' 
#' @param file The name of the file which the data is to be read from.
#' @param app_type character What app data export type produced this data? One of
#' \code{c("explorer", "email", "pulvinar")}.
#' @return Returns the file's content as an R \code{\link{data.frame}}.

load_ace_file <- function(file, app_type) {
  # read raw csv file
  if (is_excel(file)) {
    warning (file, " is Excel format, currently not supported ")
    return (data.frame())
    # raw_dat <- load_excel(file)
  } 
  
  raw_dat <- load_csv(file, app_type = app_type)
  
  if (app_type != "email") {
    out <- raw_dat %>%
      transform_mid(file = file, app_type = app_type)

    if (is_pulvinar(file) | app_type == "pulvinar") {
      out <- out %>%
        transform_post_pulvinar()
    }
    return (out)
  } else if (app_type == "email") { # only if it hasn't already been loaded
    raw_dat <- breakup_by_user(raw_dat)
    
    if (is.vector(raw_dat)) {
      ortho_names = paste(file, names(raw_dat), sep = "-")
      dfs = map2(ortho_names, raw_dat, ~attempt_transform_email(.x, .y))
      out = plyr::rbind.fill(dfs)
      return (out)
    } else {
      return (attempt_transform_email(file, raw_dat))
    }
  }
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

#' @importFrom magrittr %>%
#' @keywords internal

attempt_transform_email <- function(file, raw_dat) {
  # transform data to data frame
  df = tryCatch ({
    transformed = raw_dat %>% 
      transform_pre_email() %>%
      transform_mid(file = file, app_type = "email") %>%
      transform_post_email()
    # test if data is usable
    st = paste(transformed, collapse = "")
    # hacky workaround: newest edition of spatial span data DOES contain braces in "okay" datasets
    if (grepl("}", st) & !grepl(SPATIAL_SPAN, file, ignore.case = TRUE)) {
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

transform_pre_email <- function (raw_dat) {
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
    parse_subsections()
  
  return (dat)
}

#' @keywords internal

transform_post_email <- function (dat) {
  
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
  
  return (dat)
}

#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @keywords internal

transform_mid <- function (dat, file, app_type) {
  if (nrow(dat) == 0) return (data.frame())
  # This chunk same between email and pulvinar
  # standardize output
  if (app_type == "pulvinar") data_type <- "explorer" else data_type <- app_type
  dat <- dat %>%
    standardize_names(data_type = data_type) %>%
    mutate(file = file,
           # for faster performance bc each pulvinar file should only contain one module
           module = identify_module(file[1])) %>%
    standardize_ace_column_names()
  
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
    standardize_ace_values(app_type = app_type) %>% 
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

#' @keywords internal

transform_post_pulvinar <- function (dat) {
  if (COL_NAME %in% names(dat) & grepl("ADMIN-UCSF", dat[1, COL_PID])) { # this function expects a "name" column by which to do the matching
    dat <- remove_nondata_rows_pulvinar(dat)
  }
  return (dat)
}
