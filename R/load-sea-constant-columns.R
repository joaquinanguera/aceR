
#' SEA column name constants
#' 
#' SEA also uses some ACE column name constants.
#' These are constants which are SEA-specific.
#'
#' @keywords internal
#' @name sea_header
NULL

#' @name sea_header
COL_RESPONSE = "response"

#' @name SEA_header
Q_COL_RESPONSE <- rlang::sym(COL_RESPONSE)

#' @name sea_header
COL_QUESTION_TEXT = "question_text"

#' @name SEA_header
Q_COL_QUESTION_TEXT <- rlang::sym(COL_QUESTION_TEXT)

#' @name sea_header
COL_MODULE = "module"

#' @name SEA_header
Q_COL_MODULE <- rlang::sym(COL_MODULE)

#' @name sea_header
COL_SEA_GROUP = "sea_group"

#' @name SEA_header
Q_COL_SEA_GROUP <- rlang::sym(COL_SEA_GROUP)

#' @name sea_header
ALL_POSSIBLE_SEA_DEMOS <- c(COL_BID, COL_BID_SHORT, COL_PID, COL_AGE, COL_GRADE, COL_GENDER, COL_TIME, COL_FILE, COL_SEA_GROUP)

#' @name sea_header
Q_ALL_POSSIBLE_SEA_DEMOS <- c(Q_COL_BID, Q_COL_BID_SHORT, Q_COL_PID, Q_COL_AGE, Q_COL_GRADE, Q_COL_GENDER, Q_COL_TIME, Q_COL_FILE, Q_COL_SEA_GROUP)

#' @name sea_header

standardize_sea_column_names <- function(df) {
  new = dplyr::recode(names(df),
                      response_time = COL_RT,
                      user_answer = COL_RESPONSE,
                      user_answe = COL_RESPONSE,
                      correct_answer = COL_CORRECT_RESPONSE,
                      correct_answe = COL_CORRECT_RESPONSE,
                      question_type = COL_MODULE,
                      user_id = COL_PID,
                      seagroup = COL_SEA_GROUP,
                      sea_group = COL_SEA_GROUP
                      )
  names(df) = new
  return (df)
}

#' @name sea_header
#' @keywords internal
#' @importFrom dplyr across if_else mutate
#' @importFrom magrittr %>%
#' @importFrom tidyselect any_of

standardize_sea_values <- function(df) {
  # all columns are necessarily read in as character bc multiple modules appear in one raw file
  # but that means that re-typing can't occur until the modules are broken up. hmm.
  df[[COL_RESPONSE]] <- if_else(tolower(df[[COL_RESPONSE]]) == "no answer",
                                "no_response",
                                df[[COL_RESPONSE]])
  
  # if (!("math_recall_orientation" %in% names(df))) df[["math_recall_orientation"]] <- ""
  df <- df %>%
    mutate(across(any_of(c("math_recall_orientation")), tolower))
  return (df)
}

#' @importFrom dplyr mutate group_by ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang !! :=
#' @keywords internal

standardize_sea_ids <- function(dat, data_type = "email") {
    
    if (data_type == "nexus") {
      # make block id from pid & time
      dat <- dat %>%
        mutate(!!COL_BID := paste(!!Q_COL_PID, !!Q_COL_N_FINISHED, sep = ".session"))
    } else if (data_type == "email") {
      # for emailed SEA data, must use this hacky thing
      # because the timestamp to the second changes with every trial
      dat <- dat %>%
        group_by(!!Q_COL_PID) %>% 
        mutate(!!COL_BID := paste(!!Q_COL_PID, .data[[COL_TIME]][1]),
               bid_short = paste(!!Q_COL_PID, lubridate::floor_date(lubridate::parse_date_time(.data[[COL_TIME]][1], "mdyHMS"), unit = "days"))) %>% 
        ungroup()
    }
    
  return (dat)
}
