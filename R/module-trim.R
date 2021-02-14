
#' ACE/SEA module trimming
#'
#' Methods for trimming trials out of user data.
#'
#' @keywords internal
#' @name ace_trims
NULL

#' Trim trials from ACE/SEA data by response time within a fixed range
#'
#' Applies corresponding \code{\link{ace_trims}} to every session of data.
#'
#' @section Assumptions:
#' Assumes the \code{\link{data.frame}} is nested, with two columns:
#' \code{module} (character) and \code{data} (list, each containing a \code{\link{data.frame}}).
#'
#' @export
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang !!
#' @importFrom tidyr nest
#' @param df a \code{\link{data.frame}} containing formatted trialwise ACE data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' 
#' @param cutoff_min numeric. Remove within-subject RTs \emph{below} (but not equal to)
#' this specified value (in ms)? Defaults to \code{NA}, where no too-low values will be scrubbed.
#' Note that \code{\link{load_ace_bulk}} already removes RTs below 150 ms for all modules,
#' so those can never be included even if this minimum cutoff is set to a value below 150 ms.
#' @param cutoff_max numeric vector. Remove within-subject RTs \emph{above} (but not equal to)
#' this specified value? Defaults to \code{NA}, where no too-high values will be scrubbed.
#' Defaults to \code{FALSE}.
#' @param exclude character vector. Specify the names of modules (proper naming convention!)
#' that should be \emph{ignored}. Defaults to \code{c()}, where all modules are subject to
#' range scrubbing.
#' @param verbose logical. Print details? Defaults to \code{TRUE}.
#' @return Returns the input data, with RTs corresponding to offending trials rendered as NA.

trim_rt_trials_range <- function(df,
                                 cutoff_min = NA,
                                 cutoff_max = NA,
                                 exclude = c(),
                                 verbose = TRUE) {
  
  stopifnot(!check_module_misspelling(exclude))
  
  if (!is.na(cutoff_min) & cutoff_min < 150) {
    warning(crayon::yellow("Minimum allowable RT specified less than 150 ms.",
                           "Trials with RT < 150 ms have already been discarded!"))
  }
  
  # Quasiquoting does not seem to behave as planned inside of map()
  for (i in 1:nrow(df)) {
    
    if (!(df$module[i] %in% c(DEMOS, ISHIHARA, SPATIAL_SPAN, BACK_SPATIAL_SPAN, exclude))) {
      
      df$data[[i]] <- df$data[[i]] %>%
        group_by(!!Q_COL_BID) %>%
        mutate(!!COL_RT := as.numeric(!!Q_COL_RT))
      
      # ordered so that removing acc first doesn't alter RT for the RT calculation
      
      if (!is.na(cutoff_min)) {
        df$data[[i]] <- df$data[[i]] %>%
          mutate(!!COL_CORRECT_BUTTON := na_if_true(!!Q_COL_CORRECT_BUTTON, !!Q_COL_RT < cutoff_min & !!Q_COL_RT != -99),
                 !!COL_RT := na_if_true(!!Q_COL_RT, !!Q_COL_RT < cutoff_min & !!Q_COL_RT != -99))
      }
      if (!is.na(cutoff_max)) {
        df$data[[i]] <- df$data[[i]] %>%
          mutate(!!COL_CORRECT_BUTTON := na_if_true(!!Q_COL_CORRECT_BUTTON, !!Q_COL_RT > cutoff_max & !!Q_COL_RT != -99),
                 !!COL_RT := na_if_true(!!Q_COL_RT, !!Q_COL_RT > cutoff_max & !!Q_COL_RT != -99))
      }
      
      # needs to be grouped to prevent previous_correct_button from bleeding over between records
      df$data[[i]] <- df$data[[i]] %>%
        mutate(!!COL_PREV_CORRECT_BUTTON := make_lagged_col(!!Q_COL_CORRECT_BUTTON)) %>%
        ungroup()
    }
    
    # After all the heavy lifting is done on COL_CORRECT_BUTTON
    # if trial_accuracy is a column, NA it to match
    if ("trial_accuracy" %in% names(df$data[[i]])) {
      df$data[[i]] <- df$data[[i]] %>%
        mutate(trial_accuracy = na_if_true(trial_accuracy, is.na(!!Q_COL_CORRECT_BUTTON)))
    }
    
  }
  
  return (df)
}

#' Trim trials from ACE/SEA data by response time within N SDs of the mean
#'
#' Applies corresponding \code{\link{ace_trims}} to every session of data.
#' If trimming is to be done by range and by SD, we recommend calling \code{\link{trim_rt_trials_range}}
#' first, and then calling this function to trim by SD.
#'
#' @section Assumptions:
#' Assumes the \code{\link{data.frame}} is nested, with two columns:
#' \code{module} (character) and \code{data} (list, each containing a \code{\link{data.frame}}).
#'
#' @export
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang !!
#' @importFrom tidyr nest
#' @param df a \code{\link{data.frame}} containing formatted trialwise ACE data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' 
#' @param cutoff numeric. Remove within-subject RTs further than this many SD from
#' within-subject mean RT? Enter as one number. Defaults to 3.
#' @param exclude character vector. Specify the names of modules (proper naming convention!)
#' that should be \emph{ignored}. Defaults to \code{"SAAT"}, but can be specified as an empty
#' character vector (in which case the SAAT module's RTs would be subject to SD scrubbing).
#' @param verbose logical. Print details? Defaults to \code{TRUE}.
#' @return Returns the input data, with RTs corresponding to offending trials rendered as NA.

trim_rt_trials_sd <- function(df, cutoff = 3, exclude = c("SAAT"), verbose = TRUE) {
  
  stopifnot(!check_module_misspelling(exclude))
  
  # Quasiquoting does not seem to behave as planned inside of map()
  for (i in 1:nrow(df)) {
    
    if (!(df$module[i] %in% c(DEMOS, ISHIHARA, SPATIAL_SPAN, BACK_SPATIAL_SPAN, exclude))) {
      
      df$data[[i]] <- df$data[[i]] %>%
        group_by(!!Q_COL_BID) %>%
        mutate(!!COL_RT := as.numeric(!!Q_COL_RT))
      
      # ordered so that removing acc first doesn't alter RT for the RT calculation
      df$data[[i]] <- df$data[[i]] %>%
        mutate(rt_scaled = na_if(!!Q_COL_RT, -99),
               rt_scaled = c(scale(rt_scaled)),
               !!COL_CORRECT_BUTTON := na_if_true(!!Q_COL_CORRECT_BUTTON, abs(rt_scaled) > cutoff),
               !!COL_RT := na_if_true(!!Q_COL_RT, abs(rt_scaled) > cutoff)) %>% 
        select(-rt_scaled)
      
      # needs to be grouped to prevent previous_correct_button from bleeding over between records
      df$data[[i]] <- df$data[[i]] %>%
        mutate(!!COL_PREV_CORRECT_BUTTON := make_lagged_col(!!Q_COL_CORRECT_BUTTON)) %>%
        ungroup()
    }
    
    # After all the heavy lifting is done on COL_CORRECT_BUTTON
    # if trial_accuracy is a column, NA it to match
    if ("trial_accuracy" %in% names(df$data[[i]])) {
      df$data[[i]] <- df$data[[i]] %>%
        mutate(trial_accuracy = na_if_true(trial_accuracy, is.na(!!Q_COL_CORRECT_BUTTON)))
    }
    
  }
  
  return (df)
}
