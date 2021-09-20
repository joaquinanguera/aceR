
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
#' range scrubbing. Note that this function always excludes spatial span tasks, as those are 
#' much shorter than other modules.
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
      
      if (verbose) cat(crayon::green("Trimmed", df$module[i]), sep = "\n")
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
#' that should be \emph{ignored}. Defaults to all \code{"SAAT"} data, but can be specified as an empty
#' character vector (in which case the SAAT module's RTs would be subject to SD scrubbing).
#' Note that this function always excludes spatial span tasks, as those are much shorter 
#' than other modules.
#' @param verbose logical. Print details? Defaults to \code{TRUE}.
#' @return Returns the input data, with RTs corresponding to offending trials rendered as NA.

trim_rt_trials_sd <- function(df, cutoff = 3,
                              exclude = c("SAAT", "SAATIMPULSIVE", "SAATSUSTAINED"),
                              verbose = TRUE) {
  
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
      
      if (verbose) cat(crayon::green("Trimmed", df$module[i]), sep = "\n")
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

#' Trim initial trials from ACE/SEA data
#'
#' Applies corresponding \code{\link{ace_trims}} to every session of data.
#' If subsequent trimming is to be done by reacion time, we recommend calling this function first,
#' and then calling \code{\link{trim_rt_trials_range}} or \code{\link{trim_rt_trials_sd}}.
#'
#' @section Assumptions:
#' Assumes the \code{\link{data.frame}} is nested, with two columns:
#' \code{module} (character) and \code{data} (list, each containing a \code{\link{data.frame}}).
#'
#' @export
#' @import dplyr
#' @importFrom rlang !! sym
#' @param df a \code{\link{data.frame}} containing formatted trialwise ACE data. 
#'
#' This includes data loaded with the following methods: 
#' \enumerate{
#'   \item \code{\link{load_ace_file}}
#'   \item \code{\link{load_ace_bulk}}
#' }
#' 
#' @param n numeric. How many trials to remove from each participant's data? Defaults to 5.
#' When set as an \emph{integer}, removes the first N trials from each condition.
#' When set as a \emph{decimal} between 0 and 1, removes the first (N*100) percent of trials 
#' from each condition (more trials removed from longer tasks). Either way, this function
#' will remove the first N trials from each \emph{condition} completed by each participant.
#' @param exclude character vector. Specify the names of modules (proper naming convention!)
#' that should be \emph{ignored}. Defaults to an empty character vector, so that all modules
#' are scrubbed. Note that this function always excludes spatial span tasks, as those are
#' much shorter than other modules.
#' @param verbose logical. Print details? Defaults to \code{TRUE}.
#' @return Returns the input data, with the first N trials for each participant \emph{removed}.

trim_initial_trials <- function(df, n = 5,
                              exclude = c(),
                              verbose = TRUE) {
  
  stopifnot(!check_module_misspelling(exclude), (n > 0 & n < 1) | (n > 1 & n %% 1 == 0))
  
  if (verbose) {
    if (n < 1) {
      cat(crayon::blue("Trimming first ", n*100, "% of trials", sep = ""), sep = "\n")
    } else {
      cat(crayon::blue("Trimming first", n, "trials"), sep = "\n")
    }
  }
  
  for (i in 1:nrow(df)) {
    
    if (!(df$module[i] %in% c(DEMOS, ISHIHARA, SPATIAL_SPAN, BACK_SPATIAL_SPAN, exclude))) {
      
      if (df$module[i] %in% c(ADP, FLANKER, STROOP, SPATIAL_CUE, TASK_SWITCH)) {
        # these are modules where condition isn't blocked
        # thus trial_number doesn't restart with condition
        # and incidentally is stored in another column name
        # make dummy trial number col that is condition-specific
        if (df$module[i] == TASK_SWITCH) {
          cond_col = sym("taskswitch_state")
        } else if (df$module[i] == ADP) {
          cond_col = sym("expression")
        } else {
          cond_col = Q_COL_TRIAL_TYPE
        }
        df$data[[i]] <- df$data[[i]] %>%
          arrange(!!Q_COL_BID, trial_number) %>% 
          group_by(!!Q_COL_BID, !!cond_col) %>% 
          # so it counts from 0 like trial_number
          mutate(trial_number_temp = 0:(n()-1))
      } else {
        df$data[[i]] <- df$data[[i]] %>%
          group_by(!!Q_COL_BID) %>% 
          # Use included trial_number col because it restarts per condition, what we want
          mutate(trial_number_temp = trial_number)
      }
      
      if (n < 1) {
        df$data[[i]] <- df$data[[i]] %>%
          filter(trial_number_temp > n * max(trial_number_temp))
      } else {
        df$data[[i]] <- df$data[[i]] %>%
          filter(trial_number_temp >= n)
      }
      
      df$data[[i]] <- df$data[[i]] %>%
        select(-trial_number_temp) %>% 
        ungroup()
      
      if (verbose) cat(crayon::green("Trimmed", df$module[i]), sep = "\n")
    }
  }
  
  return (df)
}
