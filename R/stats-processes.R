
#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal

ace_descriptive_statistics <- function(x, col) {
  out = summarize(x,
                  across(any_of(col),
                         list(
                           mean = ace_mean,
                           median = ace_median,
                           count = ace_count,
                           length = ace_length,
                           sd = ace_sd,
                           min = ace_min
                           )))
  return (out)
}

#' @keywords internal 

ace_average_turns <- function (x, y) {
  avg_last_3_turns = c(turns_three = ace_turns(x, y, n = 3))
  avg_last_5_turns = c(turns_five = ace_turns(x, y, n = 5))
  return (c(avg_last_3_turns, avg_last_5_turns))
}

#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal 

ace_spatial_span <- function(x, col) {
  sustained_span = summarize(x,
                             across(any_of(col),
                                    list(span = ace_span)))
  return (sustained_span)
}

#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal 

ace_max_delay <- function(x, col) {
  max_delay = summarize(x,
                        across(any_of(col),
                               list(max_delay_time = ace_max),
                               .names = "{.fn}"))
  return (max_delay)
}

#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal 

ace_dprime_dplyr <- function(x, col) {
  dprime_out = summarize(x,
                         across(any_of(col),
                                list(dprime = ~ace_dprime(.),
                                     count_hit = ~ace_count_if(., "Hit"),
                                     count_miss = ~ace_count_if(., "Miss"),
                                     count_cr = ~ace_count_if(., "Correct Rejection"),
                                     count_fa = ~ace_count_if(., "False Alarm")),
                                .names = "sdt_{.fn}"))
  return (dprime_out)
}

#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal 

ace_ishihara_dplyr <- function(x, col) {
  out = summarize(x, across(any_of(col),
                            list(colorblind = ace_ishihara),
                            .names = "{.fn}"))
  return (out)
}

#' @keywords internal 

ace_ishihara <- function(x) { # uses y/n readings from "rg_color_deficiency" column
  if ("YES" %in% x) {return ("YES")
  } else {return ("NO")}
}

#' @importFrom dplyr summarize
#' @importFrom rlang !!
#' @keywords internal

ace_rcs <- function(x, cols) {
  acc = sym(cols[1])
  rt = sym(cols[2])
  out = summarize(x, 
                  rcs = ace_sum(!!acc) / ace_sum(!!rt) * 1000L)
  return (out)
}

#' @importFrom dplyr across mutate summarize
#' @importFrom rlang !! :=
#' @importFrom tidyselect any_of
#' @keywords internal 

ace_practice_count <- function(x, col) {
  out = summarize(x, across(any_of(col), 
                            list(practice_count = ace_median),
                            .names = "{.col}")) %>% 
    mutate(!!COL_PRACTICE_COUNT := as.integer(!!Q_COL_PRACTICE_COUNT))
  return (out)
}

#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal 

sea_descriptive_statistics <- function(x, col) {
  out = summarize(x,
                  across(any_of(col), 
                         list(mean = ace_mean,
                              median = ace_median,
                              sum = ace_sum,
                              count = ace_count,
                              length = ace_length,
                              sd = ace_sd)))
  return (out)
}

#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal 

sea_reading_descriptive_statistics <- function(x, col) {
  out = summarize(x,
                  across(any_of(col),
                         list(mean = ace_mean,
                              sum = ace_sum,
                              score = sea_sum_adj,
                              count = ace_count,
                              length = ace_length,
                              sd = ace_sd)))
  return (out)
}

#' @importFrom dplyr across summarize
#' @importFrom tidyselect any_of
#' @keywords internal 

sea_task_duration <- function(x, col) {
  out = summarize(x,
                  across(any_of(col),
                         list(duration = ace_max)))
  return (out)
}
