
#' @importFrom dplyr funs one_of summarize_at vars
#' @keywords internal

ace_descriptive_statistics <- function(x, col) {
  out = summarize_at(x, vars(one_of(col)), funs(
    mean = ace_mean,
    median = ace_median,
    count = ace_count,
    length = ace_length,
    sd = ace_sd))
  return (out)
}

#' @keywords internal 

ace_descriptive_statistics_by_group <- function(x, y) {
  var_by_group = c(sd = ace_sd_by_group(x, y), se = ace_se_by_group(x, y))
  avg_by_group = c(mean = ace_mean_by_group(x, y), median = ace_median_by_group(x, y))
  sum_by_group = c(count = ace_count_by_group(x, y), length = ace_length_by_group(x, y))
  return (c(var_by_group, avg_by_group, sum_by_group))
}

#' @keywords internal 

ace_average_turns <- function (x, y) {
  avg_last_3_turns = c(turns_three = ace_turns(x, y, n = 3))
  avg_last_5_turns = c(turns_five = ace_turns(x, y, n = 5))
  return (c(avg_last_3_turns, avg_last_5_turns))
}

#' @keywords internal 

ace_spatial_span <- function(x, col) {
  sustained_span = summarize_at(x, vars(one_of(col)), funs(
    span = ace_span))
  return (sustained_span)
}

#' @keywords internal 

ace_dprime_dplyr <- function(x, col) {
  dprime_out = summarize_at(x, vars(one_of(col)), funs(
    dprime = ace_dprime))
  return (dprime_out)
}

#' @keywords internal

ace_detection_rate <- function(x, y) {
  rate = c(rate = ace_detection(x, y))
  return (c(rate))
}

#' @keywords internal 

ace_ishihara_dplyr <- function(x, col) {
  out = summarize_at(x, vars(one_of(col)), funs(
    colorblind = ace_ishihara))
  return (out)
}

#' @keywords internal 

ace_ishihara <- function(x) { # uses y/n readings from "rg_color_deficiency" column
  if ("YES" %in% x) {return ("YES")
  } else {return ("NO")}
}

#' @importFrom dplyr funs one_of summarize_at vars
#' @keywords internal 

sea_descriptive_statistics <- function(x, col) {
  out = summarize_at(x, vars(one_of(col)), funs(
    mean = ace_mean,
    median = ace_median,
    sum = ace_sum,
    count = ace_count,
    length = ace_length,
    sd = ace_sd))
  return (out)
}

#' @importFrom dplyr funs one_of summarize_at vars
#' @keywords internal 

sea_reading_descriptive_statistics <- function(x, col) {
  out = summarize_at(x, vars(one_of(col)), funs(
    mean = ace_mean,
    sum = ace_sum,
    score = sea_sum_adj,
    count = ace_count,
    length = ace_length,
    sd = ace_sd))
  return (out)
}

#' @keywords internal 

sea_task_duration <- function(x, col) {
  out = summarize_at(x, vars(one_of(col)), funs(
    duration = ace_max))
  return (out)
}
