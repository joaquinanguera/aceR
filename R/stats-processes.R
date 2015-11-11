
#' @keywords internal

ace_descriptive_statistics <- function(x) {
  variance = c(sd = ace_sd(x), se = ace_se(x))
  summary = c(length = ace_length(x), count = ace_count(x))
  averages = c(mean = ace_mean(x), median = ace_median(x))
  return (c(variance, summary, averages))
}

#' @keywords internal 

ace_descriptive_statistics_by_group <- function(x, y) {
  var_by_group = c(sd = ace_sd_by_group(x, y), se = ace_se_by_group(x, y))
  avg_by_group = c(mean = ace_mean_by_group(x, y), median = ace_median_by_group(x, y))
  return (c(var_by_group, avg_by_group))
}