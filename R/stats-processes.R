
#' @keywords internal

ace_descriptive_statistics <- function(x) {
  variance = c(sd = ace_sd(x), se = ace_se(x))
  summary = c(num = ace_num(x), count = ace_count(x))
  averages = c(mean = ace_mean(x), median = ace_median(x))
  return (c(variance, summary, averages))
}