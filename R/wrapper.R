
#' Read, Load, and Process all ACE csv files in a directory
#'
#' Overarching wrapper function to read & parse 
#'  all ACE csv files in a directory.
#'
#' @export
#' @importFrom magrittr %>% %<>%
#' 
#' @param path_in character. A valid path to a folder containing exclusively raw
#' ACE data.
#' @param path_out character. A valid path to a folder containing exclusively raw
#' ACE data. Defaults to to \code{path_in/..}, such that the output data will be written
#' to the \emph{enclosing} folder containing the \code{path_in} folder. By default, this
#' keeps the summary data separate from the subfolder containing the raw data,
#' but you can specify the output path to whatever you choose
#' (including identical to the input path). Set this to \code{NULL} to process data
#' \emph{without} writing out a CSV.
#' @param verbose logical. Print details? Defaults to \code{TRUE}.
#' @param data_type character What app data export type produced this data? One of
#' \code{c("explorer", "email", "pulvinar")}. Must be specified.
#' @param rt_cutoff_min numeric. Remove within-subject RTs \emph{below} (but not equal to)
#' this specified value (in ms)? Defaults to \code{200}. Can be set to \code{NA},
#' where no too-low values will be scrubbed.
#' Note that \code{\link{load_ace_bulk}} already removes RTs below 150 ms for all modules,
#' so those can never be included even if this minimum cutoff is set to a value below 150 ms.
#' Passed through to \code{\link{trim_rt_trials_range}}.
#' @param post_min_trials Minimum number of trials to require in most restrictive condition.
#' Defaults to 5. This condition is checked against the \code{*_count} summary columns,
#' that count all trials with a valid response time (and all no-go trials, if a response
#' was not expected.) Passed through to \code{\link{post_clean_low_trials}}. Set this argument
#' to \code{NA} to leave all trials in and skip this step.
#' @param post_chance_level character. How strictly to retain records containing
#' above-threshold performance? One of:
#' \itemize{
#'   \item \code{"overall"}: Trim based on \emph{whole-module} accuracy. Minimum of: d' > 0,
#'   2-choice task accuracy > 0.5, 4-choice task accuracy > 0.25. \strong{Default.}
#'   \item \code{"easy"}: Trim based on \emph{within-condition} accuracy. Minimum of: d' > 0,
#'   2-choice task accuracy > 0.5, 4-choice task accuracy > 0.25.
#'   \item \code{"none"}: Do not trim any records by performance threshold.
#' }
#' Passed through to \code{\link{post_clean_chance}}.
#' @param post_metric_names a character vector containing \emph{partial} names of
#' metric columns to include in the processed output. All column names containing
#' any of the inputs will be included in output. Passed through to \code{\link{post_reduce_cols}}.
#' @param post_metric_names_exclude a character vector containing \emph{partial} names of
#' metric columns to exclude from  the processed output. All column names containing
#' any of the inputs will be excluded from output. Passed through to \code{\link{post_reduce_cols}}.
#' @return Returns a \code{\link[tibble]{tibble}} containing a cleaned selection of
#' summary statistics from every module in the data.

proc_ace_complete <- function (path_in,
                               path_out = paste(path_in, "..", sep = "/"),
                               verbose = TRUE,
                               data_type = c("explorer", "email", "pulvinar"),
                               rt_cutoff_min = 200,
                               post_min_trials = 5,
                               post_chance_level = "overall",
                               post_metric_names = c("BRT.rt_mean.correct",
                                                     "SAATIMPULSIVE.rt_mean.correct",
                                                     "SAATSUSTAINED.rt_mean.correct",
                                                     "TNT.rt_mean.correct",
                                                     "object_count_span.overall",
                                                     "FILTER.k",
                                                     "COLORSELECTION.max_delay_time.correct.strict",
                                                     "rcs.overall"),
                               post_metric_names_exclude = c("FILTER.rcs",
                                                             "TNT.rt_mean.correct.")) {
  if(data_type == "explorer") {
    app_type <- "explorer"
    post_reduce_cols_demos <- c("pid",
                                "age",
                                "handedness",
                                "bid")
  } else {
    app_type <- "classroom"
    post_reduce_cols_demos <- c("pid",
                                "age",
                                "handedness")
    post_metric_names <- c(post_metric_names, ".bid", ".time")
  }
  
  out <- load_ace_bulk(path_in, verbose = verbose, data_type = data_type)
  if (verbose) message(crayon::blue("Data loaded in from CSVs"))
  
  out %<>%
    trim_rt_trials_range(cutoff_min = rt_cutoff_min, verbose = verbose)
  if (verbose) message(crayon::blue("Trials with RT <", rt_cutoff_min, "ms NA'd out"))
  
  out %<>%
    proc_by_module(app_type = app_type,
                   output = "wide",
                   verbose = verbose)
  if (verbose) message(crayon::blue("Summary metrics processed in wide format"))
  
  if (!is.na(post_min_trials)) {
    out %<>%
      post_clean_low_trials(min_trials = post_min_trials, app_type = app_type)
    if (verbose) message(crayon::blue("Records with <", post_min_trials, " trials NA'd out"))
  }
  
  if (post_chance_level != "none") {
    if (post_chance_level == "overall") {
      post_clean_chance_overall <- TRUE
    } else if (post_chance_level == "easy") {
      post_clean_chance_overall <- FALSE 
    }
    out %<>%
      post_clean_chance(app_type = app_type,
                        overall = post_clean_chance_overall,
                        cutoff_dprime = 0,
                        cutoff_2choice = 0.5,
                        cutoff_4choice = 0.25,
                        cutoff_5choice = 0.2)
    if (verbose) message(crayon::blue("Records with below-chance performance NA'd out using setting:", post_chance_level))
  } else {
    if (verbose) message(crayon::blue("SKIPPED trimming of records with below-chance performance"))
  }
  
  out %<>%
    post_reduce_cols(demo_names = post_reduce_cols_demos,
                     metric_names = post_metric_names,
                     metric_names_exclude = post_metric_names_exclude)
  if (verbose) message(crayon::blue("Output columns trimmed down to key metrics"))
  
  if (!is.null(path_out)) {
    out_name <- paste0(path_out, "/ace_averaged_data_", Sys.Date(), ".csv")
    if (verbose) message(crayon::green("Writing out data to", out_name))
    write_csv(out, file = out_name)
  } else {
    if (verbose) message(crayon::green("NOT writing processed data to CSV, returning tibble only"))
  }
  
  if (verbose) message(crayon::magenta("Warning messages below may have come from any stage in the process.",
                                       "\n",
                                       "You can probably disregard these, but they may reflect issues in the data."))
  
  return (out)
}
