
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
#' @param post_metric_names a character vector containing \emph{partial} names of
#' metric columns to include in the processed output. All column names containing
#' any of the inputs will be included in output.
#' @return Returns a \code{\link[tibble]{tibble}} containing a cleaned selection of
#' summary statistics from every module in the data.

proc_ace_complete <- function (path_in,
                               path_out = paste(path_in, "..", sep = "/"),
                               verbose = TRUE,
                               data_type = c("explorer", "email", "pulvinar"),
                               post_metric_names = c("BRT.rt_mean.correct",
                                                     "SAAT.rt_mean.correct",
                                                     "TNT.rt_mean.correct",
                                                     "object_count_span.overall",
                                                     "FILTER.k",
                                                     "rcs.overall")) {
  if(data_type == "explorer") {
    app_type <- "explorer"
  } else {
    app_type <- "classroom"
  }
  
  out <- load_ace_bulk(path_in, verbose = verbose, data_type = data_type)
  if (verbose) message(crayon::blue("Data loaded in from CSVs"))
  
  out %<>%
    trim_rt_trials_range(cutoff_min = 200, verbose = verbose)
  if (verbose) message(crayon::blue("Trials with RT < 200 ms NA'd out"))
  
  out %<>%
    proc_by_module(app_type = app_type,
                   output = "wide",
                   verbose = verbose)
  if (verbose) message(crayon::blue("Summary metrics processed in wide format"))
  
  out %<>%
    post_clean_low_trials(min_trials = 5)
  if (verbose) message(crayon::blue("Records with <5 trials NA'd out"))
  
  out %<>%
    post_clean_chance(app_type = app_type,
                      overall = TRUE,
                      cutoff_dprime = 1,
                      cutoff_2choice = 0.5,
                      cutoff_4choice = 0.25)
  if (verbose) message(crayon::blue("Records with below-chance performance NA'd out"))
  
  out %<>%
    post_reduce_cols(demo_names = c("pid",
                                    "age",
                                    "handedness",
                                    "bid"),
                     metric_names =  post_metric_names)
  if (verbose) message(crayon::blue("Output columns trimmed down to key metrics"))
  
  if (!is.null(path_out)) {
    out_name <- paste0(path_out, "/ace_averaged_data_", Sys.Date(), ".csv")
    if (verbose) message(crayon::green(paste("Writing out data to", out_name)))
    write_csv(out, file = out_name)
  } else {
    if (verbose) message(crayon::green("NOT writing processed data to CSV, returning tibble only"))
  }
  
  if (verbose) message(crayon::magenta("Warning messages below may have come from any stage in the process.",
                                       "\n",
                                       "You can probably disregard these, but they may reflect issues in the data."))
  
  return (out)
}
