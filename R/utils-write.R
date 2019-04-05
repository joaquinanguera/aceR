
#' Make a directory
#'
#' Create a directory if it doesn't exist
#'
#' @export
#' @inheritParams base::dir.exists
#' @inheritParams base::dir.create

make_directory <- function(path, showWarnings = FALSE) {
  if (!dir.exists(path)) {
    dir.create(file.path(path), showWarnings = showWarnings)
  }
}

#' Export CSV
#'
#' Exports a list of data frames as csv into named directory
#'
#' @export
#' @importFrom purrr walk2
#' @importFrom readr write_csv
#'
#' @param dat a list of data frames to save. If saving out data from a long-form tibble, input
#' the column containing the data to be written, in the format \code{my_output$data}.
#' @param path the named release directory. If directory doesn't exist, attempts to create.
#' @param na What should NAs be written as? Defaults to "". Passed onto \code{\link{readr}[write_csv]}.
#' @inheritParams base::dir.create

export_csv <- function (dat, path = ".", na = "", showWarnings = FALSE) {
  
  if (!dir.exists(path)) dir.create(file.path(path), showWarnings = showWarnings)
  
  walk2(dat, names(dat), function (x, y) {
    write_csv(x, path = paste0(path, "/", y, ".csv"), na = na)
    cat("saving... ", y, "\n")
    }
  )

}

