
#' Export CSV
#'
#' Exports a list of data frames as csv into named directory
#'
#' @export
#' @param dat a list of data frames to save
#' @param path the named release directory

export_csv <- function (dat, path = ".", showWarnings = FALSE) {
  names = names(dat)
  for (i in names) {
    sub = as.data.frame(dat[i])
    strip = paste(i, ".", sep = "")
    names(sub) = sapply(names(sub), function (x) gsub(strip, "", x))
    file = paste(i, ".csv", sep = "")
    if (path != ".") {
      file = paste(path, file, sep = "/")
      if (!dir.exists(path)) {
        dir.create(file.path(path), showWarnings = showWarnings)
      }
    }
    print(paste("saving...", file))
    write.csv(sub, file, na = "")
  }
}