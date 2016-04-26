
#' Load demographics file from source
#'
#' @export
#' @param file The name demographics file name.
#' @return Returns demographic data

load_ace_demographics <- function(file) {
  demographics = openxlsx::read.xlsx(file, sheet = 1, colNames = TRUE)
  demographics[, COL_PID] = demographics[, 1]
  names(demographics) = tolower(gsub("[.]", "_", names(demographics)))
  return (demographics)
}