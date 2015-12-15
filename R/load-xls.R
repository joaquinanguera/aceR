
#' @keywords internal

load_excel <- function(file, sheet = "Pre Raw") {
  wk = XLConnect::loadWorkbook(file)
  df = XLConnect:::readWorksheet(wk, sheet = sheet, header = FALSE)
  return (df)
}

#' @keywords internal

is_excel <- function (filename) {
  return (grepl("xls", filename))
}