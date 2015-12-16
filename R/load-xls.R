
#' @keywords internal

load_excel <- function(file, sheet = "Post Raw") {
  wk = XLConnect::loadWorkbook(file)
  sheet = tryCatch({
    df = XLConnect:::readWorksheet(wk, sheet = sheet, header = FALSE)
    return (df)
  }, error = function(e) {
    df = XLConnect:::readWorksheet(wk, sheet = 1, header = FALSE)
    return (df)
  })
  return (sheet)
}

#' @keywords internal

is_excel <- function (filename) {
  return (grepl("xls", filename))
}