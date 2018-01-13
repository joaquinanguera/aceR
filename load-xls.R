
#' @keywords internal

load_excel <- function(file) {
  wk = XLConnect::loadWorkbook(file)
  sheets = sheet_names(wk)
  valid_sheets = tolower(c("Pre Raw", "Post Raw"))
  if (length(sheets) == 1) {
    return (load_sheet(wk, sheet = 1))
  }
  dfs = list()
  for (sheet in sheets) {
    if (tolower(sheet) %in% valid_sheets) {
      dfs[[sheet]] = load_sheet(wk, sheet = sheet)
    }
  }
  return (dfs)
}

#' @keywords internal

load_sheet <- function(wk, sheet) {
  sheet = tryCatch({
    df = XLConnect::readWorksheet(wk, sheet = sheet, header = FALSE)
    return (df)
  }, error = function(e) {
    return (data.frame())
  })
  return (sheet)
}

#' @keywords internal

sheet_names <- function(wk) {
  return (XLConnect::getSheets(wk))
}

#' @keywords internal

is_excel <- function (filename) {
  return (grepl("xls", filename))
}