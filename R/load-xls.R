
#' @keywords internal

load_excel <- function(file) {
  wk = XLConnect::loadWorkbook(file)
  sheets = sheet_names(wk)
  valid_sheets = tolower(c("Pre Raw", "Post Raw"))
  if (length(sheets) == 1) {
    return (load_sheet(wk, sheet = 1))
  }
  out = data.frame()
  for (sheet in sheets) {
    if (tolower(sheet) %in% valid_sheets) {
      sh = load_sheet(wk, sheet = sheet)
      out = plyr:::rbind.fill(out, sh)
    }
  }
  return (out)
}

#' @keywords internal

load_sheet <- function(wk, sheet) {
  sheet = tryCatch({
    df = XLConnect:::readWorksheet(wk, sheet = sheet, header = FALSE)
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