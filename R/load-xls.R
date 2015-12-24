
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
  df_lens = as.vector(sapply(dfs, length))
  equal_cols = all_equal_in_vec(df_lens)
  if (equal_cols) {
    # handle normal case
    out = flatten_df_list(dfs)
  } else {
    # handle the case where data across sheets is inconsistent
    out = data.frame()
    for (df in dfs) {
      names(df) = df[identify_first_header(df), ]
      out = plyr:::rbind.fill(out, df)
    }
    # clean up 
    names(out) = NULL
    out = data.frame(out)
    sh_rows = identify_shifted_rows(out)
    for (r in sh_rows) {
      out[r, 3] = out[r, 2]
      out[r, 2] = NA
    }
  }
  return (data.frame(out))
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

#' @keywords internal

identify_first_header <- function(dat) {
  df = replace_nas(dat, "")
  matching_rows = df[df[1] != "" & df[2] != "" & df[3] != "", ]
  row_names = numeric_row_names(matching_rows)
  return (row_names[1])
}

#' @keywords internal

identify_shifted_rows <- function (dat) {
  df = replace_nas(dat, "")
  matching_rows = df[df[1] == "" & df[2] != "" & df[3] == "", ]
  return (numeric_row_names(matching_rows))
}