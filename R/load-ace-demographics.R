
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

# TODO: this doesn't go here!!!!!!!!!

#' @keywords internal

subset_first_block = function(df) {
  sorted_bid = df[order(as.character(df[, COL_BID])), ]
  sub_pid = subset_by_col(sorted_bid, COL_PID)
  out = data.frame()
  for (sub in sub_pid) {
    out = plyr::rbind.fill(out, sub[1, ])
  }
  return (out)
}