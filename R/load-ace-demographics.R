
#' Load demographics file from source
#'
#' @export
#' @param file The path to the demographics file name.
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

# TODO: this also doesn't go here + should generalize this. super ugly.

#' @keywords internal

subset_first_block_for_tasks <- function(by_task = list(), verbose = TRUE) {
  all_tasks = data.frame(pid = "dummy")
  module_names = names(by_task)
  for (i in seq(length(module_names))) {
    module = by_task[[i]]
    module_name = module_names[i]
    if (verbose) {
      print(module_name)
    }
    first_block = subset_first_block(module)
    col_names = names(first_block)
    names(first_block) = sapply(col_names, function(x) {
      if (grepl("pid", x)) {
        new_name = x
      } else {
        new_name = paste(module_name, x, sep = "-")
      }
      return (new_name)
    })
    if (i == 1) {
      all_tasks = first_block
    } else {
      all_tasks = plyr::join(all_tasks, first_block, by = "pid")
    }
  }
  return (all_tasks)
}
