
#' List the files in a directory/folder
#'
#' Wrapper around \code{\link{list.files}()}
#'
#' @export
#' @inheritParams base::list.files
#' @return Returns a character vector containing the names of the files 
#'   in the specified directory.

files_in_directory <- function(path = ".", pattern = ".csv", recursive = TRUE) {
  files = list.files(path = path, pattern = pattern, recursive = recursive)
  return (files)
}

#' @export

read_raw_csv <- function(file) {
  raw_dat = read.csv(file, header = FALSE, row.names = NULL)
  info = parse_block_info(raw_dat)
  dat = remove_block_info(raw_dat, info)
  sub_blocks = identify_sub_block_starting_rows(dat)
  num_total_rows = nrow(raw_dat)
  num_sub_blocks = length(sub_blocks)
  out = data.frame()
  for(i in 1:num_sub_blocks) {
    sub_block_desc = sub_blocks[i]
    sub_block_col_names = sub_block_desc + 1
    sub_block_data_start = sub_block_desc + 2
    sub_block_data_end = ifelse(i == num_sub_blocks, num_total_rows, sub_blocks[i + 1] - 1)
    sub_block = raw_dat[sub_block_data_start:sub_block_data_end, ]
    names(sub_block) = unname(unlist(raw_dat[sub_block_col_names, ]))
    sub_block$category = raw_dat[sub_block_desc, ][[1]]
    out = plyr::rbind.fill(out, sub_block)
  }
  row.names(out) <- NULL
  out = merge(out, info)
  out$file = file
  return (out)
}

#' @keywords internal

parse_block_info <- function(dat) {
  first_elements = na.omit(head(dat[1], n = 10L)[dat[1] == ""])
  raw_file_header = dat[1:length(first_elements), ]
  file_header = remove_empty_cols(raw_file_header)
  colnames(file_header) = NULL
  file_header = as.data.frame(t(file_header))
  names(file_header) = as.character(unlist(file_header[1, ]))
  return (file_header[-1, ])
}

#' @keywords internal

remove_block_info <- function(dat, block_info) {
  num = length(block_info)
  return (dat[-(1:num), ])
}

#' @keywords internal

identify_sub_block_starting_rows <- function(dat) {
  matching_rows = apply(dat, 1, function(x) {
    num_empty_cells = length(x[x == ""])
    return (num_empty_cells > 1)
  })
  raw_indices = names(which(matching_rows))
  return (as.numeric(as.character(raw_indices)))
}