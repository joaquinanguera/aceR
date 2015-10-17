
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
  # read raw file
  num_cols = max(count.fields(file, sep = ','))
  raw_dat = read.csv(file, header = FALSE, row.names = NULL, col.names = seq_len(num_cols), fill = TRUE)
  raw_dat[is.na(raw_dat)] = ""
  raw_dat = remove_empty_cols(raw_dat)
  # identify & filter the session header
  info = identify_block_info(raw_dat)
  dat = filter_block_info(raw_dat, info)
  # indentify sub sections
  sections = identify_sections(dat)
  # convert subsections into data.frame
  out = data.frame()
  for (section in sections) {
    sub = dat[section$data_start : section$data_end, ]
    names(sub) = unname(unlist(dat[section$header, ]))
    if (!is.na(section$category)) {
      sub$category = dat[section$category, ][[1]]
    } 
    out = plyr::rbind.fill(out, sub)
  }
  # add file info
  row.names(out) <- NULL
  out = merge(out, info)
  out$file = file
  return (out)
}

#' @keywords internal

identify_block_info <- function(dat) {
  first_elements = na.omit(head(dat[1], n = 10L)[dat[1] == ""])
  raw_file_header = dat[1:length(first_elements), ]
  file_header = remove_empty_cols(raw_file_header)
  colnames(file_header) = NULL
  file_header = as.data.frame(t(file_header))
  names(file_header) = as.character(unlist(file_header[1, ]))
  return (file_header[-1, ])
}

#' @keywords internal

filter_block_info <- function(dat, block_info) {
  num = length(block_info)
  sub = dat[-(1:num), ]
  row.names(sub) = NULL
  return (sub)
}

#' @keywords internal 
identify_sections <- function (dat) {
  possible = identify_possible_starting_rows(dat)
  num = length(possible) - 1
  sections = list()
  for (i in 1:num) {
    start = possible[i]
    end = possible[i + 1]
    if (end - start > 1) {
      section = list()
      has_identifier = length(which(dat[start, ] != "")) == 1
      section$category = ifelse(has_identifier, start, NA)
      section$header = ifelse(has_identifier, start + 1, start)
      section$data_start = ifelse(has_identifier, start + 2, start + 1)
      section$data_end = end - 1
      sections = c(sections, list(section))
    }
  }
  return (sections)
}

#' @keywords internal

identify_possible_starting_rows <- function(dat) {
  possible_starting_rows = apply(dat, 1, function(x) {
    num_empty_cells = length(x[x == ""])
    return (num_empty_cells > 1)
  })
  possible_starting_rows[c(1, length(possible_starting_rows))] = TRUE
  return (which(possible_starting_rows))
}