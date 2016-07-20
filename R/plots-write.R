
#' Export plot
#'
#' Write plot to file
#' 
#' @export
#' @param plot a plot
#' @param file a character string giving the name of the file.

export_plot <- function(plot, file) {
  pdf(file)
  print(plot)
  dev.off()
}

#' @keywords internal

pretty_title <- function(ugly_title) {
  out = remove_special_characters(ugly_title, " ")
  out = to_title_case(out)
  return (out)
}