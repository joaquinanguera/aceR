
#' Export plot
#'
#' Write plot to file
#' 
#' @export
#' @inheritParams base::write.csv
#' @param plot a plot

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