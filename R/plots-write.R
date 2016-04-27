
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