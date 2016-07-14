
#' @export
#' @inheritParams ggplot2::ggplot

make_box_plot <- function(df, x, y, title, xlab, ylab, cohort = NULL, ...) {
  if (missing(title)) {
    title = paste(y, "by", x, sep = " ")
  }
  if (missing(xlab)) {
    xlab = x
  }
  if (missing(ylab)) {
    ylab = y
  }
  boxplot = ggplot2::ggplot(df, 
    ggplot2::aes(x = df[, x], y = df[, y])) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle(title) + 
    ggplot2::xlab(xlab) + 
    ggplot2::ylab(ylab)
  if (!is.null(cohort)) {
    boxplot = boxplot + ggplot2::aes(fill = df[, cohort])
  }
  return (boxplot)
}

#' Make box plots in bulk
#'
#' A wrapper function around \code{\link{make_box_plot}}.
#'
#' @export
#' @param x the name of x variable
#' @param y a character vector containing a list of y variables to make plots of
#' @param title_prefix Added to every plot title if \code{!is.null(path)}.
#' @param file_prefix Added to every plot title if \code{!is.null(path)}.
#' @param export Write plots to pdf?
#' @param path Directory to write plots to.

make_box_plot_bulk <- function(df, x, y = c(), title_prefix = "", file_prefix = "", path = NULL, export = FALSE) {
  plots = list()
  for (i in 1:length(y)) {
    yval = y[i]
    plot_desc = paste(yval, "by", pretty_title(x), sep = " ")
    plot_title = paste(title_prefix, ":", plot_desc, sep = " ")
    plot = make_box_plot(df, x, yval, title = plot_title)
    if (!is.null(path)) {
      file_name = paste0(path, "/", file_prefix, "_", yval, ".pdf")
    } else {
      file_name = paste0(file_prefix, "_", yval, ".pdf")
    }
    plots[[i]] = plot
    if (export) {
      export_plot(plot, file_name)
    }
  }
  return (plots)
}
