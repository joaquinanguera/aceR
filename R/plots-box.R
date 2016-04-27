
#' @export
#' @inheritParams ggplot2::ggplot

make_box_plot <- function(df, x, y, title, xlab, ylab, ...) {
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
  return (boxplot)
}