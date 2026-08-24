#' Create a histogram with ggplot2
#' 
#' @param x numeric vector 
#' @param ... additional arguments passed to `geom_histogram()`
#'
#' @return ggplot object with histogram of `x`
#'
#' @import ggplot2 
#'
#' @export
#'
plotHistogram <- function(x, ...) {

  # Check input
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector")
  }

  # Create dataframe
  dat <- data.frame(x)

  # Create plot
  p <- ggplot(dat, aes(x = x)) +
    geom_histogram(...)

  # Return
  return(p)
}
