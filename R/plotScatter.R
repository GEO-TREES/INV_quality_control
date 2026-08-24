#' Create a scatter plot with ggplot2
#' 
#' @param x numeric vector
#' @param y numeric vector
#' @param flag optional, vector of `length(x)` specifying points to highlight
#' @param ... additional arguments passed to `geom_point()`
#'
#' @return ggplot object with scatter plot of `x` and `y`
#'
#' @import ggplot2 
#'
#' @export
#'
plotScatter <- function(x, y, flag = NULL, ...) {

  # Check input
  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector")
  }

  if (!is.numeric(y)) {
    stop("'y' must be a numeric vector")
  }

  if (length(x) != length(y)) { 
    stop("'x' and 'y' must be the same length")
  }

  if (!is.null(flag) && length(x) != length(flag)) { 
    stop("'x' and 'flag' must be the same length")
  }

  # Create dataframe
  dat <- data.frame(x, y)
  
  if (!is.null(flag)) { 
    dat$flag <- flag
  }

  # Create plot
  p <- ggplot(dat, aes(x = x, y = y))

  if (!is.null(flag)) { 
    p <- p + geom_point(aes(colour = flag), ...)
  } else {
    p <- p + geom_point(...)
  }

  # Return
  return(p)
}

