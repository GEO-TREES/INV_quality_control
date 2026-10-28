#' Infer the boundaries of field subplots from stem coordinates
#'
#' @param x `r param_x_stem()`
#' @param subplot_id column name(s) in `x` uniquely identifying subplots, 
#'     e.g. `c("plot_id", "subplot_id")`
#' @param x_rel `r param_xy_rel("X")`
#' @param y_rel `r param_xy_rel("Y")`
#' @param prob numeric vector of length 2, quantiles of stem coordinates used
#'     to estimate the boundaries of each subplot. Quantiles rather than the
#'     range are used so that a few mislocated stems do not affect the 
#'     boundaries
#' @param snap optional, grid resolution (m). If provided, the boundaries are 
#'     rounded outwards to the nearest multiple of `snap`, e.g. 10 for a grid
#'     of 20 x 20 m subplots
#' @param min_n minimum number of stems with coordinates required to infer 
#'     the boundaries of a subplot
#'
#' @details
#' Where no table of subplot boundaries is available, the boundaries can be 
#' estimated from the coordinates of the stems labelled with each subplot. 
#' The output can be passed to `correctLocSub()` to find stems located 
#' outside their labelled subplot, and to `plotStemMap()` to draw subplots. 
#' In the GEO-TREES INV_processing workflow, trees without coordinates are 
#' placed at random within the extent of the stems with coordinates in the 
#' same field subplot, if there are at least 10 of them, so mislocated stems
#' affect AGB density estimates.
#'
#' @return dataframe with values of `subplot_id`, the inferred boundaries 
#'     `x_min`, `x_max`, `y_min`, `y_max`, and the number of stems used `n`
#' 
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   plot_id = "A",
#'   subplot_id = rep(1:4, each = 50),
#'   x = c(runif(100, 0, 20), runif(100, 20, 40)),
#'   y = rep(c(runif(50, 0, 20), runif(50, 20, 40)), 2)
#' )
#' 
#' inferSubplotBounds(df, c("plot_id", "subplot_id"), "x", "y", snap = 10)
#' 
#' @export
#' 
inferSubplotBounds <- function(x, subplot_id, x_rel, y_rel, 
  prob = c(0.02, 0.98), snap = NULL, min_n = 10) {

  # Check input
  columnCatch(x, subplot_id, x_rel, y_rel)

  x[[x_rel]] <- classCatch(x[[x_rel]], "numeric")
  x[[y_rel]] <- classCatch(x[[y_rel]], "numeric")

  x <- x[!is.na(x[[x_rel]]) & !is.na(x[[y_rel]]), , drop = FALSE]
  x_split <- split(x, as.list(x[, subplot_id, drop = FALSE]), drop = TRUE)

  out <- do.call(rbind, lapply(x_split, function(y) {
    if (nrow(y) < min_n) {
      return(NULL)
    }
    res <- unique(y[, subplot_id, drop = FALSE])[1, , drop = FALSE]
    qx <- stats::quantile(y[[x_rel]], prob, names = FALSE)
    qy <- stats::quantile(y[[y_rel]], prob, names = FALSE)
    if (!is.null(snap)) {
      qx <- c(floor(qx[1] / snap), ceiling(qx[2] / snap)) * snap
      qy <- c(floor(qy[1] / snap), ceiling(qy[2] / snap)) * snap
    }
    res$x_min <- qx[1]
    res$x_max <- qx[2]
    res$y_min <- qy[1]
    res$y_max <- qy[2]
    res$n <- nrow(y)
    res
  }))

  if (is.null(out)) {
    stop("No subplots with at least 'min_n' stems with coordinates")
  }
  rownames(out) <- NULL

  # Generate message
  n_sub <- length(x_split)
  if (nrow(out) < n_sub) {
    message(n_sub - nrow(out), " subplots with fewer than 'min_n' stems not inferred")
  }

  # Return
  return(out)
}
