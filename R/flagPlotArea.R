#' Calculate plot area from the relative coordinates of plot corners
#'
#' @param pt `r param_x_pt()`
#' @param plot_id column name in `pt` with plot IDs
#' @param x_rel `r param_xy_rel("X", "pt")`
#' @param y_rel `r param_xy_rel("Y", "pt")`
#' @param pt_corner optional, column name in `pt` with logical values 
#'     indicating whether each point is a plot corner. If `NULL`, all points
#'     are used
#'
#' @details
#' The plot area is the area of the convex hull of the plot corners, in the 
#' relative coordinate system of the stems. This is the area used to 
#' calculate AGB density from the sum of stem AGB.
#'
#' @return dataframe with values of `plot_id`, the number of corners, and 
#'     the plot area in hectares
#' 
#' @examples
#' pt <- data.frame(
#'   plot_id = rep(c("A", "B"), each = 4),
#'   x = c(0, 100, 100, 0, 0, 50, 50, 0),
#'   y = c(0, 0, 100, 100, 0, 0, 20, 20)
#' )
#' 
#' calcPlotArea(pt, "plot_id", "x", "y")
#' 
#' @export
#' 
calcPlotArea <- function(pt, plot_id, x_rel, y_rel, pt_corner = NULL) {

  # Check input
  columnCatch(pt, plot_id, x_rel, y_rel, pt_corner)

  if (!is.null(pt_corner)) {
    pt <- pt[pt[[pt_corner]] %in% TRUE, , drop = FALSE]
  }

  out <- do.call(rbind, lapply(split(pt, pt[[plot_id]]), function(y) {
    xy <- unique(cbind(
      classCatch(y[[x_rel]], "numeric"), 
      classCatch(y[[y_rel]], "numeric")))
    xy <- xy[stats::complete.cases(xy), , drop = FALSE]
    res <- y[1, plot_id, drop = FALSE]
    res$n_corner <- nrow(xy)
    if (nrow(xy) < 3) {
      res$area_ha <- NA_real_
    } else {
      # Shoelace formula on the convex hull
      h <- xy[grDevices::chull(xy), , drop = FALSE]
      h2 <- h[c(2:nrow(h), 1), , drop = FALSE]
      res$area_ha <- abs(sum(h[, 1] * h2[, 2] - h2[, 1] * h[, 2])) / 2 / 10000
    }
    res
  }))
  rownames(out) <- NULL

  # Return
  return(out)
}

#' Flag plots where the declared area differs from the area of the corners
#'
#' @param plot `r param_x_plot()`
#' @param pt `r param_x_pt()`
#' @param plot_id column name in `plot` and `pt` with plot IDs
#' @param plot_length column name in `plot` with plot length (m)
#' @param plot_width column name in `plot` with plot width (m)
#' @param x_rel `r param_xy_rel("X", "pt")`
#' @param y_rel `r param_xy_rel("Y", "pt")`
#' @param pt_corner optional, column name in `pt` with logical values 
#'     indicating whether each point is a plot corner
#' @param tol maximum relative difference between the declared area and the 
#'     area of the corners
#' @param comment `r param_comment()`
#'
#' @details
#' Plot area is the denominator of AGB density, so an error in plot area 
#' gives a proportional error in AGB density. The declared area, 
#' `plot_length * plot_width`, is compared to the area of the convex hull of
#' the relative coordinates of the plot corners, see `calcPlotArea()`. 
#' Plots with fewer than three corners are also flagged.
#'
#' @return dataframe with values of `plot_id` where the areas differ by more
#'     than `tol`, with the declared area, the area of the corners, and the 
#'     relative difference
#' 
#' @examples
#' plot <- data.frame(plot_id = c("A", "B"), len = c(100, 100), wid = c(100, 50))
#' pt <- data.frame(
#'   plot_id = rep(c("A", "B"), each = 4),
#'   x = c(0, 100, 100, 0, 0, 100, 100, 0),
#'   y = c(0, 0, 100, 100, 0, 0, 100, 100)
#' )
#' 
#' flagPlotArea(plot, pt, "plot_id", "len", "wid", "x", "y")
#' 
#' @export
#' 
flagPlotArea <- function(plot, pt, plot_id, plot_length, plot_width, x_rel, 
  y_rel, pt_corner = NULL, tol = 0.01, comment = NULL) {

  # Check input
  columnCatch(plot, plot_id, plot_length, plot_width)

  area <- calcPlotArea(pt, plot_id, x_rel, y_rel, pt_corner)

  out <- plot[!duplicated(plot[[plot_id]]), c(plot_id, plot_length, plot_width), 
    drop = FALSE]
  out$area_ha_declared <- classCatch(out[[plot_length]], "numeric") * 
    classCatch(out[[plot_width]], "numeric") / 10000
  out$area_ha_corner <- area$area_ha[match(out[[plot_id]], area[[plot_id]])]
  out$area_diff <- out$area_ha_corner / out$area_ha_declared - 1

  out <- out[is.na(out$area_diff) | abs(out$area_diff) > tol, , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Declared plot area differs from area of plot corners")
  }

  # Return
  return(out)
}
