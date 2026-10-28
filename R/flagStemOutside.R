#' Flag stems located outside the boundary of their plot
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param plot_id column name in `x` and `pt` with plot IDs
#' @param x_rel `r param_xy_rel("X")`
#' @param y_rel `r param_xy_rel("Y")`
#' @param pt `r param_x_pt()`
#' @param pt_x_rel `r param_xy_rel("X", "pt")`. Defaults to `x_rel`
#' @param pt_y_rel `r param_xy_rel("Y", "pt")`. Defaults to `y_rel`
#' @param pt_corner optional, column name in `pt` with logical values 
#'     indicating whether each point is a plot corner. If `NULL`, all points
#'     are used to define the plot boundary
#' @param tol distance (m) outside the plot boundary within which stems are 
#'     not flagged
#' @param comment `r param_comment()`
#'
#' @details
#' The plot boundary is defined as the convex hull of the plot corners in 
#' the relative coordinate system of the stems. In the GEO-TREES 
#' INV_processing workflow, stems outside the quadrat grid of their plot are 
#' excluded from quadrat AGB density, unless they are within 
#' `outside_max_dist_m` of a quadrat. Stems outside the plot may have 
#' swapped X and Y coordinates, coordinates relative to the wrong origin, or
#' coordinates in the wrong units.
#'
#' @return dataframe with values of `meas_id` for stems outside the plot 
#'     boundary, with the distance (m) to the boundary
#' 
#' @examples
#' stem <- data.frame(
#'   id = 1:4,
#'   plot_id = "A",
#'   x = c(10, 50, 105, 30),
#'   y = c(10, 50, 50, -20)
#' )
#' pt <- data.frame(
#'   plot_id = "A",
#'   x = c(0, 100, 100, 0),
#'   y = c(0, 0, 100, 100)
#' )
#' 
#' flagStemOutside(stem, "id", "plot_id", "x", "y", pt)
#' 
#' @export
#' 
flagStemOutside <- function(x, meas_id, plot_id, x_rel, y_rel, pt, 
  pt_x_rel = x_rel, pt_y_rel = y_rel, pt_corner = NULL, tol = 0, 
  comment = NULL) {

  # Check input
  columnCatch(x, meas_id, plot_id, x_rel, y_rel)
  columnCatch(pt, plot_id, pt_x_rel, pt_y_rel, pt_corner)

  if (!is.null(pt_corner)) {
    pt <- pt[pt[[pt_corner]] %in% TRUE, , drop = FALSE]
  }
  pt <- pt[!is.na(pt[[pt_x_rel]]) & !is.na(pt[[pt_y_rel]]), , drop = FALSE]

  # Convex hull of corners for each plot
  hulls <- lapply(split(pt, pt[[plot_id]]), function(y) {
    xy <- unique(cbind(
      classCatch(y[[pt_x_rel]], "numeric"), 
      classCatch(y[[pt_y_rel]], "numeric")))
    if (nrow(xy) < 3) {
      return(NULL)
    }
    xy[grDevices::chull(xy), , drop = FALSE]
  })

  no_hull <- setdiff(unique(x[[plot_id]]), names(hulls)[!vapply(hulls, is.null, logical(1))])
  if (length(no_hull) > 0) {
    message("Plots with fewer than three corners in 'pt' not checked: ", 
      paste(no_hull, collapse = ", "))
  }

  # Distance outside the hull for each stem, 0 if inside
  sx <- classCatch(x[[x_rel]], "numeric")
  sy <- classCatch(x[[y_rel]], "numeric")
  dist_out <- rep(NA_real_, nrow(x))
  for (p in names(hulls)) {
    h <- hulls[[p]]
    i <- which(x[[plot_id]] == p & !is.na(sx) & !is.na(sy))
    if (is.null(h) || length(i) == 0) {
      next
    }
    dist_out[i] <- distOutside(sx[i], sy[i], h)
  }

  out <- x[, unique(c(meas_id, plot_id, x_rel, y_rel)), drop = FALSE]
  out$dist_outside_m <- dist_out
  out <- out[!is.na(out$dist_outside_m) & out$dist_outside_m > tol, , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Stems outside plot boundary detected")
  }

  # Return
  return(out)
}

#' Distance from points to a convex polygon, zero inside
#'
#' @param px numeric vector of point X coordinates
#' @param py numeric vector of point Y coordinates
#' @param h two column matrix of polygon vertices, in order
#'
#' @return numeric vector of distances 
#' 
#' @keywords internal
#' @noRd
#' 
distOutside <- function(px, py, h) {
  n <- nrow(h)
  h2 <- h[c(2:n, 1), , drop = FALSE]

  # Point in polygon by ray casting
  inside <- rep(FALSE, length(px))
  for (k in seq_len(n)) {
    cross <- ((h[k, 2] > py) != (h2[k, 2] > py)) & 
      (px < (h2[k, 1] - h[k, 1]) * (py - h[k, 2]) / (h2[k, 2] - h[k, 2]) + h[k, 1])
    inside <- xor(inside, cross)
  }

  # Minimum distance to any edge
  d <- vapply(seq_len(n), function(k) {
    ex <- h2[k, 1] - h[k, 1]
    ey <- h2[k, 2] - h[k, 2]
    t <- pmin(pmax(((px - h[k, 1]) * ex + (py - h[k, 2]) * ey) / (ex^2 + ey^2), 0), 1)
    sqrt((px - h[k, 1] - t * ex)^2 + (py - h[k, 2] - t * ey)^2)
  }, numeric(length(px)))
  d <- matrix(d, nrow = length(px))

  ifelse(inside, 0, apply(d, 1, min))
}
