#' Flag plot corner coordinates which are inconsistent with the stem map
#'
#' @param pt `r param_x_pt()`
#' @param plot_id column name in `pt` with plot IDs
#' @param point_id column name in `pt` with point IDs
#' @param x_rel `r param_xy_rel("X", "pt")`
#' @param y_rel `r param_xy_rel("Y", "pt")`
#' @param x_proj column name in `pt` with projected X coordinates (m), e.g. 
#'     UTM easting
#' @param y_proj column name in `pt` with projected Y coordinates (m), e.g. 
#'     UTM northing
#' @param threshold maximum distance (m) between the measured and fitted 
#'     projected coordinates of a point. Equivalent to 
#'     `corner_outlier_dist_m` in the GEO-TREES INV_processing workflow
#' @param scale_tol maximum difference from 1 of the fitted scale between 
#'     relative and projected coordinates
#' @param comment `r param_comment()`
#'
#' @details
#' For each plot, a similarity transformation (translation, rotation and 
#' uniform scale) is fitted by least squares from the relative coordinates 
#' of the points to their projected coordinates. Points with large residuals
#' are removed one at a time, starting with the largest, while at least 
#' three points remain, and the transformation is refitted. Points with 
#' residuals greater than `threshold` may be wrongly labelled corners, 
#' have swapped coordinates, or have large GNSS errors. 
#'
#' The fitted scale should be close to 1. A scale less than 1 is expected 
#' where plot dimensions were measured along the slope rather than as 
#' horizontal distances, see `plot_planar` in the plot meta-data table. 
#' Scales far from 1 suggest that relative or projected coordinates are in 
#' the wrong units, or the wrong coordinate reference system.
#'
#' In the GEO-TREES INV_processing workflow, a polygon is fitted to the plot 
#' corners, and a warning is given if its area differs from the area defined
#' by the relative coordinates by more than 5%, which is approximately a 
#' scale difference of 2.5%.
#'
#' @return dataframe with points in plots with a scale outside `scale_tol`, 
#'     or with residuals greater than `threshold`, with the residual (m), and
#'     the fitted scale and rotation (degrees anticlockwise) of the plot
#' 
#' @examples
#' pt <- data.frame(
#'   plot_id = "A",
#'   point_id = c("SW", "SE", "NE", "NW"),
#'   x_rel = c(0, 100, 100, 0),
#'   y_rel = c(0, 0, 100, 100),
#'   e = c(500000, 500100, 500100, 500030),
#'   n = c(8000000, 8000000, 8000100, 8000100)
#' )
#' 
#' flagPlotCorner(pt, "plot_id", "point_id", "x_rel", "y_rel", "e", "n")
#' 
#' @export
#' 
flagPlotCorner <- function(pt, plot_id, point_id, x_rel, y_rel, x_proj, y_proj, 
  threshold = 10, scale_tol = 0.05, comment = NULL) {

  # Check input
  columnCatch(pt, plot_id, point_id, x_rel, y_rel, x_proj, y_proj)

  for (i in c(x_rel, y_rel, x_proj, y_proj)) {
    pt[[i]] <- classCatch(pt[[i]], "numeric")
  }

  pt <- pt[!is.na(pt[[x_rel]]) & !is.na(pt[[y_rel]]) & 
    !is.na(pt[[x_proj]]) & !is.na(pt[[y_proj]]), , drop = FALSE]

  out <- do.call(rbind, lapply(split(pt, pt[[plot_id]]), function(y) {
    if (nrow(y) < 3) {
      return(NULL)
    }

    # Represent coordinates as complex numbers, so that a similarity 
    # transformation is w = a * z + b, where a = scale * exp(i * rotation)
    z <- complex(real = y[[x_rel]], imaginary = y[[y_rel]])
    w <- complex(real = y[[x_proj]], imaginary = y[[y_proj]])

    fit <- function(use) {
      zc <- z[use] - mean(z[use])
      wc <- w[use] - mean(w[use])
      a <- sum(wc * Conj(zc)) / sum(Mod(zc)^2)
      b <- mean(w[use]) - a * mean(z[use])
      list(a = a, resid = Mod(w - (a * z + b)))
    }

    # Iteratively remove outliers
    use <- rep(TRUE, length(z))
    f <- fit(use)
    while (sum(use) > 3 && max(f$resid[use]) > threshold) {
      use[use][which.max(f$resid[use])] <- FALSE
      f <- fit(use)
    }

    res <- y[, c(plot_id, point_id), drop = FALSE]
    res$residual_m <- f$resid
    res$scale <- Mod(f$a)
    res$rotation_deg <- Arg(f$a) * 180 / pi
    res
  }))

  if (is.null(out)) {
    out <- pt[0, c(plot_id, point_id), drop = FALSE]
  } else {
    out <- out[out$residual_m > threshold | abs(out$scale - 1) > scale_tol, , drop = FALSE]
  }
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Plot corner coordinates inconsistent with stem map detected")
  }

  # Return
  return(out)
}
