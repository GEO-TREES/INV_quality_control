#' Plot a stem map for one or more plots
#'
#' @param x `r param_x_stem()`
#' @param x_rel column name in `x` with relative X coordinates
#' @param y_rel column name in `x` with relative Y coordinates
#' @param plot_id optional, column name in `x` containing plot IDs. If 
#'     provided, plots are drawn in separate facets
#' @param diam optional, column name in `x` with stem diameters, used to 
#'     scale the size of points
#' @param flag optional, vector of `nrow(x)` used to colour points, e.g. the 
#'     comment column from a flag function merged onto `x`. `NA` values are 
#'     drawn in grey and labelled "Not flagged"
#' @param subs optional dataframe with subplot boundaries, drawn as 
#'     rectangles. Must contain columns `x_min`, `x_max`, `y_min`, `y_max`, 
#'     and `plot_id` if plots are drawn in facets
#'
#' @return ggplot object 
#' 
#' @import ggplot2
#' 
#' @examples
#' df <- data.frame(
#'   plot_id = rep(c("A", "B"), each = 50),
#'   x = runif(100, 0, 100),
#'   y = runif(100, 0, 100),
#'   dbh = rlnorm(100, 3, 0.5)
#' )
#' 
#' plotStemMap(df, "x", "y", plot_id = "plot_id", diam = "dbh")
#' 
#' @export
#' 
plotStemMap <- function(x, x_rel, y_rel, plot_id = NULL, diam = NULL, 
  flag = NULL, subs = NULL) {

  # Check input
  columnCatch(x, x_rel, y_rel, plot_id, diam)

  if (!is.null(flag) && length(flag) != nrow(x)) {
    stop("'flag' must be the same length as the number of rows in 'x'")
  }

  # Create dataframe 
  dat <- data.frame(
    x = classCatch(x[[x_rel]], "numeric"), 
    y = classCatch(x[[y_rel]], "numeric"))
  dat$size <- if (!is.null(diam)) classCatch(x[[diam]], "numeric") else 1
  dat$flag <- if (!is.null(flag)) as.character(flag) else NA_character_
  if (!is.null(plot_id)) {
    dat$plot_id <- x[[plot_id]]
  }

  # Draw unflagged stems first so flagged stems are on top
  dat <- dat[order(!is.na(dat$flag)), ]
  dat$flag[is.na(dat$flag)] <- "Not flagged"

  # Create plot
  p <- ggplot(dat, aes(x = x, y = y))

  if (!is.null(subs)) {
    p <- p + geom_rect(data = subs, 
      aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
      inherit.aes = FALSE, fill = NA, colour = "grey50", linewidth = 0.3)
  }

  if (!is.null(flag)) {
    flag_lev <- setdiff(unique(dat$flag), "Not flagged")
    flag_col <- c(grDevices::hcl.colors(max(length(flag_lev), 1), "Dark 3")[
      seq_along(flag_lev)], "grey70")
    names(flag_col) <- c(flag_lev, "Not flagged")
    p <- p + 
      geom_point(aes(size = size, colour = flag), shape = 21, na.rm = TRUE) + 
      scale_colour_manual(values = flag_col, name = NULL)
  } else {
    p <- p + geom_point(aes(size = size), shape = 21, colour = "grey30", na.rm = TRUE)
  }

  if (!is.null(diam)) {
    p <- p + scale_size_area(max_size = 4, name = diam)
  } else {
    p <- p + scale_size_identity()
  }

  p <- p + 
    coord_equal() + 
    labs(x = x_rel, y = y_rel)

  if (!is.null(plot_id)) {
    p <- p + facet_wrap(~plot_id)
  }

  # Return
  return(p)
}
