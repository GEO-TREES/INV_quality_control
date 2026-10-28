#' Flag stems below the minimum diameter threshold of their plot
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param plot_id column name(s) in 'x' identifying plots, matching 
#'     `plot_plot_id` in `plot`
#' @param diam `r param_diam()`
#' @param plot `r param_x_plot()`
#' @param plot_diam_min column name in `plot` with the minimum diameter 
#'     threshold of each plot
#' @param plot_plot_id column name(s) in `plot` identifying plots. Defaults 
#'     to `plot_id`
#' @param cond `r param_cond()`
#' @param comment `r param_comment()`
#'
#' @details
#' In the GEO-TREES INV_processing workflow, stems below the minimum diameter
#' threshold of the plot (`meas_diam_min_cm`) have their own AGB estimates, 
#' but are not included in AGB density, so that AGB density matches the 
#' sampling of the plot. Many stems below the threshold may indicate that the
#' threshold in the plot meta-data is wrong, or that diameters are in the 
#' wrong units.
#'
#' @return dataframe with values of `meas_id` for stems below the minimum 
#'     diameter threshold, with the diameter and the threshold
#' 
#' @examples
#' stem <- data.frame(
#'   id = 1:4,
#'   plot_id = c("A", "A", "B", "B"),
#'   dbh = c(4, 12, 8, 12)
#' )
#' plot <- data.frame(plot_id = c("A", "B"), diam_min = c(5, 10))
#' 
#' flagDiamMin(stem, "id", "plot_id", "dbh", plot, "diam_min")
#' 
#' @export
#' 
flagDiamMin <- function(x, meas_id, plot_id, diam, plot, plot_diam_min, 
  plot_plot_id = plot_id, cond = NULL, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, plot_id, diam)
  columnCatch(plot, plot_plot_id, plot_diam_min)

  x[[diam]] <- classCatch(x[[diam]], "numeric")
  plot[[plot_diam_min]] <- classCatch(plot[[plot_diam_min]], "numeric")

  if (!is.null(cond) && nrow(x) != length(cond)) {
    stop("'cond' must be the same length as the number of rows in 'x'")
  }

  if (is.null(cond)) {
    in_scope <- rep(TRUE, nrow(x))
  } else {
    in_scope <- cond
    in_scope[is.na(in_scope)] <- FALSE
  }

  # Match plot threshold to stems
  x_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))
  plot_key <- do.call(paste, c(plot[, plot_plot_id, drop = FALSE], sep = "::"))

  if (any(duplicated(plot_key))) {
    stop("Values of 'plot_plot_id' must be unique in 'plot'")
  }

  out <- x[, unique(c(meas_id, plot_id, diam)), drop = FALSE]
  out$diam_min <- plot[[plot_diam_min]][match(x_key, plot_key)]

  if (any(is.na(out$diam_min))) {
    message(sum(is.na(out$diam_min)), 
      " stems in plots with no minimum diameter threshold in 'plot'")
  }

  # Filter to stems below threshold
  out <- out[in_scope & !is.na(out[[diam]]) & !is.na(out$diam_min) & 
    out[[diam]] < out$diam_min, , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Stems below minimum diameter threshold detected")
  }

  # Return
  return(out)
}
