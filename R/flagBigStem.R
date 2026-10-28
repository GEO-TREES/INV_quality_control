#' Flag stems which contribute a large proportion of plot basal area
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param plot_id column name(s) in 'x' defining groups within which basal 
#'     area is summed, e.g. `c("plot_id", "census_id")`
#' @param diam `r param_diam()`
#' @param threshold proportion (0 to 1) of group basal area above which a 
#'     stem is flagged
#' @param cond `r param_cond()`
#' @param comment `r param_comment()`
#'
#' @details
#' Above-ground biomass scales with roughly the square of diameter or more, 
#' so a single large stem can make up a substantial proportion of plot 
#' biomass. A data entry error in the diameter of a large stem, e.g. 250 cm 
#' instead of 25.0 cm, can therefore dominate plot AGB density. Stems flagged
#' by this function are not necessarily errors, but their measurements 
#' deserve careful checking. 
#'
#' @return dataframe with values of `meas_id` for stems with a basal area 
#'     share greater than `threshold`, with the share and the rank of the stem
#'     within its group
#' 
#' @examples
#' df <- data.frame(
#'   id = 1:6,
#'   plot_id = "A",
#'   dbh = c(10, 12, 15, 11, 250, 20)
#' )
#' 
#' flagBigStem(df, "id", "plot_id", "dbh", threshold = 0.2)
#' 
#' @export
#' 
flagBigStem <- function(x, meas_id, plot_id, diam, threshold = 0.05, 
  cond = NULL, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, plot_id, diam)

  x[[diam]] <- classCatch(x[[diam]], "numeric")

  if (!is.null(cond) && nrow(x) != length(cond)) {
    stop("'cond' must be the same length as the number of rows in 'x'")
  }

  if (!is.null(cond)) {
    cond[is.na(cond)] <- FALSE
    x <- x[cond, , drop = FALSE]
  }

  # Calculate basal area share within groups
  ba <- x[[diam]]^2
  ba[is.na(ba)] <- 0
  group_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))
  ba_group <- ave(ba, group_key, FUN = sum)

  out <- x[, unique(c(meas_id, plot_id, diam)), drop = FALSE]
  out$ba_share <- ba / ba_group
  out$ba_rank <- ave(-ba, group_key, FUN = function(i) { 
    rank(i, ties.method = "min") 
  })

  # Filter to stems above threshold
  out <- out[!is.na(out$ba_share) & out$ba_share > threshold, , drop = FALSE]
  out <- out[order(-out$ba_share), , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Stems with large basal area share detected")
  }

  # Return
  return(out)
}
