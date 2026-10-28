#' Flag implausible changes in plot-level totals between censuses
#'
#' @param x `r param_x_stem()`
#' @param plot_id column name(s) in `x` identifying plots, or other spatial 
#'     units within which totals are compared, e.g. `c("plot_id", 
#'     "subplot_id")`
#' @param census_id `r param_census_id()`
#' @param census_date `r param_census_date()`, or decimal years
#' @param target optional, column name in `x` with values to sum, e.g. 
#'     basal area or AGB of each stem. If `NULL`, the number of stems is used
#' @param cond `r param_cond()` E.g. only live stems
#' @param threshold maximum absolute annual relative change, e.g. 0.05 for 
#'     5% per year
#' @param comment `r param_comment()`
#'
#' @details
#' Plot-level totals such as stem density, basal area and AGB normally change
#' slowly in the absence of major disturbance. For each plot, totals are 
#' compared between consecutive censuses, and the annual relative change is 
#' calculated as `(total_t1 / total_t0)^(1 / years) - 1`. Large changes may 
#' indicate that part of the plot was not measured, that stems were 
#' duplicated, that diameters are in the wrong units, or that a disturbance 
#' event occurred which should be described in the plot meta-data.
#'
#' @return dataframe with values of `plot_id` and `census_id` with an annual 
#'     relative change greater than `threshold` since the previous census, 
#'     with the totals, the previous census, and the years between censuses
#' 
#' @examples
#' df <- data.frame(
#'   plot_id = rep(c("A", "B"), each = 300),
#'   census_id = rep(rep(1:3, each = 100), 2),
#'   census_date = rep(rep(c("2010", "2015", "2020"), each = 100), 2),
#'   ba = 0.05
#' )
#' # Half of plot B not measured in census 3
#' df <- df[!(df$plot_id == "B" & df$census_id == 3)[seq_len(600)] | 
#'   seq_len(600) %% 2 == 0, ]
#' 
#' flagPlotChange(df, "plot_id", "census_id", "census_date", target = "ba")
#' 
#' @export
#' 
flagPlotChange <- function(x, plot_id, census_id, census_date, target = NULL, 
  cond = NULL, threshold = 0.05, comment = NULL) {

  # Check input
  columnCatch(x, plot_id, census_id, census_date, target)

  if (!is.null(cond)) {
    if (length(cond) != nrow(x)) {
      stop("'cond' must be the same length as the number of rows in 'x'")
    }
    cond[is.na(cond)] <- FALSE
    x <- x[cond, , drop = FALSE]
  }

  # Sum values for each plot census
  x$..value.. <- if (is.null(target)) 1 else classCatch(x[[target]], "numeric")
  x$..date.. <- decYear(x[[census_date]])
  tot <- stats::aggregate(
    list(total = x$..value..), 
    by = x[, c(plot_id, census_id), drop = FALSE], 
    FUN = sum, na.rm = TRUE)
  tot$date <- stats::aggregate(
    list(date = x$..date..), 
    by = x[, c(plot_id, census_id), drop = FALSE], 
    FUN = stats::median, na.rm = TRUE)$date

  # Compare to the previous census of each plot
  plot_key <- do.call(paste, c(tot[, plot_id, drop = FALSE], sep = "::"))
  tot <- tot[order(plot_key, censusOrder(tot[[census_id]])), , drop = FALSE]
  plot_key <- do.call(paste, c(tot[, plot_id, drop = FALSE], sep = "::"))
  prev <- c(NA, seq_len(nrow(tot) - 1))
  prev[!duplicated(plot_key)] <- NA

  tot$census_prev <- tot[[census_id]][prev]
  tot$total_prev <- tot$total[prev]
  tot$years <- tot$date - tot$date[prev]
  tot$change_ann <- (tot$total / tot$total_prev)^(1 / tot$years) - 1
  tot$date <- NULL

  out <- tot[!is.na(tot$change_ann) & abs(tot$change_ann) > threshold, , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Large changes in plot totals between censuses detected")
  }

  # Return
  return(out)
}
