#' Flag measurement dates which are invalid or inconsistent within plots
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param plot_id column name(s) in 'x' identifying plots, or plot censuses 
#'     for multi-census data, e.g. `c("plot_id", "census_id")`
#' @param date column name in `x` with measurement dates. Either YYYY-MM-DD, 
#'     YYYY-MM, or YYYY formatted character dates, `Date` objects, or decimal
#'     years, see `decYear()`
#' @param ref_date optional, reference date, e.g. the date of the airborne 
#'     LiDAR (ALS) acquisition matched to the tree inventory data. Same 
#'     formats as `date`
#' @param max_diff maximum difference in years between `date` and `ref_date`
#' @param max_span maximum span in years of measurement dates within a plot. 
#'     Measurements further than half of this from the median date of their 
#'     plot are flagged
#' @param date_min earliest plausible date, as a decimal year
#' @param comment `r param_comment()`
#'
#' @details
#' In the GEO-TREES INV_processing workflow, each acquisition should contain 
#' only one census per plot, and censuses should generally occur within one 
#' year either side of the corresponding ALS acquisition. Measurement dates 
#' which are spread over a long period within a plot may indicate that 
#' measurements from more than one census have been mixed.
#'
#' @return dataframe with values of `meas_id` with suspect measurement dates, 
#'     with the date as a decimal year, the median date of the plot, and a 
#'     description of the issue
#' 
#' @examples
#' df <- data.frame(
#'   id = 1:6,
#'   plot_id = "A",
#'   date = c("2024-02-01", "2024-03", "2024-02-20", "2021-01-01", 
#'     "01/02/2024", "2030-01-01")
#' )
#' 
#' flagMeasDate(df, "id", "plot_id", "date", ref_date = "2024-06")
#' 
#' @export
#' 
flagMeasDate <- function(x, meas_id, plot_id, date, ref_date = NULL, 
  max_diff = 1, max_span = 1, date_min = 1900, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, plot_id, date)

  # Convert to decimal years
  dec <- decYear(x[[date]])
  date_max <- decYear(Sys.Date())
  plot_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))
  plot_median <- ave(dec, plot_key, FUN = function(i) { 
    stats::median(i, na.rm = TRUE) 
  })

  # Identify issues, the most severe last so it takes precedence
  issue <- rep(NA_character_, nrow(x))
  issue[abs(dec - plot_median) > max_span / 2] <- 
    sprintf("More than %s years from plot median date", max_span / 2)
  if (!is.null(ref_date)) {
    ref_dec <- decYear(ref_date)
    issue[abs(dec - ref_dec) > max_diff] <- 
      sprintf("More than %s years from reference date", max_diff)
  }
  issue[dec < date_min] <- "Date before 'date_min'"
  issue[dec > date_max] <- "Date in the future"
  issue[is.na(dec) & !is.na(x[[date]])] <- "Date not recognised"
  issue[is.na(x[[date]])] <- "Date missing"

  out <- x[, unique(c(meas_id, plot_id, date)), drop = FALSE]
  out$date_dec <- dec
  out$plot_median_dec <- plot_median
  out$issue <- issue
  out <- out[!is.na(out$issue), , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Suspect measurement dates detected")
  }

  # Return
  return(out)
}
