#' Filter check results to records in the focal census
#'
#' @param x dataframe returned by a `flag*()` or `correct*()` function, run 
#'     on data from all censuses
#' @param focal dataframe of stem measurements in the focal census, e.g. 
#'     `x[x$focal, ]`
#' @param by column name(s) in `x` and `focal` used to match records, e.g. 
#'     stem IDs for checks which flag individual stems, or stem IDs and 
#'     census IDs for checks which flag individual measurements
#'
#' @details
#' Multi-census checks are run on data from all censuses, and identify 
#' issues in any census. This function keeps only the issues which affect 
#' the focal census, which is submitted to INV_processing. For example, for 
#' a check of taxonomic names among censuses, keep stems which are in the 
#' focal census with `by = c("plot_id", "stem_id")`, and for a check of 
#' diameter timelines, keep corrected measurements in the focal census with 
#' `by = c("plot_id", "stem_id", "census_id")`.
#'
#' @return rows of `x` which match records in `focal`
#' 
#' @examples
#' flags <- data.frame(stem_id = c(1, 2, 3), census_id = c(1, 2, 2))
#' focal <- data.frame(stem_id = c(1, 2, 3, 4), census_id = 2)
#' 
#' # Stems in the focal census
#' filterFocal(flags, focal, "stem_id")
#' 
#' # Measurements in the focal census
#' filterFocal(flags, focal, c("stem_id", "census_id"))
#' 
#' @export
#' 
filterFocal <- function(x, focal, by) {

  if (is.null(x)) {
    return(NULL)
  }

  # Check input
  columnCatch(x, by)
  columnCatch(focal, by)

  x_key <- do.call(paste, c(lapply(x[, by, drop = FALSE], as.character), sep = "::"))
  focal_key <- do.call(paste, c(lapply(focal[, by, drop = FALSE], as.character), 
    sep = "::"))

  out <- x[x_key %in% focal_key, , drop = FALSE]
  rownames(out) <- NULL

  # Return
  return(out)
}

#' Order census IDs
#'
#' @param x vector of census IDs
#'
#' @return numeric vector giving the order of each census. If all census IDs
#'     can be coerced to numeric, numeric values are used, otherwise the 
#'     alpha-numeric order of the census IDs
#' 
#' @keywords internal
#' @noRd
#' 
censusOrder <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    return(as.numeric(x))
  }
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  if (!anyNA(x_num[!is.na(x)])) {
    return(x_num)
  }
  match(x, sort(unique(x)))
}
