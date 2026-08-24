#' Flag measurement values outside acceptable range 
#' 
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param target `x param_target()`
#' @param threshold optional, numeric vector of length 1 or 2. If a single value it is interpreted as an upper limit. If two values it is interpreted as a range of acceptable values in `target`
#' @param min_n Minimum number of observations in `target` to perform the check
#' @param comment `r param_comment()`
#' 
#' @details if `threshold` is not provided, the 99.9th percentile is used as the upper limit
#'
#' @return dataframe with `meas_id` where `target` is outside acceptable range in `threshold`
#' 
#' @export
#' 
flagValRange <- function(x, meas_id, target, threshold = NULL, min_n = 100, comment = NULL) {
  
  # Check arguments 
  columnCatch(x, meas_id, target)

  x[[target]] <- classCatch(x[[target]], "numeric")

  if (all(!is.finite(x[[target]]))) { 
    stop("All values in 'target' are not finite")
  }

  # Skip the check if sample too small
  n_obs <- sum(!is.na(x[[target]]))
  if (n_obs < min_n) {
    message(
      sprintf("Sample size (n=%d) is below the minimum required (min_n=%d), exiting", 
      n_obs, min_n))
    return(invisible(NULL))
  }

  # If no threshold provided set to 99.9th percentile
  if (is.null(threshold)) {
    threshold <- quantile(x[[target]], 0.999, na.rm = TRUE)
  }
  
  # Identify values above threshold
  if (length(threshold) == 1) {
    out <- x[!is.na(x[[target]]) & x[[target]] > threshold, c(meas_id, target)]
  } else if (length(threshold) == 2) {
    out <- x[
      !is.na(x[[target]]) & (x[[target]] < threshold[1] | x[[target]] > threshold[2]), 
      c(meas_id, target)]
  } else { 
    stop("'threshold' must be a numeric vector of length 1 or 2")
  }
  
  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Values outside range detected")
  }
  
  # Return
  return(out)
}
