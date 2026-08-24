#' Flag censuses with likely unit inconsistencies
#'
#' @param x `r param_x_stem()`
#' @param plot_id `r param_plot_id()`
#' @param census_id `r param_census_id()`
#' @param target `r param_target()`
#' @param threshold numeric multiplier to define an outlier census mean. 
#' @param cv_threshold numeric threshold for Coefficient of Variation to detect 
#'     mixed units within a census. 
#' @param comment `r param_comment()`
#'
#' @details
#' This function flags censuses in two ways:
#' 1. **Among-census errors**: The mean of a census is an order of magnitude 
#'    different from the median of all census means.
#' 2. **Within-census errors**: The variation (CV) within a single census is 
#'    extremely high, suggesting a mixture of units (e.g. cm and mm).
#'
#' @return dataframe with values of `plot_id` and `census_id` containing possible unit shifts in `target`
#' 
#' @export
#' 
flagValUnit <- function(x, plot_id, census_id, target, threshold = 5, cv_threshold = 50, comment = NULL) {

  # Check arguments
  columnCatch(x, plot_id, census_id, target)

  x[[target]] <- classCatch(x[[target]], "numeric")
  
  if (all(!is.finite(x[[target]]))) { 
    stop("All values in 'target' are not finite")
  }

  # Calculate Mean and CV per census
  sc <- do.call(data.frame, aggregate(
    x[[target]], 
    by = list(
      plot = x[[plot_id]],
      census = x[[census_id]]),
    FUN = function(v) {
      m <- mean(v, na.rm = TRUE)
      s <- sd(v, na.rm = TRUE)
      cv <- (s / m) * 100
      return(c(mean = m, cv = cv))
    }))
  
  # Check for between-census shift 
  sc$baseline_mean <- median(sc$x.mean, na.rm = TRUE)
  sc$bad_mean <- sc$x.mean > (sc$baseline_mean * threshold) | sc$x.mean < (sc$baseline_mean / threshold)
  sc$bad_cv <- sc$x.cv > cv_threshold
  out <- sc[sc$bad_mean | sc$bad_cv,]
  names(out) <- c(plot_id, census_id, "mean", "cv", "mean_all", "among_census", "within_census")

  # Generate comment
  if (!is.null(comment) & nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Unit-shift detected")
  }

  # Return
  return(out)
}
