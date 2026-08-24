#' Flag unusually common round-values from continuous distribution
#' 
#' @param x `r param_x_stem()`
#' @param census_id `r param_id("individual censuses")`
#' @param target `r param_target()`
#' @param precision numeric precision to check for rounding (e.g., 1 for whole numbers, 0.5 for half-units, 0.1 for first decimal, etc.)
#' @param threshold proportion (0 to 1) above which rounded values are considered unusually common
#' @param comment `r param_comment()`
#' 
#' @return dataframe with values of `census_id` containing overly rounded values in `target`
#' 
#' @export
#'
flagValRound <- function(x, census_id, target, precision = 1, threshold = 0.3, comment = NULL) {

  # Check input
  columnCatch(x, census_id, target)

  x[[target]] <- classCatch(x[[target]], "numeric")

  if (all(!is.finite(x[[target]]))) { 
    stop("All values in 'target' are not finite")
  }

  # Split by census ID
  x_split <- split(x, as.list(x[,census_id]), drop = TRUE)

  # For each census
  out <- do.call(rbind, lapply(x_split, function(y) { 
    out <- unique(y[,census_id, drop = FALSE])

    # identify which values are multiples of the specified precision
    # using a small tolerance for floating point comparisons
    is_round <- abs(y[[target]] %% precision) < 1e-10
  
    # calculate the proportion of rounded values (excluding nas)
    out$prop_round <- mean(is_round, na.rm = TRUE)
  
    # if the proportion of rounded values exceeds the threshold, 
    # generate a report and flag the specific values
    out[out$prop_round > threshold,]
  }))
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) & nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) { 
    message("Rounded values detected")
  }

  # Return
  return(out)
}
