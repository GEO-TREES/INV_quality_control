#' Flag ID values in one table which are missing from another table
#'
#' @param x dataframe to check, e.g. the stem measurement table
#' @param y dataframe to match against, e.g. the plot meta-data table
#' @param x_id column name(s) in `x` to match
#' @param y_id column name(s) in `y` to match, in the same order as `x_id`. 
#'     Defaults to `x_id`
#' @param comment `r param_comment()`
#'
#' @details
#' Rows of `x` where any of `x_id` are `NA` or empty are ignored, use 
#' `flagValMissing()` to find these. ID values are compared as character 
#' strings, so e.g. "1" and 1 are treated as the same value.
#'
#' @return dataframe with unique values of `x_id` which are not found in 
#'     `y_id`, with the number of rows in `x` with each value 
#' 
#' @examples
#' stem <- data.frame(plot_id = c("A", "A", "B", "C"), diam = 1:4)
#' plot <- data.frame(plot_id = c("A", "B"))
#' 
#' flagTableMatch(stem, plot, "plot_id")
#' 
#' @export
#' 
flagTableMatch <- function(x, y, x_id, y_id = x_id, comment = NULL) {

  # Check input
  columnCatch(x, x_id)
  columnCatch(y, y_id)

  if (length(x_id) != length(y_id)) {
    stop("'x_id' and 'y_id' must be the same length")
  }

  # Create keys
  x_ids <- x[, x_id, drop = FALSE]
  x_empty <- Reduce(`|`, lapply(x_ids, function(i) { 
    is.na(i) | trimws(as.character(i)) == ""
  }))
  x_key <- do.call(paste, c(lapply(x_ids, as.character), sep = "::"))
  y_key <- do.call(paste, c(lapply(y[, y_id, drop = FALSE], as.character), sep = "::"))

  # Find unmatched keys
  unmatched <- !x_empty & !x_key %in% y_key

  out <- x_ids[unmatched & !duplicated(x_key), , drop = FALSE]
  out$n_records <- as.vector(table(x_key[unmatched])[
    x_key[unmatched & !duplicated(x_key)]])
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("ID values not found in 'y'")
  }

  # Return
  return(out)
}
