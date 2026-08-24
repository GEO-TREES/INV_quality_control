#' Flag duplicated measurement records 
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param census_id `r param_census_id()`
#'
#' @return dataframe with values of `meas_id` that are duplicated in `x`
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 2, 3, 1, 1),
#'   census = c(2010, 2010, 2010, 2010, 2010),
#'   dbh = c(10, 15, 20, 11, 14)
#' )
#' 
#' flagRecordDup(df, c("tree_id", "census"))
#' 
#' @export
flagRecordDup <- function(x, meas_id, census_id = NULL) {
  
  # Check input
  columnCatch(x, meas_id, census_id)

  # Extract ID columns
  ids <- x[, c(meas_id, census_id), drop = FALSE]

  # Find duplicated values 
  out <- unique(ids[duplicated(ids), , drop = FALSE])

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) { 
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Duplicated measurement records detected")
  }

  # Return
  return(out)
}

