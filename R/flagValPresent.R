#' Flag measurements with values which should be empty
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param target `r param_target()`
#' @param cond `r param_cond()`
#' @param comment `r param_comment()`
#'
#' @details
#' The opposite of `flagValMissing()`. Identifies records where any column in
#' `target` is not `NA` or empty (`""`), only for rows where `cond` evaluates
#' to `TRUE`. For example, stems recorded as missing ("M") must not have a 
#' diameter, POM or height measurement in the GEO-TREES INV_processing 
#' workflow.
#'
#' @return dataframe of `meas_id` with values present in `target`
#' 
#' @examples
#' df <- data.frame(
#'   id = 1:4,
#'   dbh = c(10, NA, 8, 12),
#'   code = c("AS", "M", "M", "AS")
#' )
#' 
#' flagValPresent(df, "id", "dbh", cond = grepl("M", df$code))
#' 
#' @export
#' 
flagValPresent <- function(x, meas_id, target, cond = NULL, comment = NULL) {
  
  # Check input
  columnCatch(x, meas_id, target)

  # Check cond is same length as rows in x
  if (!is.null(cond)) { 
    if (nrow(x) != length(cond)) {
      stop("'cond' must be the same length as the number of rows in 'x'")
    }
  }

  # Evaluate condition
  if (is.null(cond)) {
    in_scope <- rep(TRUE, nrow(x))
  } else {
    in_scope <- cond
    # Ensure NAs in condition result in FALSE
    in_scope[is.na(in_scope)] <- FALSE
  }
  
  # Identify present values in target columns
  pres_df <- as.data.frame(lapply(x[, target, drop = FALSE], function(i) {
    is_present <- !is.na(i)
    if (is.character(i)) {
      is_present <- is_present & trimws(i) != ""
    }
    is_present
  }))
  any_present <- rowSums(pres_df) > 0 & in_scope

  # Filter to rows with any present values
  out <- cbind(x[, meas_id, drop = FALSE], pres_df)[any_present, , drop = FALSE]

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) { 
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Values present which should be empty")
  }

  # Return
  return(out)
}
