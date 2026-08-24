#' Flag measurements with missing values
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param target `r param_target()`
#' @param cond `r param_cond()`
#' @param comment `r param_comment()`
#'
#' @details
#' Identifies records where any column in `target` is `NA` or empty (`""`), 
#' only for rows where `cond` evaluates to `TRUE`.
#'
#' @return dataframe of `meas_id` with missing values in `target`
#' 
#' @examples
#' df <- data.frame(
#'   dbh = c(10, NA, 8, NA, NA),
#'   status = c("alive", "alive", "dead", "alive", "dead")
#' )
#' 
#' flagValMissing(df, "dbh", df$status == "alive")
#' 
#' @export
#' 
flagValMissing <- function(x, meas_id, target, cond = NULL, comment = NULL) {
  
  # Check input
  columnCatch(x, meas_id, target)

  # Check cond is same length as rows in x
  if (!is.null(cond)) { 
    if (nrow(x) != length(cond)) {
      stop("'cond' must be the same length as the number of rows in 'x'")
    }
  }

  # Evaluate condition within the data frame environment
  if (is.null(cond)) {
    in_scope <- rep(TRUE, nrow(x))
  } else {
    in_scope <- cond
    # Ensure NAs in condition result in FALSE
    in_scope[is.na(in_scope)] <- FALSE
  }
  
  # Isolate target columns
  val <- x[, target, drop = FALSE]

  miss_list <- lapply(val, function(i) {
    is_missing <- is.na(i)

    if (is.character(i)) {
      is_missing <- is_missing | trimws(i) == ""
    }
    
    if (is.numeric(i)) {
      is_missing <- is_missing | !is.finite(i)
    } 

    is_missing
  })

  # Convert list back to a matrix 
  miss_df <- as.data.frame(do.call(cbind, miss_list))
  any_missing <- rowSums(miss_df) > 0

  # Add meas_id columns
  miss_df_meas <- cbind(x[,meas_id], miss_df)

  # Filter to rows missing any values
  out <- miss_df_meas[any_missing,]

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) { 
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Missing values detected")
  }

  # Return
  return(out)
}

