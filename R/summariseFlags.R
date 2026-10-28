#' Summarise the number of records flagged by each check
#'
#' @param x named list of dataframes returned by `flag*()` and `correct*()` 
#'     functions
#'
#' @return dataframe with the name of each check and the number of rows 
#'     flagged
#' 
#' @examples
#' flag_list <- list(
#'   flagRecordDup = data.frame(id = 1:3),
#'   flagValRange = data.frame(id = integer(0)),
#'   flagValUnit = NULL)
#' 
#' summariseFlags(flag_list)
#' 
#' @export
#' 
summariseFlags <- function(x) {

  if (!is.list(x) || is.data.frame(x) || is.null(names(x))) {
    stop("'x' must be a named list of dataframes")
  }

  out <- data.frame(
    check = names(x),
    n_flagged = vapply(x, function(i) {
      if (is.null(i)) 0L else nrow(i)
    }, integer(1)))
  rownames(out) <- NULL

  # Return
  return(out)
}
