#' Check that columns exist in a dataframe
#'
#' @param x dataframe
#' @param ... character vectors specifying column names in 'x'
#'
#' @examples
#' df <- data.frame(
#'   a = seq(1:5),
#'   b = seq(6:10),
#'   c = seq(11:15))
#'
#' cols1 <- c("a", "b")
#' cols2 <- c("c")
#' 
#' columnCatch(df, cols1, cols2)
#' 
#' cols3 <- c("d", "e")
#' columnCatch(df, cols1, cols3)
#' 
#' 
#' @keywords internal
#' @noRd
#' 
columnCatch <- function(x, ...) {
  if (!all(c(...) %in% names(x))) {
    arg_calls <- sapply(substitute(list(...))[-1], deparse)
    stop(paste(paste0("'", arg_calls, "'"), collapse = ", "), " must specify column names in 'x'")
  }
}

