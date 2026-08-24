#' Flag trees where the number of stems exceeds a threshold
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual trees")`
#' @param census_id `r param_census_id(optional = TRUE)`
#' @param threshold maximum allowable number of stems for a single tree in a single census.
#' @param comment `r param_comment()`
#'
#' @return dataframe with values of `ind_id` containing trees with more stems than 'threshold'
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 1, 2, 2),
#'   year = c(2010, 2010, 2010, 2010, 2010),
#'   dbh = c(10, 12, 14, 20, 22)
#' )
#' 
#' flagMultiMany(df, "tree_id", "year", threshold = 2)
#' 
#' @export
flagMultiMany <- function(x, ind_id, census_id = NULL, threshold, comment = NULL) {
  
  # Check input
  columnCatch(x, ind_id, census_id)

  threshold <- classCatch(threshold, "numeric")

  if (length(threshold) > 1) {
    stop("'threshold' must be a numeric vector of length 1")
  }

  # Aggregate: Count the length of the dummy column for each group
  x$n_stem <- 1
  out <- aggregate(x["n_stem"], by = x[c(ind_id, census_id)], FUN = sum)

  # Filter to multi-stemmed trees with more stems than threshold
  out_fil <- out[out$n_stem > threshold,]

  # Generate comment
  if (!is.null(comment) & nrow(out_fil) > 0) {
    out_fil$comment <- comment
  }

  # Generate message
  if (nrow(out_fil) > 0) {
    message("Trees with too many stems detected")
  }
  
  # Return
  return(out_fil)
}
