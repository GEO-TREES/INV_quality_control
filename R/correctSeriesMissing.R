#' Correct missing values by carrying forward or backwards 
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param census_id `r param_census_id()`
#' @param target `r param_target()`
#' @param method character vector containing "forward" or "backward" to specify the direction in which missing values will be filled. 
#' @param comment `r param_comment()`
#' 
#' @details
#' Missing values will be filled in the order specified by the vector. 
#' E.g `method = c("backward", "forward")` will fill first backwards, then forwards.
#'
#' @return dataframe with values of `ind_id` and `census_id` with corrected values of columns in `target`
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 1, 2, 2, 2),
#'   year = c(2000, 2005, 2010, 2000, 2005, 2010),
#'   x_rel = c(10.5, NA, 12.1, NA, 18.0, 19.5)
#' )
#'
#' correctSeriesMissing(
#'   x = df,
#'   ind_id = "tree_id",
#'   census_id = "year",
#'   target = "x_rel",
#'   method = c("forward", "backward"),
#'   comment = "Missing values filled"
#' )
#' 
#' 
#' @export
#' 
correctSeriesMissing <- function(x, ind_id, census_id, target, 
  method = c("forward", "backward"), comment = NULL) { 

  # Check input
  columnCatch(x, ind_id, census_id, target)

  method <- match.arg(method, choices = c("forward", "backward"), several.ok = TRUE)

  # Order dataframe by individuals and then chro,nologically by census
  out <- x[do.call(order, x[, c(ind_id, census_id)]), ]

  # Optionally add comment column
  if (!is.null(comment)) { 
    out$comment <- NA_character_
  }
  
  # Create a unified grouping vector
  if (length(ind_id) > 1) {
    grp <- do.call(paste, c(out[ind_id], sep = "::"))
  } else {
    grp <- out[[ind_id]]
  }
  
  # Helper: Base R Last Observation Carried Forward (Group Aware)
  locf <- function(v, gr) {
    good <- !is.na(v)
    if (!any(good)) return(v)
    
    idx <- cumsum(good)
    good_idx <- which(good)
    
    # Map each element to its donor's global index
    donor_idx <- rep(NA_integer_, length(v))
    has_donor <- idx > 0
    donor_idx[has_donor] <- good_idx[idx[has_donor]]
    
    # Values can only be carried forward if the donor belongs to the same group
    valid <- !is.na(donor_idx) & (gr == gr[donor_idx])
    
    out <- v
    out[valid] <- v[donor_idx[valid]]
    return(out)
  }
  
  # Helper: Base R Next Observation Carried Backward
  nocb <- function(v, gr) {
    rev(locf(rev(v), rev(gr)))
  }
  
  # 4. Iteratively apply the fills for each target column and requested method
  for (col in target) {
    for (m in method) {
      if (m == "forward") {
        fnew <- locf(out[[col]], grp)
        if (!is.null(comment)) {
          out$comment[is.na(out[[col]]) & !is.na(fnew)] <- comment
        }
        out[[col]] <- fnew
      } else if (m == "backward") {
        bnew <- nocb(out[[col]], grp)
        if (!is.null(comment)) {
          out$comment[is.na(out[[col]]) & !is.na(bnew)] <- comment
        }
        out[[col]] <- bnew
      }
    }
  }
  
  # Generate message
  if (any(is.na(x[,target, drop = FALSE]) & !is.na(out[,target, drop = FALSE]))) { 
    message("Missing values filled")
  }

  # Return
  return(out)
}
