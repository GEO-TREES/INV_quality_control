#' Flag trees with stems spread across multiple locations
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual trees")
#' @param x_rel column name in `x` with relative X coordinates
#' @param y_rel column name in `x` with relative Y coordinates
#' @param threshold numeric maximum allowable distance between any two stems 
#'     belonging to the same tree.
#' @param census_id `r param_census_id(optional = TRUE)`
#' @param comment `r param_comment()`
#'
#' @details
#' This function calculates the pairwise Euclidean distance between all stems 
#' associated with a single `ind_id`. If any distance exceeds the `threshold`, 
#' the `ind_id` is flagged.
#'
#' @return dataframe with values of `ind_id` containing any stems with pairwise distances greater than `threshold` 
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 2, 2),
#'   gx = c(0, 0.5, 10, 25),
#'   gy = c(0, 0.2, 10, 30)
#' )
#' 
#' flagMultiLoc(df, "tree_id", c("gx", "gy"), threshold = 5)
#' 
#' @export
flagMultiLoc <- function(x, ind_id, x_rel, y_rel, threshold, census_id = NULL, comment = NULL) {
  
  # Ensure coordinate columns exist
  columnCatch(x, ind_id, census_id, x_rel, y_rel)

  x[[x_rel]] <- classCatch(x[[x_rel]], "numeric")
  x[[y_rel]] <- classCatch(x[[y_rel]], "numeric")

  # Split coordinate data by tree_id
  ind_list <- split(x[, c(ind_id, census_id, x_rel, y_rel), drop = FALSE], 
    as.list(x[,c(ind_id, census_id), drop = FALSE]), drop = TRUE)

  # Check each tree
  is_too_spread <- lapply(ind_list, function(y) {
    # If multi-stemmed
    if (nrow(y) > 1) {
      # Calculate pairwise Euclidean distances
      distances <- dist(y[,c(x_rel, y_rel)])
      
      # Check if any distance is greater than the threshold
      if (any(distances > threshold, na.rm = TRUE)) {
        # Calculate mean pairwise distance
        ind_flag <- y[1, c(ind_id, census_id), drop = FALSE]
        ind_flag$dist_mean <- mean(distances, na.rm = TRUE) 
        ind_flag
      }
    }
  })

  out <- do.call(rbind, is_too_spread)
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) & nrow(out) > 0) { 
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Multi-stemmed trees with incongruous stem coordinates detected")
  }
    
  # Return
  return(out)
}
