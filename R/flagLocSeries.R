#' Flag individuals where location differs among censuses by a distance threshold
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param x_rel column name in `x` with relative X coordinates
#' @param y_rel column name in `x` with relative Y coordinates
#' @param census_id `r param_census_id()`
#' @param threshold numeric maximum allowable distance between stems between censuses.
#' @param comment `r param_comment()`
#' 
#' @details
#' This function calculates the pairwise Euclidean distance between all records for each stem associated with a single `ind_id`. If any distance exceeds the `threshold`, the `ind_id` is flagged.
#'
#' @return dataframe with values of `ind_id` where maximum distance moved
#' exceeds `dist_threshold`, along with the calculated `max_dist`
#' 
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 2, 3, 3),
#'   census = c(1, 2, 1, 2, 1, 2),
#'   gx = c(10.5, 10.5, 20.0, 25.0, 30.1, 30.2),
#'   gy = c(10.5, 10.6, 20.0, 25.0, 30.1, 30.2)
#' )
#' 
#' # Flag stems that moved more than 2 units
#' flagLocSeries(df, ind_id = "id", x_rel = "gx", y_rel = "gy", census_id = "census", dist_threshold = 2)
#' 
#' @export
#' 
flagLocSeries <- function(x, ind_id, x_rel, y_rel, census_id, dist_threshold, comment = NULL) { 

  # Check columns exist
  columnCatch(x, ind_id, census_id, x_rel, y_rel)

  # Filter out rows with missing coordinates to calculate distances properly
  x_coords <- x[!is.na(x[[x_rel]]) & !is.na(x[[y_rel]]), ]

  # Calculate maximum pairwise distance for each individual
  max_dists <- sapply(split(x_coords[, c(x_rel, y_rel)], x_coords[[ind_id]]), function(coords) {
    if (nrow(coords) < 2) {
      return(0)
    }
    # dist() calculates Euclidean distance matrix, max() finds the largest movement
    max(dist(coords))
  })

  # Create output dataframe
  out <- data.frame(
    id = names(max_dists), 
    max_dist = unname(max_dists), 
    stringsAsFactors = FALSE
  )
  names(out)[1] <- ind_id # Match the user-provided ID column name

  # Filter based on user-defined threshold
  out_fil <- out[out$max_dist > dist_threshold, ]
  rownames(out_fil) <- NULL

  # Generate comment
  if (!is.null(comment) & nrow(out_fil) > 0) {
    out_fil$comment <- comment
  }

  # Generate message
  if (nrow(out_fil) > 0) {
    message(paste("Location changes exceeded distance threshold (", dist_threshold, ") among censuses", sep = ""))
  }
  
  # Return
  return(out_fil)
}
