#' Flag and optionally remove ghost recruits with missing values from previous censuses 
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param census_id `r param_census_id()`
#' @param target `r param_target()`
#' @param comment `r param_comment()`
#'
#' @details
#' Identifies the first record for each unique `id`. If __all__ of the `target` 
#' columns contain `NA` in this first record, the row is flagged. This is useful 
#' for cleaning "ghost" records where an ID was back-filled to censuses before 
#' data was collected.
#'
#' @export
#' 
flagRecruitPre <- function(x, ind_id, census_id, target, comment = NULL) {
  
  # Check arguments
  columnCatch(x, ind_id, census_id, target)

  # Add temporary row index 
  x$..row_id.. <- seq_len(nrow(x))

  # Sort data by individual ID then census ID 
  x_sorted <- x[do.call(order, x[c(ind_id, census_id)]), ]

  # Identify the first record for each ID
  is_first_record <- !duplicated(x_sorted[,ind_id])
  
  # Check for NAs in target columns within first records
  # If target is a vector, check if __all__ target columns are NA
  all_na <- rowSums(is.na(x_sorted[, target, drop = FALSE])) == length(target)

  # Combine: Is first record AND missing target values?
  flagged_sorted_idx <- which(is_first_record & all_na)
  
  # Map back to original indices
  flagged_indices <- x_sorted$..row_id..[flagged_sorted_idx]
  flagged_rows <- x$..row_id.. %in% flagged_indices

  # Create output
  out <- x[flagged_rows, c(ind_id, census_id)]

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }
  
  # Generate message
  if (nrow(out) > 0) {
    message("Pre-census recruits detected")
  }

  # Return
  return(out)
}
