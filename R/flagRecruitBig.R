#' Flag potential overgrown recruits
#'
#' @description Identifies "overgrown recruits", i.e. stems that first appear 
#' in a census with a diameter much larger than the minimum diameter threshold 
#' of the plot, accounting for expected growth since the previous census.
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param diam `r param_diam()`
#' @param census_date `r param_census_date()`
#' @param census_date_vec `r param_census_date_vec()`
#' @param min_diam_threshold `r param_min_diam_threshold()`
#' @param growth_max maximum feasible annual diameter growth rate
#' @param comment `r param_comment()`
#'
#' @return dataframe with values of `ind_id` that are possibly overgrown recruits 
#'
#' @export
#'
flagRecruitBig <- function(x, ind_id, diam, census_date, census_date_vec = NULL,
  min_diam_threshold = 10, growth_max = 5, comment = NULL) { 
  
  # Check input
  columnCatch(x, ind_id, diam, census_date)
  
  # Sort dataframe chronologically
  x <- x[do.call(order, x[, c(ind_id, census_date)]), ]
  
  # Coerce dates (Assuming standard as.Date logic)
  x[[census_date]] <- as.Date(x[[census_date]]) 
  
  # Create census timeline
  if (is.null(census_date_vec)) { 
    census_date_vec <- unique(x[[census_date]]) 
  } 
  census_date_vec <- sort(census_date_vec[!is.na(census_date_vec)]) 
  
  # Identify first appearance of each individual
  is_first <- !duplicated(x[, ind_id, drop = FALSE])
  first_records <- x[is_first, ]
  
  # Filter to recruits (not seen in first census)
  recruits_idx <- !is.na(first_records[[diam]]) & 
    !is.na(first_records[[census_date]]) & 
    (first_records[[census_date]] > census_date_vec[1])
  
  if (any(recruits_idx)) {
    candidates <- first_records[recruits_idx, ]
  
    # Find previous census date for all candidates 
    curr_date_pos <- match(candidates[[census_date]], census_date_vec)
    prev_census_dates <- census_date_vec[curr_date_pos - 1]
  
    # Growth calculation
    int_days <- as.numeric(candidates[[census_date]] - prev_census_dates)
    max_growth_allowed <- (int_days / 365.25) * growth_max
    est_prev_diam <- candidates[[diam]] - max_growth_allowed
  
    # Create output
    out <- candidates[est_prev_diam > min_diam_threshold, c(ind_id, census_date, diam)]
  } else {
    out <- x[0, c(ind_id, census_date, diam)]
  }
  rownames(out) <- NULL 

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) { 
    out$comment <- comment 
  } 

  # Generate message
  if (nrow(out) > 0) {  
    message("Overgrown recruits detected") 
  } 
  
  # Return
  return(out) 
}
