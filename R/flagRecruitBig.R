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
#' @param plot_id optional, column name(s) in 'x' identifying plots. If 
#'     provided and `census_date_vec` is `NULL`, the census timeline is guessed
#'     separately for each plot, so that stems in the first census of a plot 
#'     are not treated as recruits
#' @param min_diam_threshold `r param_min_diam_threshold()`
#' @param growth_max maximum feasible annual diameter growth rate
#' @param comment `r param_comment()`
#'
#' @return dataframe with values of `ind_id` that are possibly overgrown recruits 
#'
#' @export
#'
flagRecruitBig <- function(x, ind_id, diam, census_date, census_date_vec = NULL,
  plot_id = NULL, min_diam_threshold = 10, growth_max = 5, comment = NULL) { 
  
  # Check input
  columnCatch(x, ind_id, diam, census_date, plot_id)
  
  # Coerce dates (Assuming standard as.Date logic)
  x[[census_date]] <- as.Date(x[[census_date]]) 
  
  # Sort dataframe chronologically
  x <- x[do.call(order, x[, c(ind_id, census_date)]), ]
  
  # Create census timeline, for each plot if plot IDs provided
  if (!is.null(census_date_vec)) { 
    plot_key <- rep("all", nrow(x))
    census_list <- list(all = sort(as.Date(census_date_vec)))
  } else {
    if (!is.null(plot_id)) {
      plot_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))
    } else {
      plot_key <- rep("all", nrow(x))
    }
    census_list <- lapply(split(x[[census_date]], plot_key), function(y) {
      sort(unique(y[!is.na(y)]))
    })
  }
  
  # Identify first appearance of each individual
  is_first <- !duplicated(x[, ind_id, drop = FALSE])
  first_records <- x[is_first, ]
  first_plot <- plot_key[is_first]
  first_census <- as.Date(vapply(census_list[first_plot], function(y) {
    as.numeric(y[1])
  }, numeric(1)))
  
  # Filter to recruits (not seen in first census)
  recruits_idx <- !is.na(first_records[[diam]]) & 
    !is.na(first_records[[census_date]]) & 
    (first_records[[census_date]] > first_census)
  
  if (any(recruits_idx)) {
    candidates <- first_records[recruits_idx, ]
    cand_plot <- first_plot[recruits_idx]
  
    # Find previous census date for all candidates 
    prev_census_dates <- as.Date(vapply(seq_len(nrow(candidates)), function(i) {
      cv <- census_list[[cand_plot[i]]]
      as.numeric(max(cv[cv < candidates[[census_date]][i]]))
    }, numeric(1)))
  
    # Growth calculation
    int_days <- as.numeric(candidates[[census_date]] - prev_census_dates)
    max_growth_allowed <- (int_days / 365.25) * growth_max
    est_prev_diam <- candidates[[diam]] - max_growth_allowed
  
    # Create output
    out <- candidates[!is.na(est_prev_diam) & est_prev_diam > min_diam_threshold, 
      c(ind_id, census_date, diam)]
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
