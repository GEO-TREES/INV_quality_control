#' Flag and optionally relocate stems to subplot centers 
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param subplot_id column name(s) in `x` uniquely identifying subplots
#' @param x_rel column name in `x` with relative X coordinates
#' @param y_rel column name in `x` with relative Y coordinates
#' @param subs dataframe with subplot boundary definitions
#' @param subs_subplot_id column name(s) in `subs` uniquely identifying subplots
#' @param subs_x_min column name in `subs` with minimum X coordinate of subplot
#' @param subs_x_max column name in `subs` with maximum X coordinate of subplot
#' @param subs_y_min column name in `subs` with minimum Y coordinate of subplot
#' @param subs_y_max column name in `subs` with maximum Y coordinate of subplot
#' @param correct logical, if TRUE, returns dataframe with corrected coordinates. 
#'   if FALSE, returns a logical vector of flags.
#' @param comment `r param_comment()`
#' 
#' @return dataframe of values in `meas_id` with flagged and optionally corrected relative coordinates
#' 
#' @export
#' 
correctLocSub <- function(x, meas_id, subplot_id, x_rel, y_rel, 
  subs, subs_subplot_id, subs_x_min, subs_x_max, subs_y_min, subs_y_max, 
  correct = FALSE, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, subplot_id, x_rel, y_rel)
  columnCatch(subs, subs_subplot_id, subs_x_min, subs_x_max, subs_y_min, subs_y_max)

  x[[x_rel]] <- classCatch(x[[x_rel]], "numeric")
  x[[y_rel]] <- classCatch(x[[y_rel]], "numeric")
  subs[[subs_x_min]] <- classCatch(subs[[subs_x_min]], "numeric")
  subs[[subs_x_max]] <- classCatch(subs[[subs_x_max]], "numeric")
  subs[[subs_y_min]] <- classCatch(subs[[subs_y_min]], "numeric")
  subs[[subs_y_max]] <- classCatch(subs[[subs_y_max]], "numeric")

  # Merge reference data into a temporary working object
  x$subplot_id <- paste(x[,subplot_id], sep = "::")
  subs$subplot_id <- paste(subs[,subs_subplot_id], sep = "::")

  tmp <- merge(
    x[, c(meas_id, subplot_id, x_rel, y_rel)], 
    subs[, c(subs_subplot_id, subs_x_min, subs_x_max, subs_y_min, subs_y_max)], 
    by.x = "subplot_id", 
    by.y = "subplot_id", 
    all.x = TRUE,
    sort = FALSE
  )

  # Identify stems outside recorded subplot 
  # Returns TRUE if missing OR outside numeric bounds
  is_outside <- tmp[[x_rel]] < tmp[[subs_x_min]] | 
    tmp[[x_rel]] > tmp[[subs_x_max]] | 
    tmp[[y_rel]] < tmp[[subs_y_min]] | 
    tmp[[y_rel]] > tmp[[subs_y_max]]
  is_outside[is.na(is_outside)] <- TRUE

  # Optionally correct
  if (correct) {
    # Calculate subplot centers
    center_x <- (tmp[[subs_x_min]] + tmp[[subs_x_max]]) / 2
    center_y <- (tmp[[subs_y_min]] + tmp[[subs_y_max]]) / 2

    # Update coordinates where flagged
    tmp$x_rel_correctLocSub <- NA_real_ 
    tmp$y_rel_correctLocSub <- NA_real_ 
    tmp$x_rel_correctLocSub[is_outside] <- center_x[is_outside] 
    tmp$y_rel_correctLocSub[is_outside] <- center_y[is_outside] 

    out <- tmp[,c(meas_id, subplot_id, x_rel, y_rel, "x_rel_correctLocSub", "y_rel_correctLocSub")]
  } else {
    out <- tmp[,c(meas_id, subplot_id, x_rel, y_rel)]
  }

  out_fil <- out[is_outside,]

  # Generate comment
  if (!is.null(comment) & nrow(out_fil) > 0) {
    out_fil$comment <- comment
  }

  # Generate message
  if (nrow(out_fil) > 0) { 
    message("Stem coordinates outside subplot detected")
  }

  # Return
  return(out_fil)
}
