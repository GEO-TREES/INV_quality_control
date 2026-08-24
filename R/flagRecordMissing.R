#' Flag missing measurement records
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param census_id `r param_census_id()`
#' @param census_id_vec `r param_census_id_vec()`
#' @param comment `r param_comment()`
#'
#' @return dataframe with values of `ind_id` and `census_id` that are missing from `x`
#' 
#' @details
#' `census_id` can be either a vector of census dates, or census IDs. These 
#' values are not required to represent actual dates, but should sort 
#' alpha-numerically in the order of the censuses.
#'
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 2, 2, 2, 3),
#'   year = c(2010, 2020, 2010, 2015, 2020, 2010),
#'   dbh = c(10, 12, 15, 16, 17, 5)
#' )
#'
#' flagRecordMissing(df, ind_id = "tree_id", census_id = "year")
#'
#' all_years <- c(2010, 2012, 2015, 2020)
#' 
#' flagRecordMissing(
#'   x = df, 
#'   ind_id = "tree_id", 
#'   census_id = "year", 
#'   census_id_vec = all_years, 
#'   comment = "Missing measurement record")
#'
#' @export
#' 
flagRecordMissing <- function(x, ind_id, census_id, census_id_vec = NULL, comment = NULL) { 

  # Check input
  columnCatch(x, ind_id, census_id)

  # If census ID vector not provided, guess from existing data
  if (is.null(census_id_vec)) {
    message("'census_id_vec' not provided, guessing from values in 'census_id'")
    census_id_vec <- sort(unique(x[[census_id]])) 
  } else {
    census_id_vec <- sort(census_id_vec)
  }

  # Split by individual
  x_split <- split(x, x[, ind_id, drop = FALSE], drop = TRUE)

  # Only retain multi-census individuals
  x_split_fil <- x_split[unlist(lapply(x_split, nrow)) > 1]

  # Identify missing records
  missing_list <- lapply(x_split_fil, function(y) {
    first_c <- min(y[[census_id]])
    last_c  <- max(y[[census_id]])

    gaps <- census_id_vec[
      !census_id_vec %in% y[[census_id]] & 
      census_id_vec < last_c & 
      census_id_vec > first_c
    ]

    if (length(gaps) > 0) {
      # Keep all columns from the original dataframe
      template <- y[1, , drop = FALSE]
      
      # Set all columns EXCEPT the individual ID(s) to NA
      cols_to_na <- setdiff(names(template), ind_id)
      if (length(cols_to_na) > 0) {
        template[, cols_to_na] <- NA
      }
      
      # Replicate for the number of missing censuses
      new_rows <- template[rep(1, length(gaps)), , drop = FALSE]
      
      # Insert the missing census IDs (overwriting the NA)
      new_rows[[census_id]] <- gaps
      
      new_rows
    } else {
      NULL
    }
  })

  out <- do.call(rbind, missing_list)
  
  # Handle case where no missing records are found
  if (is.null(out) || nrow(out) == 0) {
    out <- x[0, , drop = FALSE]
    if (!is.null(comment)) {
      out$comment <- character(0)
    }
    return(out)
  }

  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment_flagRecordMissing <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Missing measurement records detected")
  }

  # Return
  return(out)
}
