#' Correct mortality status timelines
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param census_id `r param_census_id()`
#' @param diam optional `r param_diam()`. If provided, the presence of a 
#' diameter measurement implies the individual is alive if any missing values 
#' remain after first set of corrections.
#' @param status `r param_status()`
#' (TRUE = alive, FALSE = dead, NA = unseen)
#' @param comment `r param_comment()`
#' 
#' @details
#' Missing mortality status records are imputed using a stepwise algorithm. 
#' Missing data are interpolated if preceded and followed by records with an 
#' identical mortality status. Where missing data occurred in the first or 
#' final census, it is imputed by extrapolation if preceded or succeeded by 
#' two or more records with an identical mortality status. When neither 
#' extrapolation or interpolation can be applied, e.g. for a stem with no 
#' mortality status information, or where preceding and proceeding mortality 
#' status differ, stems are assumed dead if no diameter measurement is 
#' recorded, and vice versa, if the `diam` column is provided. 
#' Finally, to correct for apparent resurrection of stems, all records prior 
#' to when the stem is last recorded as alive and has a diameter measurement 
#' are corrected to alive. 
#'
#' @return dataframe of `ind_id` and `census_id` where individuals had 
#' corrections to the mortality status timeline.
#' 
#' @examples
#' x <- data.frame(
#'   stem_id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 7, 8),
#'   census_id = c(2010, 2015, 2020, 2010, 2015, 2020, 2010, 2015, 2020, 
#'     2010, 2015, 2020, 2010, 2015, 2020, 2010, 2010, 2010),
#'   diam = c(5, 10, 10, 5, 10, 15, 5, 10, 15, 5, 10, 15, 5, 10, NA, 5, NA, NA),
#'   status = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, 
#'     TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' correctMortalitySeries(x,
#'   ind_id = "stem_id",
#'   census_id = "census_id",
#'   diam = "diam",
#'   status = "status",
#'   comment = "Diameter timelines corrected"
#' )
#' 
#' @export
#' 
correctMortalitySeries <- function(x, ind_id, census_id, diam, status, 
  comment = NULL) {

  # Check input
  columnCatch(x, ind_id, census_id, diam, status) 

  x[[status]] <- classCatch(x[[status]], "logical")

  # Split by individual
  x <- x[do.call(order, x[, c(ind_id, census_id)]), ]
  x_split <- split(x, as.list(x[,ind_id, drop = FALSE]), drop = TRUE)

  # Filter to multi-census data
  x_split_fil <- x_split[unlist(lapply(x_split, nrow) > 1)]

  # Impute dodgy and missing mortality values 
  # For each stem:
  out <- do.call(rbind, lapply(x_split_fil, function(y) { 
    # Create vector to hold methods for interpolation/extrapolation
    mis_method <- rep(NA_character_, length(y[[status]]))

    # Compute run length encoding, excluding NAs
    y_rle_nona <- rle(c(na.omit(y[[status]])))

    # Compute run length encoding, with NAs
    y_rle <- rleNA(y[[status]])

    # Interpolate dead
    # If any dead found
    if (any(y_rle_nona$lengths[y_rle_nona$values == 0] > 0)) {
      # For each run of values
      for (i in seq_along(y_rle$values)[-1]) {
        # If status NA, preceded and followed by dead, not final value
        if (is.na(y_rle$values[i]) & 
          all(y_rle$values[c(i-1, i+1)] == 0, na.rm = TRUE) & 
          i != length(y_rle$values)) {
          # Fill NAs with alive 
          y_rle$values[i] <- 0
          mis_method[seq(sum(y_rle$lengths[1:(i-1)])+1, 
            sum(y_rle$lengths[1:i]))] <- "Interpolated dead"
        }
      }
    }

    # Interpolate alive
    # If any alive found
    if (any(y_rle_nona$lengths[y_rle_nona$values == 1] > 0)) {
      # For each run of values
      for (i in seq_along(y_rle$values)[-1]) {
        # If status NA, preceded and followed by alive, not final value
        if (is.na(y_rle$values[i]) & 
          all(y_rle$values[c(i-1, i+1)] == 1, na.rm = TRUE) & 
          i != length(y_rle$values)) {
          # Fill NAs with alive 
          y_rle$values[i] <- 1
          mis_method[seq(sum(y_rle$lengths[1:(i-1)])+1, 
            sum(y_rle$lengths[1:i]))] <- "Interpolated alive"
        }
      }
    }

    # If missing is first group
    if (is.na(y_rle$values[1]) & any(!is.na(y_rle$values))) {
      # If proceeded by 2 dead
      if (y_rle$values[2] == 0 & y_rle$lengths[2] >1) {
        # Fill NAs with dead
        y_rle$values[1] <- 0
        mis_method[1:y_rle$lengths[1]] <- "Proceeded by >1 dead, inferred dead"
      # If proceeded by 2 alive
      } else if (y_rle$values[2] == 1 & y_rle$lengths[2] >1) {
        # Fill NAs with alive
        y_rle$values[1] <- 1
        mis_method[1:y_rle$lengths[1]] <- "Preceded by >1 alive, inferred alive"
      }
    }

    # If missing is last group
    if (is.na(y_rle$values[length(y_rle$values)]) & any(!is.na(y_rle$values))) {
      # If preceded by 2 dead
      if (y_rle$values[length(y_rle$values)-1] == 0 & 
        y_rle$lengths[length(y_rle$lengths)-1] >1) {
        # Fill NAs with dead
        y_rle$values[length(y_rle$values)] <- 0
        mis_method[length(mis_method):(y_rle$lengths[length(y_rle$lengths)]+1)] <- 
          "Preceded by >1 dead, inferred dead"
      # If preceded by 2 alive
      } else if (y_rle$values[length(y_rle$values)-1] == 1 & 
        y_rle$lengths[length(y_rle$lengths)-1] >1) {
        # Fill NAs with alive
        y_rle$values[length(y_rle$values)] <- 1
        mis_method[length(mis_method):(y_rle$lengths[length(y_rle$lengths)]+1)] <- 
          "Proceeded by >1 alive, inferred alive"
      }
    }

    # Reverse run length encoding to get vector with interpolated values 
    y[[status]] <- inverse.rle(y_rle)

    # If any remain missing
    if (!is.null(diam) && any(is.na(y[[status]]))) {
      
      # Find indices of missing status
      na_idx <- which(is.na(y[[status]]))
      
      for (i in na_idx) {
        # Must not be the very first measurement (requires a preceding state)
        if (i > 1) { 
          has_diam <- !is.na(y[[diam]][i])
          prev_stat <- y[[status]][i - 1] # Looks at the immediately preceding row
          
          # Logic 1: Diameter present AND previously alive
          if (has_diam && !is.na(prev_stat) && prev_stat == 1) {
            y[[status]][i] <- 1
            mis_method[i] <- "Diameter presence and previously alive, inferred alive"
            
          # Logic 2: Diameter absent AND previously dead
          } else if (!has_diam && !is.na(prev_stat) && prev_stat == 0) {
            y[[status]][i] <- 0
            mis_method[i] <- "Diameter absence and previously dead, inferred dead"
          }
        }
      }
    }

    # Identify case where alive after dead
    all_alive <- which(y[[status]] == TRUE)
    if (length(all_alive) > 0) { 
      max_alive <- max(all_alive)
    } else {
      max_alive <- NA_real_
    }

    dead_indices <- which(y[[status]] == FALSE)

    if (length(dead_indices) > 0) {
      max_dead <- max(dead_indices)
    } else {
      max_dead <- NA 
    }

    # Create vector to hold changes
    fix_method <- rep(NA_character_, length(y[[status]]))

    # Find alives
    alives <- which(y[[status]] == TRUE)

    # If any alives
    if (length(alives) > 0) {
      # Convert all within first and last to alive
      interm_change <- intersect(seq(alives[1], alives[length(alives)]),
        which(x[[status]] == 0))
      if (length(interm_change) > 0) {
        y[interm_change, status] <- 1
        fix_method[interm_change] <- "Inconsistent death converted to alive"
      }

      # If any diams
      if (!is.null(diam)) {
        # Find with diam
        diams <- which(!is.na(y[[diam]]))
        if (length(diams) > 0) {
          # Convert all dead with diameters before first alive to alive
          before_change <- intersect(intersect(1:min(alives), 1:min(diams)), 
            which(x[[status]] == 0))
          if (length(before_change) > 0) {
            y[before_change, status] <- 1
            fix_method[before_change] <- "Death before alive with diameter converted to alive"
          }
        }

        # Convert consecutive alive without diameter before first alive with diameter to dead
        first_alive_wdiam <- intersect(alives, diams)
        if (length(first_alive_wdiam) > 0) {
          first_alive_wdiam <- min(first_alive_wdiam)
        } else { 
          first_alive_wdiam <- integer(0)
        }
        alive_no_diam <- setdiff(alives[alives < first_alive_wdiam], diams)
        if (length(alive_no_diam) > 0) {
          y[alive_no_diam, status] <- 0
          fix_method[alive_no_diam] <- "Missing diameters before alive with diameter converted to dead"
        }
      }

      # Convert all within first and last to alive
      alives <- which(y[[status]] == 1)
      interm_change <- intersect(seq(alives[1], alives[length(alives)]),
        which(y[[status]] == 0))
      if (length(interm_change) > 0) {
        y[interm_change, status] <- 1
        fix_method[interm_change] <- "Inconsistent death converted to alive"
      }
    }

    # Return
    y_out <- y
    y_out$method <- pasteVals(mis_method, fix_method, sep = ";")

    if (any(!is.na(y_out$method))) { 
      y_out[,c(ind_id, census_id, status, "method")]
    } else {
      NULL
    }

  }))
  rownames(out) <- NULL
  out[[status]] <- as.logical(out[[status]])

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) { 
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Mortality timelines corrected")
  }

  # Return
  return(out)
}

