################## 
# INTERNAL HELPERS 
################## 

#' Build all pairwise census combinations for one stem
#'
#' @param y single-stem subset of `x`, ordered by `census_date`
#' @param census_date `r param_census_date()`
#' @param diam `r param_diam()`
#' @param status `r param_status()`
#' @param pom `r param_pom()`
#' 
#' @return data.frame of all pairwise combinations with growth metrics
#'
#' @keywords internal
#' @noRd
#' 
buildPairs <- function(y, census_date, diam, status, pom) {
  idx <- combn(seq_len(nrow(y)), 2L)
  i0 <- idx[1L, ]
  iT <- idx[2L, ]
  d0 <- y[[diam]][i0]
  dT <- y[[diam]][iT]
  t0 <- y[[census_date]][i0]
  tT <- y[[census_date]][iT]
  tdiff <- as.numeric(tT - t0)
  growth_ann <- (dT - d0) * tdiff / 365.25
  data.frame(
    census_t0 = t0,
    census_tT = tT,
    diam_t0 = d0,
    diam_tT = dT,
    status_t0 = y[[status]][i0],
    status_tT = y[[status]][iT],
    pom_t0 = y[[pom]][i0],
    pom_tT = y[[pom]][iT],
    census_dist = tdiff,
    growth_ann = growth_ann
  )
}

#' Greedily identify the minimum set of census dates responsible for bad pairs
#'
#' Iteratively removes the census date that appears most often across bad
#' pairs until no bad pairs remain or only one pair is left.
#'
#' @param p data.frame of pairs for one stem, containing `census_t0`,
#'   `census_tT`, and `bad_meas`
#' @return numeric vector of bad census dates, or `NULL` if none found
#'
#' @keywords internal
#' @noRd
#' 
findBadCensuses <- function(p) {
  bad_census <- c()
  p_tmp <- p
  while (any(p_tmp$bad_meas) && nrow(p_tmp) > 1L) {
    bad_dates <- c(
      p_tmp$census_t0[p_tmp$bad_meas],
      p_tmp$census_tT[p_tmp$bad_meas]
    )
    counts <- sort(table(bad_dates), decreasing = TRUE)
    worst <- names(counts)[1L]
    p_tmp <- p_tmp[p_tmp$census_t0 != worst & p_tmp$census_tT != worst, ]
    bad_census <- c(bad_census, worst)
  }
  if (length(bad_census) == 0L) {
    return(NULL) 
  } else { 
    return(bad_census)
  }
}

#' Identify and realign permanent shifts in a diameter trajectory
#'
#' @param y single-stem subset of `x`, ordered by `census_date`
#' @param ind_id `r param_id("individual stems")`
#' @param plot_id `r param_plot_id()`
#' @param census_date `r param_census_date()`
#' @param diam `r param_diam()`
#' @param growth_thresh numeric vector of length 2 (min, max acceptable growth)
#' @param digits number of decimal places for rounding
#' 
#' @return data.frame of rows with corrected diameters, or NULL if no shift
#' 
#' @keywords internal
#' @noRd
#' 
imputeShift <- function(y, ind_id, plot_id, census_date, diam, growth_thresh, digits) {

  # Need at least 3 points to confirm a shift (before, jump, after)
  n <- nrow(y)
  if (n < 3) {
    return(NULL) 
  }
  
  # Calculate sequential increments
  t_diff <- as.numeric(diff(y[[census_date]])) / 365.25
  d_diff <- diff(y[[diam]])
  growth_seq <- d_diff / t_diff
  
  # Flag abnormal sequential growth
  is_abnormal <- !is.na(growth_seq) & 
    (growth_seq < growth_thresh[1] | 
      growth_seq > growth_thresh[2])
  
  corrected_indices <- c()
  diam_cor <- y[[diam]]
  cumulative_shift <- 0
  
  # Loop through sequential increments to find shift topologies
  for (i in 1:(n - 1)) {
    # A shift is defined as: An abnormal jump...
    if (is_abnormal[i]) {
      # ...followed by a normal increment (meaning the series stabilized at a new, wrong baseline)
      if (i < (n - 1) && !is_abnormal[i + 1]) {
        
        # Calculate the magnitude of the false jump. 
        # We conservatively assume the 'true' growth during the jump period was 0
        shift_magnitude <- d_diff[i] 
        cumulative_shift <- cumulative_shift + shift_magnitude
        
        # Apply the correction to THIS measurement and ALL subsequent measurements
        # (This is handled by accumulating the shift)
      }
    }
    
    # If a shift has occurred historically, apply the cumulative realignment
    if (cumulative_shift != 0) {
      target_idx <- i + 1
      diam_cor[target_idx] <- y[[diam]][target_idx] - cumulative_shift
      corrected_indices <- c(corrected_indices, target_idx)
    }
  }
  
  if (length(corrected_indices) == 0) return(NULL)
  
  # Format output to match imputePairwise columns exactly so rbind() works
  keep_cols <- unique(c(ind_id, plot_id, census_date, diam, ".stem_key"))
  out_rows <- y[unique(corrected_indices), keep_cols, drop = FALSE]
  
  out_rows$diam_cor <- round(diam_cor[unique(corrected_indices)], digits = digits)
  out_rows$correction_method <- "shift realignment"
  
  return(out_rows)
}

#' Helper to apply pairwise mean imputation logic
#'
#' @keywords internal
#' @noRd
imputePairwise <- function(x_multi, x_split, ind_id, plot_id, census_date, diam, 
  status, pom, growth_thresh, n_stem_thresh, digits) {
  
  # Build pairwise growth tables ---------------------------------------------------
  pairs_list <- lapply(x_split, function(y) {
    p <- buildPairs(y, census_date, diam, status, pom)
    p$.stem_key <- y$.stem_key[1L]
    p$.group_key <- y$.group_key[1L]
    p$pom_change <- p$pom_t0 != p$pom_tT
    p$bad_meas <- (
      (!is.na(p$growth_ann) &
        (p$growth_ann > growth_thresh[2] | p$growth_ann < growth_thresh[1]) &
        !p$pom_change) |
      is.na(p$diam_t0) | is.na(p$diam_tT)
    )
    p
  })
  pairs <- do.call(rbind, pairs_list)
  rownames(pairs) <- NULL

  # Identify bad census dates ------------------------------------------------------
  has_bad <- tapply(pairs$bad_meas, pairs$.stem_key, any)
  has_good <- tapply(!pairs$bad_meas, pairs$.stem_key, any)
  bad_stems <- names(has_bad[has_bad & has_good])

  pairs_bad_split <- split(
    pairs[pairs$.stem_key %in% bad_stems, ],
    pairs[pairs$.stem_key %in% bad_stems, ]$.stem_key
  )

  bad_census_df <- do.call(rbind, lapply(names(pairs_bad_split), function(sk) {
    bc <- as.Date(findBadCensuses(pairs_bad_split[[sk]]))
    if (is.null(bc)) {
      return(NULL)
    } else {
      data.frame(.stem_key = sk, bad_census = bc, stringsAsFactors = FALSE)
    }
  }))

  missing_df <- x_multi[is.na(x_multi[[diam]]), c(".stem_key", census_date)]
  names(missing_df)[names(missing_df) == census_date] <- "bad_census"

  all_bad <- unique(rbind(bad_census_df, missing_df))

  if (nrow(all_bad) == 0L) {
    return(NULL)
  }

  # Compute mean growth rates from good pairs -------------------------------------
  bad_lookup <- paste(all_bad$.stem_key, all_bad$bad_census)
  good_pairs <- pairs[
    !paste(pairs$.stem_key, pairs$census_t0) %in% bad_lookup &
      !paste(pairs$.stem_key, pairs$census_tT) %in% bad_lookup &
      pairs$status_t0 == TRUE & pairs$status_tT == TRUE &
      !pairs$pom_change &
      !is.na(pairs$growth_ann),
  ]

  group_mean <- tapply(good_pairs$growth_ann, good_pairs$.group_key, mean, na.rm = TRUE)

  stem_mean <- tapply(good_pairs$growth_ann, good_pairs$.stem_key, mean, na.rm = TRUE)
  stem_n <- tapply(good_pairs$growth_ann, good_pairs$.stem_key, function(g) { sum(!is.na(g)) })

  # Impute bad measurements -------------------------------------------------------
  bad_stem_keys <- unique(all_bad$.stem_key)

  out <- do.call(rbind, lapply(bad_stem_keys, function(sk) {
    y <- x_split[[sk]]
    bad_dates <- all_bad$bad_census[all_bad$.stem_key == sk]
    gk <- y$.group_key[1L]

    # Anchor: good measurements for this stem
    good_mask <- !y[[census_date]] %in% bad_dates & !is.na(y[[diam]])

    if (!any(good_mask)) {
      return(NULL)
    }

    anchor_diam <- y[[diam]][good_mask]
    anchor_date <- y[[census_date]][good_mask]

    # Choose imputation rate and label
    if (sk %in% names(stem_n)) {
      n_good <- stem_n[[sk]] 
    } else {
      n_good <- 0L
    }
    use_stem <- n_good >= n_stem_thresh

    if (use_stem && sk %in% names(stem_mean)) {
      rate <- stem_mean[[sk]]
    } else if (gk %in% names(group_mean)) {
      rate <- group_mean[[gk]]
    } else {
      rate <- NA_real_
    }

    if (use_stem) {
      meth <- "stem mean" 
    } else { 
      meth <- "group mean"
    }

    if (is.na(rate)) {
      return(NULL)
    }

    # Project from every anchor to every bad date, then average projections
    imputed <- vapply(bad_dates, function(bd) {
      preds <- anchor_diam + rate * (as.numeric(bd - anchor_date) / 365.25)
      mean(preds, na.rm = TRUE)
    }, numeric(1L))

    # Match back to original rows
    row_idx <- match(bad_dates, y[[census_date]])
    keep_cols <- unique(c(ind_id, plot_id, census_date, diam, ".stem_key"))
    rows <- y[row_idx, keep_cols, drop = FALSE]
    
    rows$diam_cor <- round(imputed, digits = digits)
    rows$correction_method <- meth
    rows
  }))

  return(out)
}



###############
# MAIN FUNCTION
###############

#' Correct diameter timelines using mean growth rate imputation
#'
#' Flags anomalous diameter measurements by computing annual growth rates
#' across all pairwise census combinations for each stem, then imputing
#' flagged values by projecting from good measurements using a mean annual
#' growth rate. The mean rate is drawn from the stem's own history if
#' sufficient good increments exist, otherwise from the plot or group mean.
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param plot_id `r param_plot_id()`. To compute mean growth rates
#' @param census_date `r param_census_date()`
#' @param diam `r param_diam()`
#' @param status `r param_status()`
#' @param pom column name in `x` with point of measurement values. Pairs
#'   spanning a POM change are excluded from growth rate calculations but
#'   are not themselves flagged as errors.
#' @param growth_thresh negative and positive annual growth threshold in cm/year. 
#'     Pairs with growth exceeding this threshold are flagged. If a vector of
#'     length 2 is provided, the values are interpreted as a range. If a vector
#'     of length 1 is provided this value is interpreted as an absolute
#'     threshold and is applied to both negative and positive growth
#'     increments.
#' @param n_stem_thresh minimum number of good growth increments required to
#'   use stem-level rather than group-level mean imputation 
#' @param digits number of decimal places for the corrected diameter column
#' @param method character vector specifying the error correction method(s) to apply. 
#'   Options are `"shift"` and/or `"pairwise"`. Multiple methods can be supplied 
#'   and will be executed sequentially. 
#'   - `"shift"`: Identifies and realigns permanent shifts in a diameter trajectory 
#'     (e.g., caused by a slipped tape or unrecorded change in point of measurement) 
#'     by diagnosing abnormal jumps followed by normal growth. It realigns the 
#'     trajectory without discarding valid subsequent measurements.
#'   - `"pairwise"`: Identifies punctual anomalous spikes or dips by evaluating 
#'     growth across all pairwise census combinations for a stem. Flagged values 
#'     are imputed using a mean annual growth rate derived from the stem's own 
#'     history or the plot group mean.
#'   Defaults to `c("shift", "pairwise")`, which optimally resolves permanent 
#'   structural shifts first before smoothing any remaining localized spikes.
#' @param comment `r param_comment()`
#'
#' @details
#' Error detection follows a greedy vote procedure. For each stem, all
#' pairwise growth rates are computed. Pairs where growth exceeds the
#' threshold (excluding POM changes) or where either measurement is missing
#' are flagged. The census date appearing most frequently across bad pairs is
#' removed iteratively until no bad pairs remain. Missing diameter values are
#' also flagged unconditionally.
#'
#' Imputation projects forward and backward from every good anchor
#' measurement using the mean rate, then averages across all projections. If
#' a stem has `>= n_stem_thresh` good increments its own mean rate is used;
#' otherwise the group mean is used. Stems with no good measurements at all
#' cannot be imputed and are silently omitted from the output.
#'
#' @return dataframe of rows in `x` where a diameter correction was applied,
#'   containing the original `ind_id`, `census_date`, and `diam` columns plus:
#'   - `diam_cor`: imputed diameter (rounded to `digits`)
#'   - `correction_method`: `"stem mean"` or `"group mean"`
#'   - `comment` (if provided)
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   plot_id = "P1",
#'   stem_id = rep(c("A", "B"), each = 5),
#'   year = rep(c(2000, 2005, 2010, 2015, 2020), 2),
#'   diameter = c(10, 11, 30, 13, 14, # spike at 2010
#'   10, 11, 12, 13, 14),
#'   alive = TRUE,
#'   pom = 1.3
#' )
#'
#' correctDiameterSeries(x,
#'   ind_id = "stem_id",
#'   plot_id = "plot_id",
#'   census_date = "year",
#'   diam = "diameter",
#'   status = "alive",
#'   pom = "pom"
#' )
#'
correctDiameterSeries <- function(x, ind_id, plot_id, census_date, diam, status,
  pom, growth_thresh  = 4, n_stem_thresh = 3L, digits = 1L, 
  method = c("shift", "pairwise"), comment = NULL) {

  # Input checks -------------------------------------------------------------------
  method <- match.arg(method, several.ok = TRUE)
  columnCatch(x, ind_id, plot_id, census_date, diam, status, pom)
  x[[diam]] <- classCatch(x[[diam]], "numeric")
  x$.date_fmt <- classCatch(x[[census_date]], "Date")
  x[[status]] <- classCatch(x[[status]], "logical")
  x[[pom]] <- classCatch(x[[pom]], "numeric")
  growth_thresh <- classCatch(growth_thresh, "numeric")
  n_stem_thresh <- classCatch(n_stem_thresh, "numeric")
  digits <- classCatch(digits, "numeric")

  if (length(growth_thresh) == 1) {
    growth_thresh <- c(-abs(growth_thresh), abs(growth_thresh))
  } else if (length(growth_thresh) > 2) { 
    stop("'growth_thresh' must be a vector of length 1 or 2")
  }

  # Sort, build compound keys ------------------------------------------------------
  x <- x[do.call(order, x[, c(ind_id, ".date_fmt"), drop = FALSE]), ]
  x$.stem_key <- do.call(paste, c(x[, ind_id, drop = FALSE], sep = "::"))
  x$.group_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))

  # Save original diameter to preserve it for the final output log
  orig_diam_col <- paste0(".orig_", diam)
  x[[orig_diam_col]] <- x[[diam]]

  # Filter to multi-census stems ---------------------------------------------------
  n_cens <- tapply(x$.stem_key, x$.stem_key, length)
  x_multi <- x[x$.stem_key %in% names(n_cens[n_cens > 1L]), ]

  if (nrow(x_multi) == 0L) {
    return(NULL)
  }

  x_split <- split(x_multi, x_multi$.stem_key)
  out_list <- list()

  # --------------------------------------------------------------------------------
  # SHIFT CORRECTION
  # --------------------------------------------------------------------------------
  if ("shift" %in% method) {
    out_shift <- do.call(rbind, lapply(x_split, function(y) {
      imputeShift(y, ind_id, plot_id, ".date_fmt", diam, growth_thresh, digits)
    }))
    
    if (!is.null(out_shift) && nrow(out_shift) > 0L) {
      out_list$shift <- out_shift
      
      match_key_out <- paste(out_shift$.stem_key, out_shift$.date_fmt, sep = "::")
      match_key_multi <- paste(x_multi$.stem_key, x_multi$.date_fmt, sep = "::") 

      idx <- match(match_key_out, match_key_multi)
      x_multi[[diam]][idx] <- out_shift$diam_cor
      
      # Re-split the newly updated data for the next step
      x_split <- split(x_multi, x_multi$.stem_key)
    }
  }

  # --------------------------------------------------------------------------------
  # PAIRWISE CORRECTION
  # --------------------------------------------------------------------------------
  if ("pairwise" %in% method) {
    out_pw <- imputePairwise(x_multi, x_split, ind_id, plot_id, ".date_fmt", 
                                 diam, status, pom, growth_thresh, n_stem_thresh, digits)
    
    if (!is.null(out_pw) && nrow(out_pw) > 0L) {
      
      # Restore the true original raw diameter for the output log
      match_key_pw <- paste(out_pw$.stem_key, out_pw$.date_fmt, sep = "::")
      match_key_x <- paste(x$.stem_key, x$.date_fmt, sep = "::")
      out_pw[[diam]] <- x[[orig_diam_col]][match(match_key_pw, match_key_x)] 

      out_list$pw <- out_pw
    }
  }

  # --------------------------------------------------------------------------------
  # MERGE AND FORMAT OUTPUT
  # --------------------------------------------------------------------------------
  if (length(out_list) == 0L) {
    return(NULL)
  }
  
  if (length(out_list) == 1L) {
    out <- out_list[[1L]]
  } else {
    # If both ran and found errors, merge logs. If a row was corrected twice, 
    # keep the final pairwise calculation and combine the method strings.
    out_shift <- out_list$shift
    out_pw <- out_list$pw
    
    key_shift <- paste(out_shift$.stem_key, out_shift$.date_fmt, sep = "::")
    key_pw <- paste(out_pw$.stem_key, out_pw$.date_fmt, sep = "::") 

    # Rows only in shift
    only_shift <- out_shift[!key_shift %in% key_pw, ]
    # Rows only in pairwise
    only_pw <- out_pw[!key_pw %in% key_shift, ]
    # Rows corrected by BOTH
    both <- out_pw[key_pw %in% key_shift, ]
    if (nrow(both) > 0L) {
      both$correction_method <- paste(
        out_shift$correction_method[match(paste(both$.stem_key, both$.date_fmt, sep = "::"), key_shift)], 
        "+", both$correction_method
      )
    } 

    out <- do.call(rbind, list(only_shift, only_pw, both))
    # Re-sort to keep output tidy
    out <- out[do.call(order, out[, c(ind_id, ".date_fmt"), drop = FALSE]), ]
  }

  if (!is.null(comment)) {
    out$comment <- comment
  }

  out$.stem_key <- NULL
  rownames(out) <- NULL
  out[[census_date]] <- as(out$.date_fmt, class(x[[census_date]]))
  out$.date_fmt <- NULL

  # Return
  return(out) 
}

#' Plot diameter correction results
#'
#' Takes the original dataset and the output of correctDiameterSeries() and 
#' generates paginated plots showing the original vs. corrected trajectories.
#'
#' @param x original full dataframe (the same `x` passed to `correctDiameterSeries()`)
#' @param cor output dataframe returned by `correctDiameterSeries()`
#' @param ind_id `r param_id("individual stems")`
#' @param census_date `r param_census_date()`
#' @param diam `r param_diam()`
#' @param pom `r param_pom(optional = TRUE)` 
#'
#' @return list of ggplot objects, printed to the active device.
#'
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @export
#'
plotDiameterCorrections <- function(x, cor, ind_id, census_date, diam, pom = NULL) {
  
  if (is.null(cor) || nrow(cor) == 0L) {
    message("No corrections found to plot.")
    return(invisible(NULL))
  }
  
  # Create string key for individual stems
  x$.stem_key <- do.call(paste, c(x[, ind_id, drop = FALSE], sep = "::"))
  cor$.stem_key <- do.call(paste, c(cor[, ind_id, drop = FALSE], sep = "::"))
  
  # Filter raw data to only include stems that have at least one correction
  bad_stems <- unique(cor$.stem_key)
  x_fil <- x[x$.stem_key %in% bad_stems, ]
  
  # Merge corrected values and methods back into data
  plot_data <- merge(
    x_fil,
    cor[, c(".stem_key", census_date, "diam_cor", "correction_method")],
    by = c(".stem_key", census_date),
    all.x = TRUE
  )
  
  # Create continuous 'Final' diameter column 
  # uses original diam if no correction occurred that year
  plot_data$diam_final <- ifelse(is.na(plot_data$diam_cor), plot_data[[diam]], plot_data$diam_cor)
  
  # Ensure chronological order for proper line drawing
  plot_data <- plot_data[
    do.call(order, plot_data[, c(".stem_key", census_date), drop = FALSE]), ]
  
  # Generate plots
  plots <- list()
  for (stem in bad_stems) {

    # Subset data for this specific stem
    stem_data <- plot_data[plot_data$.stem_key == stem, ]
    
    p_plot <- ggplot2::ggplot(stem_data, ggplot2::aes(x = .data[[census_date]])) +
      
      # ORIGINAL Trajectory (Dashed Red)
      ggplot2::geom_line(ggplot2::aes(y = .data[[diam]], color = "Initial"), 
                         linetype = "dashed", alpha = 0.6) +
      ggplot2::geom_point(ggplot2::aes(y = .data[[diam]], color = "Initial"), 
                          size = 2, alpha = 0.6) +
      
      # CORRECTED Trajectory (Solid Green)
      ggplot2::geom_line(ggplot2::aes(y = diam_final, color = "Final/Corrected"), 
                         linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = diam_final, color = "Final/Corrected"), 
                          size = 2.5) +
      
      # Labels for Correction Method
      ggrepel::geom_text_repel(
        ggplot2::aes(y = diam_cor, label = correction_method),
        na.rm = TRUE, color = "purple", size = 3, 
        nudge_y = 1, box.padding = 0.5, direction = "y"
      ) +
      
      # Optional POM labels
      {
        if (!is.null(pom) && pom %in% names(stem_data)) { 
          ggrepel::geom_text_repel(
            ggplot2::aes(y = diam_final, label = paste("POM:", .data[[pom]])),
            color = "blue", size = 2.5, box.padding = 0.3, alpha = 0.7
          )
        }
      } +
      
      # Styling
      ggplot2::scale_color_manual(
        name = "Measurement",
        values = c("Initial" = "red", "Final/Corrected" = "forestgreen"),
        limits = c("Initial", "Final/Corrected")
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      
      # Title and Axis Labels
      ggplot2::labs(title = paste("Stem:", stem), 
                    x = "Census Date", 
                    y = "Diameter (cm)")
    
    # Save plot to list using the stem key as the name
    plots[[stem]] <- p_plot
  }

  # Return
  return(plots)
} 
