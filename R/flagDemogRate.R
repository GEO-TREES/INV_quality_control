#' Flag implausible mortality and recruitment rates between censuses
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param plot_id column name(s) in `x` identifying plots
#' @param census_id `r param_census_id()`
#' @param census_date `r param_census_date()`, or decimal years
#' @param status `r param_status()`
#' @param threshold maximum annual mortality or recruitment rate, e.g. 0.05 
#'     for 5% per year
#' @param comment `r param_comment()`
#'
#' @details
#' For each pair of consecutive censuses in each plot, stems alive in the 
#' first census which are alive in the second census are survivors, and 
#' stems alive in the second census which were not recorded in the first 
#' census are recruits. Annual mortality and recruitment rates are 
#' calculated following Sheil et al. (1995) and Kohyama et al. (2018): 
#' `mortality = 1 - (n_surv / n_t0)^(1 / years)` and 
#' `recruitment = 1 - (n_surv / n_t1)^(1 / years)`. Stems alive in the first
#' census with no record in the second census are counted as not surviving, 
#' and are reported separately as `n_absent`.
#'
#' Annual mortality and recruitment rates in old-growth forests are 
#' typically 1-3%. High mortality may indicate stems which were missed, or 
#' retagged, see `flagTagChange()`. High recruitment may indicate stems 
#' which were missed in the previous census, a change in the minimum 
#' diameter threshold, or retagged stems.
#'
#' @return dataframe with plot census intervals where either rate is greater
#'     than `threshold`, with the number of stems alive in each census, the 
#'     numbers of survivors, dead, absent and recruit stems, and the rates
#' 
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   plot_id = "A",
#'   stem_id = rep(1:200, each = 2),
#'   census_id = rep(1:2, 200),
#'   census_date = rep(c(2010, 2015), 200),
#'   alive = TRUE
#' )
#' # 30% of stems dead in census 2
#' df$alive[df$census_id == 2][1:60] <- FALSE
#' 
#' flagDemogRate(df, c("plot_id", "stem_id"), "plot_id", "census_id", 
#'   "census_date", "alive")
#' 
#' @export
#' 
flagDemogRate <- function(x, ind_id, plot_id, census_id, census_date, status, 
  threshold = 0.05, comment = NULL) {

  # Check input
  columnCatch(x, ind_id, plot_id, census_id, census_date, status)

  x[[status]] <- classCatch(x[[status]], "logical")
  x$..ord.. <- censusOrder(x[[census_id]])
  x$..date.. <- decYear(x[[census_date]])
  x$..stem.. <- do.call(paste, c(x[, ind_id, drop = FALSE], sep = "::"))
  plot_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))

  out <- do.call(rbind, lapply(split(x, plot_key), function(y) {
    cens <- sort(unique(y$..ord..))
    if (length(cens) < 2) {
      return(NULL)
    }
    do.call(rbind, lapply(seq_len(length(cens) - 1), function(i) {
      y0 <- y[y$..ord.. == cens[i], , drop = FALSE]
      y1 <- y[y$..ord.. == cens[i + 1], , drop = FALSE]
      alive0 <- unique(y0$..stem..[y0[[status]] %in% TRUE])
      alive1 <- unique(y1$..stem..[y1[[status]] %in% TRUE])
      n_surv <- sum(alive0 %in% alive1)
      years <- stats::median(y1$..date.., na.rm = TRUE) - 
        stats::median(y0$..date.., na.rm = TRUE)

      res <- y0[1, plot_id, drop = FALSE]
      res$census_t0 <- y0[[census_id]][1]
      res$census_t1 <- y1[[census_id]][1]
      res$years <- years
      res$n_t0 <- length(alive0)
      res$n_t1 <- length(alive1)
      res$n_surv <- n_surv
      res$n_dead <- sum(alive0 %in% y1$..stem..[y1[[status]] %in% FALSE])
      res$n_absent <- sum(!alive0 %in% y1$..stem..)
      res$n_recruit <- sum(!alive1 %in% y0$..stem..)
      res$mort_ann <- 1 - (n_surv / res$n_t0)^(1 / years)
      res$recr_ann <- 1 - (n_surv / res$n_t1)^(1 / years)
      res
    }))
  }))

  if (is.null(out)) {
    message("No plots with more than one census")
    out <- x[0, plot_id, drop = FALSE]
  } else {
    out <- out[(!is.na(out$mort_ann) & out$mort_ann > threshold) | 
      (!is.na(out$recr_ann) & out$recr_ann > threshold), , drop = FALSE]
  }
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("High mortality or recruitment rates detected")
  }

  # Return
  return(out)
}
