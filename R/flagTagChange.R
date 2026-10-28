#' Flag recruits which may be retagged stems from the previous census
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param plot_id column name(s) in `x` identifying plots
#' @param census_id `r param_census_id()`
#' @param x_rel `r param_xy_rel("X")`
#' @param y_rel `r param_xy_rel("Y")`
#' @param diam `r param_diam()`
#' @param status `r param_status()`
#' @param threshold maximum distance (m) between a recruit and the stem it may 
#'     replace
#' @param diam_tol maximum relative difference in diameter between a recruit
#'     and the stem it may replace
#' @param comment `r param_comment()`
#'
#' @details
#' For each pair of consecutive censuses in each plot, stems which were alive
#' in the first census, but are dead or absent in the second census, are 
#' compared to recruits in the second census, i.e. live stems which were not 
#' recorded in any previous census. A recruit which is close to a 
#' disappeared stem, with a similar diameter, may be the same stem with a new
#' tag. Retagged stems inflate mortality and recruitment rates, and break 
#' diameter timelines, so that errors in the focal census cannot be found 
#' from previous censuses.
#'
#' @return dataframe with values of `ind_id` and `census_id` for possible 
#'     retagged recruits, with their diameter, and the ID, diameter and 
#'     distance of the disappeared stem they may replace
#' 
#' @examples
#' df <- data.frame(
#'   plot_id = "A",
#'   stem_id = c(1, 1, 2, 2, 3, 4),
#'   census_id = c(1, 2, 1, 2, 2, 2),
#'   x = c(10, 10, 50, 50, 10.3, 80),
#'   y = c(10, 10, 50, 50, 10.1, 80),
#'   dbh = c(30, NA, 20, 21, 30.5, 10),
#'   alive = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
#' )
#' 
#' flagTagChange(df, c("plot_id", "stem_id"), "plot_id", "census_id", 
#'   "x", "y", "dbh", "alive")
#' 
#' @export
#' 
flagTagChange <- function(x, ind_id, plot_id, census_id, x_rel, y_rel, diam, 
  status, threshold = 1, diam_tol = 0.1, comment = NULL) {

  # Check input
  columnCatch(x, ind_id, plot_id, census_id, x_rel, y_rel, diam, status)

  x[[status]] <- classCatch(x[[status]], "logical")
  x[[diam]] <- classCatch(x[[diam]], "numeric")
  x$..ord.. <- classCatch(x[[census_id]], "integer")
  x$..stem.. <- do.call(paste, c(x[, ind_id, drop = FALSE], sep = "::"))
  plot_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))

  out <- do.call(rbind, lapply(split(x, plot_key), function(y) {
    cens <- sort(unique(y$..ord..))
    if (length(cens) < 2) {
      return(NULL)
    }
    first_ord <- tapply(y$..ord.., y$..stem.., min)
    do.call(rbind, lapply(seq_len(length(cens) - 1), function(i) {
      y0 <- y[y$..ord.. == cens[i], , drop = FALSE]
      y1 <- y[y$..ord.. == cens[i + 1], , drop = FALSE]

      # Stems alive in t0 which are dead or absent in t1
      alive1 <- y1$..stem..[y1[[status]] %in% TRUE]
      gone <- y0[y0[[status]] %in% TRUE & !y0$..stem.. %in% alive1 & 
        !is.na(y0[[diam]]), , drop = FALSE]

      # Recruits in t1
      rec <- y1[y1[[status]] %in% TRUE & first_ord[y1$..stem..] == cens[i + 1] & 
        !is.na(y1[[diam]]), , drop = FALSE]

      if (nrow(gone) == 0 || nrow(rec) == 0) {
        return(NULL)
      }

      # Pairs of disappeared stems and recruits within `threshold`
      both <- rbind(gone, rec)
      p <- pairsWithin(classCatch(both[[x_rel]], "numeric"), 
        classCatch(both[[y_rel]], "numeric"), threshold)
      p <- p[p$i <= nrow(gone) & p$j > nrow(gone), , drop = FALSE]
      if (nrow(p) == 0) {
        return(NULL)
      }
      d0 <- both[[diam]][p$i]
      d1 <- both[[diam]][p$j]
      p <- p[abs(d1 - d0) / pmax(d0, d1) <= diam_tol, , drop = FALSE]
      if (nrow(p) == 0) {
        return(NULL)
      }

      res <- both[p$j, unique(c(ind_id, census_id, diam)), drop = FALSE]
      res$stem_prev <- both$..stem..[p$i]
      res$census_prev <- both[[census_id]][p$i]
      res$diam_prev <- both[[diam]][p$i]
      res$dist_m <- p$dist
      res
    }))
  }))

  if (is.null(out)) {
    out <- x[0, unique(c(ind_id, census_id, diam)), drop = FALSE]
  }
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Possible retagged stems detected")
  }

  # Return
  return(out)
}

#' Find pairs of points within a distance of each other
#'
#' @param px numeric vector of X coordinates
#' @param py numeric vector of Y coordinates
#' @param d maximum distance
#'
#' @return dataframe with columns `i` and `j`, indices of pairs of points 
#'     with `i < j`, and `dist`, the distance between them
#' 
#' @keywords internal
#' @noRd
#' 
pairsWithin <- function(px, py, d) {
  ok <- which(!is.na(px) & !is.na(py))
  empty <- data.frame(i = integer(0), j = integer(0), dist = numeric(0))
  if (length(ok) < 2) {
    return(empty)
  }

  # Assign points to grid cells of size `d`, and compare points in 
  # neighbouring cells only
  cells <- data.frame(id = ok, cx = floor(px[ok] / d), cy = floor(py[ok] / d))
  out <- do.call(rbind, lapply(-1:1, function(dx) {
    do.call(rbind, lapply(-1:1, function(dy) {
      nb <- cells
      nb$cx <- nb$cx + dx
      nb$cy <- nb$cy + dy
      m <- merge(cells, nb, by = c("cx", "cy"), suffixes = c("_i", "_j"))
      m <- m[m$id_i < m$id_j, , drop = FALSE]
      data.frame(i = m$id_i, j = m$id_j)
    }))
  }))
  out <- unique(out)
  out$dist <- sqrt((px[out$i] - px[out$j])^2 + (py[out$i] - py[out$j])^2)
  out <- out[out$dist <= d, , drop = FALSE]
  rownames(out) <- NULL
  out
}
