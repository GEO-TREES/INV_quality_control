#' Flag stems which may be duplicated, with the same location and diameter
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param plot_id column name(s) in `x` identifying groups within which 
#'     stems are compared, e.g. `c("plot_id", "census_id")`
#' @param x_rel `r param_xy_rel("X")`
#' @param y_rel `r param_xy_rel("Y")`
#' @param diam `r param_diam(optional = TRUE)` If provided, stems are only 
#'     flagged if their diameters are similar
#' @param tree_id optional, column name in `x` with tree IDs. Stems of the 
#'     same tree are not compared, as they often share the coordinates of 
#'     the tree
#' @param dist maximum distance (m) between duplicated stems
#' @param diam_tol maximum relative difference in diameter between 
#'     duplicated stems
#' @param comment `r param_comment()`
#'
#' @details
#' Stems which were given a new tag without removing the old record, or 
#' records which were entered twice with different stem IDs, are counted 
#' twice in AGB density estimates. These may be found as pairs of stems from
#' different trees with nearly identical coordinates and diameters. 
#'
#' @return dataframe with values of `meas_id` for possible duplicated stems, 
#'     with the coordinates, diameter, and a `dup_group` integer shared by 
#'     stems which may be duplicates of each other
#' 
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   plot_id = "A",
#'   tree_id = c(1, 2, 3, 4, 4),
#'   x = c(10, 10.05, 30, 50, 50),
#'   y = c(10, 10, 30, 50, 50),
#'   dbh = c(25.1, 25.3, 12, 15, 15)
#' )
#' 
#' flagStemDupLoc(df, "id", "plot_id", "x", "y", diam = "dbh", 
#'   tree_id = "tree_id")
#' 
#' @export
#' 
flagStemDupLoc <- function(x, meas_id, plot_id, x_rel, y_rel, diam = NULL, 
  tree_id = NULL, dist = 0.2, diam_tol = 0.05, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, plot_id, x_rel, y_rel, diam, tree_id)

  px <- classCatch(x[[x_rel]], "numeric")
  py <- classCatch(x[[y_rel]], "numeric")
  d <- if (!is.null(diam)) classCatch(x[[diam]], "numeric") else NULL
  plot_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))

  # Find pairs of stems within `dist` in each group
  pairs <- do.call(rbind, lapply(split(seq_len(nrow(x)), plot_key), function(i) {
    p <- pairsWithin(px[i], py[i], dist)
    if (nrow(p) == 0) {
      return(NULL)
    }
    data.frame(i = i[p$i], j = i[p$j])
  }))

  if (!is.null(pairs)) {
    if (!is.null(tree_id)) {
      pairs <- pairs[x[[tree_id]][pairs$i] != x[[tree_id]][pairs$j] | 
        is.na(x[[tree_id]][pairs$i]), , drop = FALSE]
    }
    if (!is.null(d)) {
      rel_diff <- abs(d[pairs$i] - d[pairs$j]) / pmax(d[pairs$i], d[pairs$j])
      pairs <- pairs[!is.na(rel_diff) & rel_diff <= diam_tol, , drop = FALSE]
    }
  }

  if (is.null(pairs) || nrow(pairs) == 0) {
    out <- x[0, unique(c(meas_id, plot_id, x_rel, y_rel, diam)), drop = FALSE]
    out$dup_group <- integer(0)
  } else {
    # Group connected pairs
    grp <- connectedGroups(pairs$i, pairs$j)
    out <- x[grp$id, unique(c(meas_id, plot_id, x_rel, y_rel, diam)), drop = FALSE]
    out$dup_group <- grp$group
    out <- out[order(out$dup_group), , drop = FALSE]
  }
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Possible duplicated stems detected")
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

#' Find connected groups from pairs of indices
#'
#' @param i integer vector of indices
#' @param j integer vector of indices, paired with `i`
#'
#' @return dataframe with columns `id`, each unique index, and `group`, an 
#'     integer identifying the connected group of each index
#' 
#' @keywords internal
#' @noRd
#' 
connectedGroups <- function(i, j) {
  ids <- sort(unique(c(i, j)))
  grp <- seq_along(ids)
  ii <- match(i, ids)
  jj <- match(j, ids)

  # Propagate the minimum group label along pairs until stable
  repeat {
    g_min <- pmin(grp[ii], grp[jj])
    m <- tapply(c(g_min, g_min), c(ii, jj), min)
    k <- as.integer(names(m))
    new <- grp
    new[k] <- pmin(new[k], m)
    new <- new[new]
    if (identical(new, grp)) {
      break
    }
    grp <- new
  }

  data.frame(id = ids, group = match(grp, unique(grp)))
}
