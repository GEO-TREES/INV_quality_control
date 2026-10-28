#' Flag trees where no stems have spatial coordinates 
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual trees")`
#' @param x_rel `r param_xy_rel("X")`
#' @param y_rel `r param_xy_rel("Y")`
#' @param diam `r param_diam(optional = TRUE)` If provided, the basal area of 
#'     each tree is returned
#' @param comment `r param_comment()`
#'
#' @details
#' In the GEO-TREES INV_processing workflow, stems without coordinates in 
#' trees where other stems have coordinates are given the mean coordinates of
#' the other stems. Trees where no stems have coordinates are placed at 
#' random within their field subplot, or within the plot, in each AGB 
#' simulation, which increases the uncertainty of quadrat AGB density 
#' estimates. Large unmapped trees have the greatest effect.
#'
#' @return dataframe with values of `ind_id` for trees with no stem 
#'     coordinates, with the number of stems, and optionally the basal area 
#'     (m^2, if `diam` is in cm)
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 2, 3, 3),
#'   x = c(1, NA, NA, NA, NA),
#'   y = c(1, NA, NA, NA, NA),
#'   dbh = c(10, 12, 50, 20, 25)
#' )
#' 
#' flagTreeUnmapped(df, "tree_id", "x", "y", diam = "dbh")
#' 
#' @export
#' 
flagTreeUnmapped <- function(x, ind_id, x_rel, y_rel, diam = NULL, comment = NULL) {

  # Check input
  columnCatch(x, ind_id, x_rel, y_rel, diam)

  # Identify stems with coordinates
  mapped <- !is.na(x[[x_rel]]) & !is.na(x[[y_rel]])
  tree_key <- do.call(paste, c(x[, ind_id, drop = FALSE], sep = "::"))
  tree_mapped <- tapply(mapped, tree_key, any)
  unmapped <- !tree_mapped[tree_key]

  # Summarise unmapped trees
  out <- x[unmapped & !duplicated(tree_key), ind_id, drop = FALSE]
  key_out <- tree_key[unmapped & !duplicated(tree_key)]
  out$n_stem <- as.vector(table(tree_key)[key_out])

  if (!is.null(diam)) {
    ba <- pi * (classCatch(x[[diam]], "numeric") / 200)^2
    out$ba_m2 <- as.vector(tapply(ba, tree_key, sum, na.rm = TRUE)[key_out])
  }
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Trees without stem coordinates detected")
  }

  # Return
  return(out)
}
