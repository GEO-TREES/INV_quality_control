#' Flag IDs which are nested within more than one parent ID
#'
#' @param x `r param_x_stem()`
#' @param group_id column name(s) in `x` within which `child_id` must be 
#'     unique, e.g. `c("site_id", "plot_id")`
#' @param parent_id column name in `x` with parent IDs, e.g. "tree_id"
#' @param child_id column name in `x` with child IDs, e.g. "stem_id"
#' @param comment `r param_comment()`
#'
#' @details
#' In the GEO-TREES format, stem IDs are unique within a plot, so each stem 
#' should only ever belong to one tree, across all censuses. Similarly, the 
#' stems of a tree are normally all within one subplot. This function finds 
#' child IDs which occur with more than one parent ID within a group.
#'
#' @return dataframe with values of `group_id` and `child_id` which occur 
#'     with more than one value of `parent_id`, with the parent IDs 
#'     concatenated
#' 
#' @examples
#' df <- data.frame(
#'   plot_id = "A",
#'   tree_id = c(1, 1, 2, 3),
#'   stem_id = c(1, 2, 2, 3)
#' )
#' 
#' # Stems in more than one tree
#' flagIdNest(df, "plot_id", "tree_id", "stem_id")
#' 
#' @export
#' 
flagIdNest <- function(x, group_id, parent_id, child_id, comment = NULL) {

  # Check input
  columnCatch(x, group_id, parent_id, child_id)

  # Unique combinations of IDs
  ids <- unique(x[!is.na(x[[child_id]]), c(group_id, child_id, parent_id), drop = FALSE])
  child_key <- do.call(paste, c(ids[, c(group_id, child_id), drop = FALSE], sep = "::"))

  # Count parents per child
  n_parent <- table(child_key)
  multi <- names(n_parent)[n_parent > 1]
  ids_multi <- ids[child_key %in% multi, , drop = FALSE]
  key_multi <- child_key[child_key %in% multi]

  out <- ids_multi[!duplicated(key_multi), c(group_id, child_id), drop = FALSE]
  out[[parent_id]] <- vapply(split(ids_multi[[parent_id]], key_multi), 
    function(i) { 
      paste(sort(unique(i)), collapse = ";") 
    }, character(1))[key_multi[!duplicated(key_multi)]]
  out$n_parent <- as.vector(n_parent[key_multi[!duplicated(key_multi)]])
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("IDs nested within more than one parent detected")
  }

  # Return
  return(out)
}
