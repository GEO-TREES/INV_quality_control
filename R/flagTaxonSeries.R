#' Flag individuals where taxonomic name differs among censuses
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param taxon_name `r param_taxon_name()`
#' @param census_id `r param_census_id()`
#' @param comment `r param_comment()`
#'
#' @return dataframe with values of `ind_id` where `taxon_name` differs among censuses
#' 
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 2, 3, 3),
#'   census = c(1, 2, 1, 2, 1, 2),
#'   sp = c("A", "B", "C", "C", "D", "D")
#' )

#' flagTaxonSeries(df, "sp", "id", "census")
#' 
#' @export
#' 
flagTaxonSeries <- function(x, ind_id, taxon_name, census_id, comment = NULL) { 

  # Check columns exist
  columnCatch(x, ind_id, taxon_name, census_id)

  # Aggregate to get count and concatenated taxonomic names
  out <- aggregate(x[taxon_name], by = x[ind_id], 
    FUN = function(y) { 
      u <- unique(na.omit(y))
      # Return a named vector with both pieces of information
      c(n_unique = length(u), 
      taxon_name = paste(u, collapse = ";"))
    })

  # Flatten the matrix column that aggregate() creates into normal columns
  out <- cbind(out[ind_id], as.data.frame(out[[taxon_name]]))
  out_fil <- out[out$n_unique > 1, c(ind_id, "taxon_name")]
  names(out_fil)[ncol(out_fil)] <- taxon_name
  rownames(out_fil) <- NULL

  # Generate comment
  if (!is.null(comment) & nrow(out_fil) > 0) {
    out_fil$comment <- comment
  }

  # Generate message
  if (nrow(out_fil) > 0) {
    message("Taxon names differ among censuses")
  }
  
  # Return
  return(out_fil)
}
