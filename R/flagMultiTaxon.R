#' Flag trees where stems differ in taxonomic name 
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual trees")`
#' @param taxon_name `r param_taxon_name()`
#' @param census_id `r param_census_id(optional = TRUE)`
#' @param comment `r param_comment()`
#'
#' @return dataframe with values of `ind_id` with multiple taxonomic names
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 2, 2),
#'   year = c(2010, 2010, 2010, 2010),
#'   sp = c("Quercus alba", "Quercus rubra", "Acer rubrum", "Acer rubrum")
#' )
#' 
#' flagMultiTaxon(df, "tree_id", "year", "sp")
#' 
#' @export
flagMultiTaxon <- function(x, ind_id, taxon_name, census_id = NULL, comment = NULL) {
  
  # Check input
  columnCatch(x, ind_id, census_id, taxon_name)

  # Split the taxonomic names by this group key
  x_split <- split(x, as.list(x[,c(ind_id, census_id), drop = FALSE]), drop = TRUE)
  
  # Check if any group has more than 1 unique name
  out <- do.call(rbind, lapply(x_split, function(y) {
    if (length(unique(y[[taxon_name]])) > 1) {
      df <- unique(y[,c(ind_id, census_id), drop = FALSE])
      df$taxon_name <- paste(y[[taxon_name]], collapse = ";")
      df
    }
  }))
  row.names(out) <- NULL

  # Generate comment
  if (!is.null(comment) & nrow(out) > 0) {
    out$comment <- comment
  } 

  # Generate message
  if (nrow(out) < 0) {
    message("Multi-stemmed individuals with multiple taxonomic names detected")
  }
  
  # Return
  return(out)
}
