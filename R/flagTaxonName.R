#' Flag Potential Issues in Taxonomic Names
#'
#' @description
#' Evaluates a data frame with taxonomic names and returns a character vector 
#' of flags/comments for names that violate standard nomenclature. 
#'
#' @details
#' The function performs two checks:
#' \itemize{
#'   \item \strong{Suffix checks:} Validates that family names end in "-aceae" 
#'   and ensures that genus or species names do not.
#'   \item \strong{Pattern matching:} Uses a regular expression to identify 
#'   forbidden characters (e.g. punctuation or symbols) across all provided 
#'   taxonomic name columns.
#' }
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param taxon_name `r param_taxon_name()`
#' @param family optional column name containing taxonomic family names in `x`
#' @param genus optional column name containing taxonomic genus names in `x`
#' @param species optional column name containing taxonomic species names in `x`
#' @param regex regular expression specifying allowed characters.
#'   Defaults to \code{"[^a-zA-Z ]"}, letters and spaces only.
#' @param comment `r param_comment()`
#'
#' @return dataframe with values of `meas_id` with issues in taxonomic names in `x`
#'
#' @examples
#' df <- data.frame(
#'   fam = c("Asteraceae", "Poa"), 
#'   gen = c("Abies", "Pinaceae"),
#'   spec = c("alba", "sylvestris"),
#'   full = c("Abies alba", "Pinaceae sylvestris")
#' )
#' 
#' # Generate the flag vector
#' df$flags <- flagTaxonName(df, taxon_name = "full", family = "fam", genus = "gen")
#'
#' @export
#'
flagTaxonName <- function(x, meas_id, taxon_name, family = NULL, genus = NULL, 
  species = NULL, regex = "[^a-zA-Z ]", comment = NULL) {

  # Check input
  columnCatch(x, meas_id, taxon_name, family, genus, species)

  out <- x[,c(meas_id, taxon_name, family, genus, species)]

  # Check for family names in species or genus
  if (!is.null(species)) {
    out$species_aceae <- grepl("aceae$", x[[species]])
  }

  if (!is.null(genus)) {
    out$genus_aceae <- grepl("aceae$", x[[genus]])
  }

  # Check for incongruous names in family column
  if (!is.null(family)) { 
    out$family_aceae <- !grepl("aceae$", x[[family]]) & !is.na(x[[family]])
  }

  # Check regex across all taxonomic names
  out$regex_all <- Reduce(`|`, lapply(x[c(taxon_name, family, genus, species)], function(y) { 
    grepl(regex, y) 
  }))

  # Filter to rows with issues
  existing_cols <- intersect(names(out), 
    c("species_aceae", "genus_aceae", "family_aceae", "regex_all"))

  out_fil <- out[rowSums(out[, existing_cols, drop = FALSE], na.rm = TRUE) > 0, ]

  # Generate comment
  if (!is.null(comment) & nrow(out_fil) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out_fil) > 0) { 
    message("Taxonomic name issues detected")
  }

  # Return
  return(out_fil)
}


