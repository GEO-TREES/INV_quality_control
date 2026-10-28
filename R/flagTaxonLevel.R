#' Flag stems not identified to species level
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param taxon_name `r param_taxon_name()`
#' @param comment `r param_comment()`
#'
#' @details
#' Classifies the taxonomic resolution of each name using simple rules:
#' \itemize{
#'   \item \strong{unidentified}: empty, or e.g. "Indet", "Unknown", "sp.".
#'   \item \strong{family}: a single word ending in "-aceae", or a conserved 
#'     family name, e.g. "Leguminosae".
#'   \item \strong{genus}: a single word, or a genus followed by e.g. "sp.", 
#'     "spp.", "sp2", or a morphospecies code.
#'   \item \strong{species_uncertain}: a genus and epithet qualified by 
#'     "cf." or "aff.".
#'   \item \strong{species}: anything else.
#' }
#' In the GEO-TREES INV_processing workflow, wood density is estimated at the
#' finest taxonomic level available, falling back to genus, family, and 
#' finally the plot mean. Stems which are not identified to species therefore
#' have more uncertain AGB estimates.
#'
#' @return dataframe with values of `meas_id` for stems not identified to 
#'     species, with the taxonomic name and its level
#' 
#' @examples
#' df <- data.frame(
#'   id = 1:7,
#'   sp = c("Burkea africana", "Brachystegia sp.", "Fabaceae", "Indet", NA,
#'     "Ochna cf. pulchra", "Combretum sp2")
#' )
#' 
#' flagTaxonLevel(df, "id", "sp")
#' 
#' @export
#' 
flagTaxonLevel <- function(x, meas_id, taxon_name, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, taxon_name)

  nm <- trimws(gsub("\\s+", " ", as.character(x[[taxon_name]])))
  words <- strsplit(ifelse(is.na(nm), "", nm), " ")
  w1 <- vapply(words, function(i) { if (length(i) > 0) i[1] else "" }, character(1))
  w2 <- vapply(words, function(i) { if (length(i) > 1) i[2] else "" }, character(1))
  n_words <- lengths(words)

  fam_alt <- c("Compositae", "Cruciferae", "Gramineae", "Guttiferae", 
    "Labiatae", "Leguminosae", "Palmae", "Umbelliferae")
  unid <- "^(indet|indeterminate|unknown|unidentified|unid|morpho|morphospecies|sp|spp|na|none|-+|\\?+)\\.?$"
  # Epithets which do not identify a species, and morphospecies codes, e.g. 
  # "sp2", "A", "1"
  genus_only <- "^(sp|spp|sp\\.?[0-9]+[a-z]?|indet|morpho|morphospecies)\\.?$"
  morpho_code <- "^([A-Z][A-Z0-9]*|[0-9].*)$"
  uncertain <- "^(cf|aff)\\.?$"

  # Classify taxonomic level, with the coarsest level last
  level <- rep("species", nrow(x))
  level[n_words >= 2 & grepl(uncertain, w2, ignore.case = TRUE)] <- "species_uncertain"
  level[n_words == 1 | (n_words >= 2 & 
    (grepl(genus_only, w2, ignore.case = TRUE) | grepl(morpho_code, w2)))] <- "genus"
  level[grepl("aceae$", w1) | w1 %in% fam_alt] <- "family"
  level[n_words == 0 | grepl(unid, w1, ignore.case = TRUE)] <- "unidentified"

  out <- x[, unique(c(meas_id, taxon_name)), drop = FALSE]
  out$taxon_level <- level
  out <- out[out$taxon_level != "species", , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Stems not identified to species detected")
  }

  # Return
  return(out)
}
