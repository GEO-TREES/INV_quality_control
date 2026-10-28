#' Flag taxa from alternative growth forms which are not labelled as such
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param taxon_name `r param_taxon_name()`
#' @param family optional, column name in `x` with taxonomic family names. 
#'     Recommended, as most alternative growth forms are identified by family
#' @param growth_form optional, column name in `x` with growth form labels. 
#'     If provided, only stems where `growth_form` is empty or "tree" are 
#'     flagged
#' @param lookup optional, dataframe with columns `taxon` (family or genus 
#'     name) and `growth_form`, to replace the default lookup table
#' @param comment `r param_comment()`
#'
#' @details
#' Many tree inventories include palms, tree ferns, bamboos, bananas, cycads 
#' and other plants which are not dicot or conifer trees. Standard tree 
#' allometries, e.g. Chave et al. (2014), are inappropriate for these plants.
#' In the GEO-TREES INV_processing workflow, stems can be assigned an 
#' alternative allometry in `agb_allometry`, e.g. "goodman2013" for palms, 
#' or excluded by `growth_form`. The default lookup table contains families 
#' and genera of common alternative growth forms. Hemi-epiphytic figs 
#' (*Ficus*) are flagged as possible stranglers, as they are often 
#' excluded from AGB density. Lianas cannot be reliably identified from 
#' family or genus, and are not included in the lookup table.
#'
#' @return dataframe with values of `meas_id` for stems likely to be an 
#'     alternative growth form, with the taxonomic name and the likely 
#'     growth form
#' 
#' @examples
#' df <- data.frame(
#'   id = 1:4,
#'   sp = c("Burkea africana", "Hyphaene petersiana", "Cyathea sp.", 
#'     "Ficus sycomorus"),
#'   fam = c("Fabaceae", "Arecaceae", "Cyatheaceae", "Moraceae")
#' )
#' 
#' flagFormAlt(df, "id", "sp", family = "fam")
#' 
#' @export
#' 
flagFormAlt <- function(x, meas_id, taxon_name, family = NULL, 
  growth_form = NULL, lookup = NULL, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, taxon_name, family, growth_form)

  # Default lookup table
  if (is.null(lookup)) {
    lookup <- rbind(
      data.frame(growth_form = "palm", taxon = c("Arecaceae", "Palmae", 
        "Attalea", "Bactris", "Borassus", "Cocos", "Elaeis", "Euterpe", 
        "Geonoma", "Hyphaene", "Iriartea", "Licuala", "Mauritia", 
        "Oenocarpus", "Phoenix", "Raphia", "Socratea")),
      data.frame(growth_form = "rattan", taxon = c("Calamus", "Daemonorops", 
        "Korthalsia", "Plectocomia")),
      data.frame(growth_form = "tree fern", taxon = c("Cyatheaceae", 
        "Dicksoniaceae", "Alsophila", "Cyathea", "Dicksonia", "Sphaeropteris")),
      data.frame(growth_form = "bamboo", taxon = c("Poaceae", "Gramineae", 
        "Bambusa", "Chusquea", "Dendrocalamus", "Gigantochloa", "Guadua", 
        "Oxytenanthera")),
      data.frame(growth_form = "banana", taxon = c("Musaceae", 
        "Strelitziaceae", "Ensete", "Musa", "Ravenala")),
      data.frame(growth_form = "cycad", taxon = c("Cycadaceae", "Zamiaceae")),
      data.frame(growth_form = "cactus", taxon = "Cactaceae"),
      data.frame(growth_form = "pandan", taxon = c("Pandanaceae", "Pandanus")),
      data.frame(growth_form = "grass tree", taxon = "Xanthorrhoea"),
      data.frame(growth_form = "possible strangler", taxon = "Ficus"))
  }
  columnCatch(lookup, "taxon", "growth_form")

  # Match genus, then family, to the lookup table
  genus <- sub(" .*", "", trimws(as.character(x[[taxon_name]])))
  form <- lookup$growth_form[match(genus, lookup$taxon)]
  if (!is.null(family)) {
    form_fam <- lookup$growth_form[match(trimws(x[[family]]), lookup$taxon)]
    form[is.na(form)] <- form_fam[is.na(form)]
  }

  # Only flag stems which are not already labelled
  if (!is.null(growth_form)) {
    gf <- trimws(tolower(as.character(x[[growth_form]])))
    unlabelled <- is.na(gf) | gf == "" | gf == "tree"
  } else {
    unlabelled <- rep(TRUE, nrow(x))
  }

  out <- x[, unique(c(meas_id, taxon_name, family, growth_form)), drop = FALSE]
  out$growth_form_likely <- form
  out <- out[!is.na(form) & unlabelled, , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Possible alternative growth forms detected")
  }

  # Return
  return(out)
}
