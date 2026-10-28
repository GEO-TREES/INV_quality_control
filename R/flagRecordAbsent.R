#' Flag stems alive in a previous census which are absent from the focal census
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param plot_id column name(s) in `x` identifying plots
#' @param census_id `r param_census_id()`
#' @param status `r param_status()`
#' @param focal logical vector of `nrow(x)`, `TRUE` for measurements in the 
#'     focal census of their plot
#' @param comment `r param_comment()`
#'
#' @details
#' Stems which were alive when they were last measured before the focal 
#' census should be recorded in the focal census, either with a new 
#' measurement, as dead, or as missing ("M"). Stems with no record at all in 
#' the focal census may have been missed in the field or lost during data 
#' entry, and their AGB is missing from AGB density estimates. Unlike 
#' `flagRecordMissing()`, this function also finds stems absent from the 
#' focal census when it is the latest census.
#'
#' @return dataframe with values of `ind_id` for stems absent from the focal 
#'     census, with the census ID and status of their last measurement, and
#'     the focal census ID of their plot
#' 
#' @examples
#' df <- data.frame(
#'   plot_id = "A",
#'   stem_id = c(1, 1, 2, 2, 3, 4),
#'   census_id = c(1, 2, 1, 2, 1, 1),
#'   alive = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
#' )
#'
#' # Latest census of each plot
#' df$focal <- df$census_id == ave(df$census_id, df$plot_id, FUN = max)
#' 
#' flagRecordAbsent(df, c("plot_id", "stem_id"), "plot_id", "census_id", 
#'   "alive", df$focal)
#' 
#' @export
#' 
flagRecordAbsent <- function(x, ind_id, plot_id, census_id, status, focal, 
  comment = NULL) {

  # Check input
  columnCatch(x, ind_id, plot_id, census_id, status)

  if (length(focal) != nrow(x)) {
    stop("'focal' must be the same length as the number of rows in 'x'")
  }
  focal[is.na(focal)] <- FALSE

  x[[status]] <- classCatch(x[[status]], "logical")

  # Order of each census, and of the focal census of each plot
  ord <- censusOrder(x[[census_id]])
  plot_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))
  stem_key <- do.call(paste, c(x[, ind_id, drop = FALSE], sep = "::"))
  focal_ord <- tapply(ord[focal], plot_key[focal], max)
  focal_cens <- tapply(as.character(x[[census_id]])[focal], plot_key[focal], 
    function(i) { i[1] })

  # Last measurement of each stem before the focal census
  before <- !is.na(ord) & ord < focal_ord[plot_key]
  before[is.na(before)] <- FALSE
  idx <- which(before)
  idx <- idx[order(stem_key[idx], -ord[idx])]
  last <- idx[!duplicated(stem_key[idx])]

  # Alive at last measurement, but no record in the focal census
  absent <- last[!stem_key[last] %in% stem_key[focal] & x[[status]][last] %in% TRUE]

  out <- x[absent, unique(c(ind_id, census_id, status)), drop = FALSE]
  out$census_focal <- unname(focal_cens[plot_key[absent]])
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Stems absent from the focal census detected")
  }

  # Return
  return(out)
}
