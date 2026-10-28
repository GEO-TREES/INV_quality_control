#' Flag stem measurements which will be excluded from AGB estimates
#'
#' @description
#' Reproduces the record selection of `recordFilter()` and `agbdFilter()` in 
#' the GEO-TREES INV_processing workflow, to show which stem measurements 
#' will not contribute to estimates of above-ground biomass (AGB) and AGB 
#' density (AGBD), and why. Check that each excluded stem is excluded for the
#' right reason, e.g. that live standing stems are not excluded because of a 
#' missing code.
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param plot `r param_x_plot()`
#' @param plot_id column name in `x` and `plot` with plot IDs
#' @param tree_id column name in `x` with tree IDs
#' @param stem_id column name in `x` with stem IDs
#' @param diam `r param_diam()`
#' @param code column name in `x` with stem condition codes
#' @param pom `r param_pom()`
#' @param date column name in `x` with measurement dates
#' @param plot_diam_min column name in `plot` with the minimum diameter 
#'     threshold of each plot
#' @param census_id `r param_census_id(optional = TRUE)`. The processing 
#'     workflow expects one census per plot. If `x` contains more than one 
#'     census, provide `census_id` so that measurements in different 
#'     censuses are not treated as repeat measurements of the same stem
#' @param growth_form optional, column name in `x` with growth form labels
#' @param missing_diam logical, if `TRUE` live stems without a diameter are 
#'     kept, and their diameter is imputed during AGB error propagation, as 
#'     with `missing_diam_method: "impute"` in `param.yaml`
#' @param include_broken logical, if `FALSE` stems broken above the POM 
#'     ("B") are excluded from AGB density, as with `include_broken`
#' @param agbd_diam_min_cm minimum diameter threshold for AGB density, as 
#'     with `agbd_diam_min_cm`. The larger of this and the threshold of the 
#'     plot is used
#' @param growth_form_exclude optional, values of `growth_form` excluded 
#'     from AGB density, as with `growth_form_exclude`
#' @param comment `r param_comment()`
#'
#' @details
#' Each record is given the first reason which applies, in the same order as
#' `./06_record_fil.R` in the processing workflow. Records excluded from AGB 
#' have no AGB estimate. Records which are "AGB only" have their own AGB 
#' estimate, but are not included in AGB density. Where a stem has more than 
#' one measurement, e.g. at more than one POM, only one is kept: the 
#' measurement with a diameter, then the highest POM, the latest date, and 
#' the largest diameter.
#'
#' @return dataframe with values of `meas_id` for records excluded from AGB 
#'     or AGB density, with columns `exclude` ("AGB" or "AGBD") and `reason`
#' 
#' @examples
#' stem <- data.frame(
#'   record_id = 1:7,
#'   plot_id = "A",
#'   tree_id = c(1, 2, 3, 4, 5, 6, 6),
#'   stem_id = c(1, 2, 3, 4, 5, 6, 6),
#'   diam_cm = c(20, 15, NA, 3, 30, 40, 41),
#'   code = c("AS", "DS", "AS", "AS", "AF", "AS", "AS"),
#'   pom_m = 1.3,
#'   measurement_date = 2024.1
#' )
#' plot <- data.frame(plot_id = "A", meas_diam_min_cm = 5)
#' 
#' flagAgbExclude(stem, "record_id", plot, missing_diam = FALSE)
#' 
#' @export
#' 
flagAgbExclude <- function(x, meas_id, plot, plot_id = "plot_id", 
  tree_id = "tree_id", stem_id = "stem_id", diam = "diam_cm", code = "code", 
  pom = "pom_m", date = "measurement_date", 
  plot_diam_min = "meas_diam_min_cm", census_id = NULL, growth_form = NULL, 
  missing_diam = TRUE, include_broken = TRUE, agbd_diam_min_cm = 0, 
  growth_form_exclude = NULL, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, plot_id, tree_id, stem_id, diam, code, pom, date, 
    census_id, growth_form)
  columnCatch(plot, plot_id, plot_diam_min)

  d <- classCatch(x[[diam]], "numeric")
  cd <- ifelse(is.na(x[[code]]), "", as.character(x[[code]]))

  # Records used to estimate AGB, see `recordFilter()`
  keep <- (!is.na(d) | missing_diam) & grepl("A", cd) & grepl("S", cd) & 
    !grepl("T", cd) & !grepl("M", cd)

  # Keep one measurement per stem. Stems without a stem ID are treated as 
  # separate stems
  stem_key <- ifelse(is.na(x[[stem_id]]) | x[[stem_id]] == "", 
    paste0("..row", seq_len(nrow(x))), as.character(x[[stem_id]]))
  group_key <- do.call(paste, c(x[, c(plot_id, census_id, tree_id), drop = FALSE], 
    list(stem_key), sep = "::"))
  p <- classCatch(x[[pom]], "numeric")
  dec <- decYear(x[[date]])
  idx <- which(keep)
  ord <- idx[order(group_key[idx], is.na(d[idx]), -p[idx], -dec[idx], -d[idx])]
  first <- rep(FALSE, nrow(x))
  first[ord[!duplicated(group_key[ord])]] <- TRUE
  repeat_meas <- keep & !first
  keep <- keep & first

  # Records used to estimate AGB density, see `agbdFilter()`
  diam_min <- plot[[plot_diam_min]][match(x[[plot_id]], plot[[plot_id]])]
  diam_min <- pmax(diam_min, agbd_diam_min_cm)
  below_min <- !is.na(d) & !is.na(diam_min) & d < diam_min
  broken <- grepl("B", cd) & !include_broken
  gf_excl <- if (!is.null(growth_form) && length(growth_form_exclude) > 0) {
    x[[growth_form]] %in% growth_form_exclude
  } else {
    rep(FALSE, nrow(x))
  }

  # Assign reasons, in the order of ./06_record_fil.R
  # Later assignments take precedence
  reason <- rep(NA_character_, nrow(x))
  reason[keep & is.na(diam_min)] <- "AGB only: plot has no diameter threshold"
  reason[keep & gf_excl] <- "AGB only: excluded growth form"
  reason[keep & broken] <- "AGB only: broken above POM"
  reason[keep & below_min] <- "AGB only: below diameter threshold"
  reason[repeat_meas] <- "excluded: repeat measurement of stem"
  reason[!keep & is.na(d)] <- "excluded: no diameter"
  reason[!keep & grepl("T", cd)] <- "excluded: broken below POM"
  reason[!keep & !grepl("S", cd)] <- "excluded: not standing"
  reason[!keep & !grepl("A", cd)] <- "excluded: dead"
  reason[!keep & grepl("M", cd)] <- "excluded: missing"
  reason[!keep & cd == ""] <- "excluded: no code"

  out <- x[, unique(c(meas_id, plot_id, census_id, diam, code)), drop = FALSE]
  out$exclude <- ifelse(grepl("^excluded", reason), "AGB", "AGBD")
  out$reason <- reason
  out <- out[!is.na(out$reason), , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message(sum(out$exclude == "AGB"), " records excluded from AGB, ", 
      sum(out$exclude == "AGBD"), " further records excluded from AGB density")
  }

  # Return
  return(out)
}
