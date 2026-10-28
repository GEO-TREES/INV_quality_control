#' Flag Point of Measurement (POM) values inconsistent with the plot default
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param plot_id column name(s) in 'x' identifying plots, matching 
#'     `plot_plot_id` in `plot`
#' @param pom `r param_pom()`
#' @param code column name in `x` with stem condition codes
#' @param plot `r param_x_plot()`
#' @param plot_pom_default column name in `plot` with the default POM of 
#'     each plot
#' @param plot_plot_id column name(s) in `plot` identifying plots. Defaults 
#'     to `plot_id`
#' @param code_raised character vector of codes in `code` which justify a 
#'     POM which differs from the default, e.g. "U" (buttress) and "I" 
#'     (irregular stem)
#' @param tol tolerance (m) for comparing `pom` to the default POM
#' @param pom_max maximum plausible POM (m). Larger values may be in the 
#'     wrong units
#' @param comment `r param_comment()`
#'
#' @details
#' Flags four kinds of issue:
#' \itemize{
#'   \item POM above the default, without a code in `code_raised`.
#'   \item POM below the default.
#'   \item A code in `code_raised`, but POM at the default.
#'   \item POM greater than `pom_max`, e.g. recorded in cm.
#' }
#' In the GEO-TREES INV_processing workflow, diameters measured above 1.3 m 
#' can optionally be corrected to their equivalent at 1.3 m with a taper 
#' model, so POM values directly affect AGB estimates.
#'
#' @return dataframe with values of `meas_id` with suspect POM values, with 
#'     the POM, default POM, code, and a description of the issue
#' 
#' @examples
#' stem <- data.frame(
#'   id = 1:5,
#'   plot_id = "A",
#'   pom = c(1.3, 2.5, 2.5, 1.3, 130),
#'   code = c("AS", "ASU", "AS", "ASU", "AS")
#' )
#' plot <- data.frame(plot_id = "A", pom_default = 1.3)
#' 
#' flagPomDefault(stem, "id", "plot_id", "pom", "code", plot, "pom_default")
#' 
#' @export
#' 
flagPomDefault <- function(x, meas_id, plot_id, pom, code, plot, 
  plot_pom_default, plot_plot_id = plot_id, code_raised = c("U", "I"), 
  tol = 0.05, pom_max = 10, comment = NULL) {

  # Check input
  columnCatch(x, meas_id, plot_id, pom, code)
  columnCatch(plot, plot_plot_id, plot_pom_default)

  x[[pom]] <- classCatch(x[[pom]], "numeric")
  plot[[plot_pom_default]] <- classCatch(plot[[plot_pom_default]], "numeric")

  # Match default POM to stems
  x_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))
  plot_key <- do.call(paste, c(plot[, plot_plot_id, drop = FALSE], sep = "::"))
  pom_default <- plot[[plot_pom_default]][match(x_key, plot_key)]

  p <- x[[pom]]
  has_raised <- grepl(paste0("[", paste(code_raised, collapse = ""), "]"), x[[code]])

  # Identify issues
  issue <- rep(NA_character_, nrow(x))
  issue[!has_raised & p > pom_default + tol] <- "POM above default without raised POM code"
  issue[p < pom_default - tol] <- "POM below default"
  issue[has_raised & abs(p - pom_default) <= tol] <- "Raised POM code but POM at default"
  issue[p > pom_max] <- "POM implausibly large, check units"

  out <- x[, unique(c(meas_id, plot_id, pom, code)), drop = FALSE]
  out$pom_default <- pom_default
  out$issue <- issue
  out <- out[!is.na(out$issue), , drop = FALSE]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("POM values inconsistent with plot default detected")
  }

  # Return
  return(out)
}
