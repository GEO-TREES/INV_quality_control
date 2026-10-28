#' Check a table against a GEO-TREES column specification
#'
#' @description
#' Compares the columns of a table against the GEO-TREES L0 column 
#' specification. By default, specifications are read directly from the templates in the 
#' INV_processing repository, so that they are always up to date.
#'
#' @param x dataframe to check, e.g. the stem measurement table
#' @param spec either a dataframe with columns `column_name`, `class`, and 
#'     optionally `min_val` and `max_val`, the path or URL of a CSV file 
#'     containing such a dataframe, or the name of an INV_processing 
#'     template in `spec_dir`: "stem", "plot", "pt", "taxon", "height", "wd", 
#'     "subplot", or "stem_dup"
#' @param spec_dir directory or URL containing the INV_processing templates,
#'     used if `spec` is a template name. Defaults to the templates in the 
#'     INV_processing GitHub repository. Use the path to the `templates/` 
#'     directory of a local copy of INV_processing to work offline
#' @param allow_extra logical, if `TRUE` columns in `x` which are not in 
#'     `spec` are reported as warnings rather than errors. Useful where extra 
#'     columns are needed during QC, e.g. `census_id` for multi-census checks
#' @param check_order logical, if `TRUE` check that columns are in the same 
#'     order as `spec`
#' @param warn_cols columns for which values outside the range in `spec` are
#'     reported as warnings rather than errors. By default stem coordinates,
#'     as stems may be mapped slightly outside the plot
#'
#' @details
#' Columns with the wrong class are reported as errors if they cannot be 
#' coerced to the correct class without introducing `NA` values, and as 
#' warnings otherwise, e.g. an integer column which should be numeric, or a 
#' character column where all values are `NA` and the column has been read 
#' as logical. 
#'
#' @return dataframe with one row per issue, with columns `column`, 
#'     `severity` ("error" or "warning"), and `issue`
#' 
#' @examples
#' spec <- data.frame(
#'   column_name = c("plot_id", "diam_cm", "pom_m"),
#'   class = c("character", "numeric", "numeric"),
#'   min_val = c(NA, 0, 0),
#'   max_val = c(NA, 1000, 50))
#' 
#' df <- data.frame(
#'   plot_id = c("A", "B"),
#'   diam_cm = c(10L, -5L),
#'   notes = NA)
#' 
#' checkTableSpec(df, spec)
#' 
#' @export
#' 
checkTableSpec <- function(x, spec, 
  spec_dir = "https://raw.githubusercontent.com/GEO-TREES/INV_processing/main/templates",
  allow_extra = FALSE, check_order = TRUE, warn_cols = c("x_rel_m", "y_rel_m")) {

  # Load specification
  if (is.character(spec) && length(spec) == 1) {
    spec_names <- c("stem", "plot", "pt", "taxon", "height", "wd", "subplot", 
      "stem_dup")
    if (spec %in% spec_names) {
      spec <- file.path(spec_dir, paste0(spec, "_cols.csv"))
    } else if (!file.exists(spec) && !grepl("^https?://", spec)) {
      stop("'spec' must be a dataframe, a path or URL of a CSV file, or one of: ",
        paste(spec_names, collapse = ", "))
    }
    spec <- tryCatch(utils::read.csv(spec, fileEncoding = "UTF-8-BOM"),
      error = function(e) {
        stop("Unable to read specification from: ", spec, call. = FALSE)
      })
  }

  columnCatch(spec, "column_name", "class")
  spec <- spec[!is.na(spec$column_name) & spec$column_name != "", ]

  issue <- function(column, severity, text) {
    data.frame(column = column, severity = severity, issue = text)
  }

  out <- list()

  # Missing columns
  cols_missing <- setdiff(spec$column_name, names(x))
  if (length(cols_missing) > 0) {
    out$missing <- issue(cols_missing, "error", "Missing column")
  }

  # Extra columns
  cols_extra <- setdiff(names(x), spec$column_name)
  if (length(cols_extra) > 0) {
    out$extra <- issue(cols_extra, ifelse(allow_extra, "warning", "error"), 
      "Column not in specification, must be removed before processing")
  }

  # Column classes
  cols_shared <- spec[spec$column_name %in% names(x), ]
  out$class <- do.call(rbind, lapply(seq_len(nrow(cols_shared)), function(i) {
    col <- cols_shared$column_name[i]
    cls <- cols_shared$class[i]
    if (any(class(x[[col]]) == cls)) {
      return(NULL)
    }
    coercible <- tryCatch({
      classCatch(x[[col]], cls)
      TRUE
    }, error = function(e) { 
      FALSE 
    })
    issue(col, ifelse(coercible, "warning", "error"), 
      sprintf("Class should be '%s' not '%s'%s", cls, 
        paste(class(x[[col]]), collapse = "/"), 
        ifelse(coercible, ", but can be coerced", "")))
  }))

  # Column order
  if (check_order && length(cols_missing) == 0) {
    cols_order <- names(x)[names(x) %in% spec$column_name]
    if (!identical(cols_order, spec$column_name)) {
      out$order <- issue(NA_character_, "error", 
        paste("Column order should be:", paste(spec$column_name, collapse = ", ")))
    }
  }

  # Values outside range
  if (all(c("min_val", "max_val") %in% names(spec))) {
    out$range <- do.call(rbind, lapply(seq_len(nrow(cols_shared)), function(i) {
      col <- cols_shared$column_name[i]
      v <- suppressWarnings(as.numeric(x[[col]]))
      lo <- suppressWarnings(as.numeric(cols_shared$min_val[i]))
      hi <- suppressWarnings(as.numeric(cols_shared$max_val[i]))
      n_lo <- if (is.na(lo)) 0 else sum(v < lo, na.rm = TRUE)
      n_hi <- if (is.na(hi)) 0 else sum(v > hi, na.rm = TRUE)
      sev <- ifelse(col %in% warn_cols, "warning", "error")
      rbind(
        if (n_lo > 0) issue(col, sev, sprintf("%d values below %s", n_lo, lo)),
        if (n_hi > 0) issue(col, sev, sprintf("%d values above %s", n_hi, hi)))
    }))
  }

  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- issue(character(0), character(0), character(0))
  }
  rownames(out) <- NULL

  # Generate message
  if (any(out$severity == "error")) {
    message("Table does not match specification")
  }

  # Return
  return(out)
}
