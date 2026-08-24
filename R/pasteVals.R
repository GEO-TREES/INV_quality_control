#' Paste values together, replace NAs with blank, optional separator
#'
#' @param ... vectors or dataframe to be pasted together
#' @param sep separator between adjacent values in vectors
#' @param collapse separator between sets of values across vectors
#' @param unique logical, if TRUE duplicated values are removed
#' @param sort logical, if TRUE values are sorted
#'
#' @keywords Internal
#' @noRd
#' 
#' @export
#' 
pasteVals <- function(..., sep = "", collapse = NULL, 
  remna = TRUE, unique = FALSE, sort = FALSE) {
  ret <-
    apply(
      X = cbind(...),
      MARGIN = 1,
      FUN = function(x) {
        if (all(is.na(x))) {
          NA_character_
        } else {
          if (remna) {
            x <- x[!is.na(x)]
          }
          if (unique) {
            x <- x[!duplicated(x)]
          }
          if (sort) {
            x <- sort(x, na.last = TRUE)
          }
          paste(x, collapse = sep)
        }
      }
    )
  if (!is.null(collapse)) {
    paste(ret, collapse = collapse)
  } else {
    ret
  }
}

