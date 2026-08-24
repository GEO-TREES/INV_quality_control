#' Flag row indices with invalid or inconsistent status codes
#'
#' @param x `r param_x_stem()` 
#' @param meas_id `r param_id("individual measurements")`
#' @param target `r param_target()`
#' @param accepted optional, character vector of all valid individual codes 
#' @param invalid_comb character vector or list of vectors defining invalid combinations of substrings in `target`
#' @param cond `r param_cond()`
#' @param comment `r param_comment()`
#'
#' @details
#' This function flags rows where the `target` contains substrings which are invalid when used in combination. 

#' @return dataframe with values of `meas_id` containing invalid or inconsistent codes in `target`
#'
#' @examples
#' df <- data.frame(
#'   code = c("S", "F", "SF", "R"),
#'   dbh = c(10, 12, 11, 15)
#' )
#' 
#' flagCodeComb(df, 
#'   target = "code", 
#'   accepted = c("S", "F"),
#'   invalid_comb = list(
#'     c("S", "F")
#'   )
#' )
#' 
#' @export
flagCodeVal <- function(x, meas_id, target, accepted = NULL, invalid_comb = NULL, cond = NULL, comment = NULL) {
  
  # Check arguments
  columnCatch(x, meas_id, target)

  x[[target]] <- classCatch(x[[target]], "character")

  # Make invalid combinations a list if not already 
  if (!is.list(invalid_comb) && is.atomic(invalid_comb)) {
    invalid_comb <- list(invalid_comb)
  }
  
  # Check cond is same length as rows in x
  if (!is.null(cond)) { 
    if (nrow(x) != length(cond)) {
      stop("'cond' must be the same length as the number of rows in 'x'")
    }
  }

  # Evaluate condition within the data frame environment
  if (is.null(cond)) {
    in_scope <- rep(TRUE, nrow(x))
  } else {
    in_scope <- cond
    # Ensure NAs in condition result in FALSE
    in_scope[is.na(in_scope)] <- FALSE
  }

  # Check for invalid values
  if (!is.null(accepted)) { 
    pattern <- paste0("[^", paste(gsub("([\\^\\-\\]])", "\\\\\\1", accepted), collapse = ""), "]")
    not_accepted <- grepl(pattern, x[[target]]) & in_scope
  } else {
    not_accepted <- NULL
  }

  # Check for invalid combinations
  if (!is.null(invalid_comb)) { 
    not_valid_comb <- colSums(do.call(rbind, lapply(invalid_comb, function(y) { 
      res <- Reduce(`&`, lapply(y, grepl, x = x[[target]]))
    }))) > 0
  } else {
    not_valid_comb <- NULL
  }

  # Create output
  out <- x[,c(meas_id, target)]
  out$not_accepted <- not_accepted
  out$not_valid_comb <- not_valid_comb 

  # Filter to invalid rows
  if (is.null(not_accepted)) {
    not_accepted <- rep(FALSE, nrow(x))
  }

  if (is.null(not_valid_comb)) {
    not_valid_comb <- rep(FALSE, nrow(x))
  }

  out_fil <- out[not_accepted + not_valid_comb > 0,]

  # Generate comment
  if (!is.null(comment) & nrow(out_fil) > 0) { 
    out_fil$comment <- comment
  }

  # Generate message
  if (nrow(out_fil) > 0) {
    message("Invalid code values or combinations detected")
  }

  # Return
  return(out_fil)
} 
