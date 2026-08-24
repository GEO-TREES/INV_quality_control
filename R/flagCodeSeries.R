#' Flag incongruous stem code timelines
#' 
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param census_id `r param_census_id()`
#' @param target `r param_target()`
#' @param invalid_trans character vector or list of vectors defining invalid transitions of substrings in `target`
#' @param comment `r param_comment()`
#' 
#' @export
#' 
flagCodeSeries <- function(x, ind_id, census_id, target, invalid_trans, comment = NULL) {
  
  # Check arguments
  columnCatch(x, ind_id, census_id, target)
  
  # Add temporary row index
  x$..row_id.. <- seq_len(nrow(x))
  
  # Helper function: check if character exists anywhere in the string
  has_code <- function(code_string, val) {
    if (is.na(code_string)) {
      return(FALSE)
    }
    grepl(val, code_string, fixed = TRUE)
  }
  
  # Split by individual
  x <- x[order(x[[census_id]]), , drop = FALSE]
  x_split <- split(x, as.list(x[, ind_id, drop = FALSE]), drop = TRUE)
  
  # For each individual
  res_list <- lapply(x_split, function(y) {
    
    # Order by census date
    codes <- y[[target]]
    n <- length(codes)
    
    if (n < 2) {
      return(invisible(NULL))
    }
    
    # Loop over adjacent time steps
    for (i in seq_len(n - 1)) {
      c_curr <- codes[i]
      c_next <- codes[i + 1]
      
      # Check against all rules in the list
      for (rule in invalid_trans) {
        val1 <- rule[1]
        val2 <- rule[2]
        
        if (is.na(val1) && is.na(val2)) {
          stop("Invalid rule: Both elements in a transition rule cannot be NA.")
        }
        
        # --- Evaluate Condition 1 (Time T) ---
        if (is.na(val1)) {
          cond1 <- !has_code(c_curr, val2)
        } else {
          cond1 <- has_code(c_curr, val1)
        }
        
        # --- Evaluate Condition 2 (Time T+1) ---
        if (is.na(val2)) {
          cond2 <- !has_code(c_next, val1)
        } else {
          cond2 <- has_code(c_next, val2)
        }
        
        # If both conditions are met, flag illegal transition
        if (cond1 && cond2) {
          y_un <- y[1,ind_id, drop = FALSE]
          y_un$code <- paste(y[[target]], collapse = ";")
          return(y_un)
        }
      }
    }
  })

  out <- do.call(rbind, res_list)
  rownames(out) <- NULL
  
  # Generate comment
  if (!is.null(comment) & nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Invalid code transitions detected")
  }
  
  # Return
  return(out)
} 
