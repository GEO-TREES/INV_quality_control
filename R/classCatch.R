#' Check class and safely coerce if possible
#'
#' @param x vector to check
#' @param target_class string representing the desired class (e.g. "numeric"). 
#'     Should have an associated `as.*` function
#' 
#' @return original object if class matches, or a safely coerced version.
#' 
#' @keywords Internal
#' @noRd
#' 
classCatch <- function(x, target_class) {
  # Return immediately if already the correct class
  if (inherits(x, target_class)) {
    return(x)
  }
  
  # Identify coercion function
  coerce_fn_name <- paste0("as.", target_class)
  
  if (!exists(coerce_fn_name, mode = "function")) {
    stop(sprintf("No coercion function '%s' for class '%s'.", 
      coerce_fn_name, target_class), call. = FALSE)
  }
  
  coerce_fn <- get(coerce_fn_name)
  
  # Attempt coercion with safety checks 
  tryCatch({
    res <- coerce_fn(x)
    
    # Check if coercion introduced NAs 
    new_na <- !is.na(x) & is.na(res)
    if (any(new_na)) {
      stop(call. = FALSE) 
    }
    
    return(res)
    
  }, warning = function(w) {
    stop(sprintf("Unable to coerce values to %s, NAs introduced.", target_class), 
      call. = FALSE)
         
  }, error = function(e) {
    stop(sprintf("Unable to coerce values to %s, NAs introduced.", target_class), 
      call. = FALSE)
  })
}
