#' Check relationship of two variables and find outliers with linear model 
#' 
#' @param model a fitted model object (e.g., lm, glm, nls)
#' @param newdata a data frame containing all predictors and the observed variable
#' @param obs_var the name of the column in newdata containing the observed values
#' @param threshold the multiplier for the model's residual standard deviation
#'
#' @return an integer vector of row indices in newdata that are outliers
#' 
#' @export
#' 
flagRelOutlier <- function(model, newdata, obs_var, threshold = 4) {
  
  # basic input validation
  if (!is.data.frame(newdata)) {
    stop("'newdata' must be a data frame or dataframe extension")
  }
  
  if (!(obs_var %in% names(newdata))) {
    stop(sprintf("observed variable '%s' not found in newdata", obs_var))
  }

  # identify required predictors from the model
  # attr(terms(model), "term.labels") extracts the x-variables
  required_vars <- attr(stats::terms(model), "term.labels")
  missing_vars <- setdiff(required_vars, names(newdata))
  
  if (length(missing_vars) > 0) {
    stop(paste("newdata is missing required predictors:", 
        paste(missing_vars, collapse = ", ")))
  }

  # Generate predictions 
  preds <- predict(model, newdata = newdata, na.action = na.exclude)
  
  # Calculate prediction error
  obs_values <- newdata[[obs_var]]
  errors <- obs_values - preds
  
  # Extract sigma (residual standard error) from the original model
  mod_sigma <- sigma(model)
  
  if (is.null(mod_sigma)) {
    stop("Unable to extract `sigma()` from model object.")
  }

  # Identify indices where the error is extreme
  # true if: |error| >= threshold * sigma
  outlier_vec <- abs(errors) >= (mod_sigma * threshold)

  # Standardise NA to FALSE
  out <- ifelse(is.na(outlier_vec), FALSE, outlier_vec)
  
  # Message
  n_outliers <- sum(out)
  if (n_outliers > 0) {
    message(sprintf(
      "Found %d outlier(s) in %d rows (Threshold: %s * %.2f sigma)",
      n_outliers, nrow(newdata), threshold, mod_sigma
    ))
  }

  # Return
  return(out)
}
