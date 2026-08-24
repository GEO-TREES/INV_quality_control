#' Flag outliers based on the residuals from a fitted model
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param mod a fitted model object (e.g., `lm`, `glm`, `nls`)
#' @param x_var optional, column name in `x` with predictor variable from model
#' @param y_var optional, column name in `x` with response variable from model
#' @param threshold numeric multiplier for residual standard deviation to identify outliers
#' @param comment `r param_comment()`
#' 
#' @return dataframe with values of `meas_id` identified as outliers from the supplied model
#' 
#' @export
#' 
flagModOutlier <- function(x, meas_id, mod, x_var = NULL, y_var = NULL, 
  threshold = 4, comment = NULL) {
  
  # Identify model terms
  model_y <- as.character(formula(mod)[[2]])
  model_x <- setdiff(all.vars(formula(mod)), c(model_y, names(coef(mod))))

  # Create copy of x for prediction 
  x_tmp <- x
  
  # Map columns if user provided names
  if (!is.null(y_var)) {
    names(x_tmp)[names(x_tmp) == y_var] <- model_y
  }
  if (!is.null(x_var)) {
    names(x_tmp)[names(x_tmp) == x_var] <- model_x
  }

  # Check input
  columnCatch(x_tmp, meas_id, model_x, model_y)

  # Predict and calculate residuals using mapped data
  preds <- predict(mod, newdata = x_tmp)
  x_tmp$mod_resid <- x_tmp[[model_y]] - preds
  
  # Return indices
  out <- x_tmp[
    unname(
      abs(x_tmp$mod_resid) >= (stats::sigma(mod) * threshold) & 
        !is.na(x_tmp$mod_resid)), 
    c(meas_id, model_x, model_y, "mod_resid")]

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) { 
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Model outliers detected")
  }

  # Return
  return(out)
}

