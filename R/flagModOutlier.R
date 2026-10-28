#' Flag outliers based on the residuals from a fitted model
#'
#' @param x `r param_x_stem()`
#' @param meas_id `r param_id("individual measurements")`
#' @param mod a fitted model object with `formula()`, `predict()` and 
#'     `sigma()` methods, e.g. `lm`, `glm`, `nls`
#' @param x_var optional, column name(s) in `x` with the predictor 
#'     variable(s) of the model, in the order they appear in the model 
#'     formula. If `NULL`, `x` must contain columns named as in the model
#' @param y_var optional, column name in `x` with the response variable of 
#'     the model. If `NULL`, `x` must contain a column named as in the model
#' @param threshold numeric multiplier for residual standard deviation to identify outliers
#' @param comment `r param_comment()`
#' 
#' @details
#' Residuals are calculated on the scale of the model response. For example, 
#' for a model of `log(H) ~ log(D)`, residuals are calculated as the 
#' difference between the observed and predicted `log(H)`, and compared to 
#' the residual standard deviation of the model on the same scale. Rows with
#' missing values in the predictors or the response are not flagged.
#' 
#' @return dataframe with values of `meas_id` identified as outliers from the 
#'     supplied model, with the predictor(s), response, and residual
#' 
#' @examples
#' set.seed(1)
#' df <- data.frame(id = 1:100, d = runif(100, 5, 80))
#' df$h <- exp(0.9 + 0.6 * log(df$d) + rnorm(100, 0, 0.1))
#' df$h[c(5, 50)] <- df$h[c(5, 50)] * 3
#' 
#' mod <- lm(log(H) ~ log(D), data = data.frame(H = df$h, D = df$d))
#' 
#' flagModOutlier(df, "id", mod, x_var = "d", y_var = "h", threshold = 3)
#' 
#' @export
#' 
flagModOutlier <- function(x, meas_id, mod, x_var = NULL, y_var = NULL, 
  threshold = 4, comment = NULL) {
  
  # Identify model terms
  f <- stats::formula(mod)
  model_y <- all.vars(f[[2]])
  model_x <- setdiff(all.vars(f[[3]]), c(model_y, names(stats::coef(mod))))

  if (length(model_y) != 1) {
    stop("The response of 'mod' must contain a single variable")
  }

  # Extract residual standard deviation from the model
  mod_sigma <- stats::sigma(mod)
  if (is.null(mod_sigma) || !is.finite(mod_sigma)) {
    stop("Unable to extract `sigma()` from 'mod'")
  }

  # Map user provided column names to model terms
  columnCatch(x, meas_id, x_var, y_var)
  x_tmp <- x
  if (!is.null(y_var)) {
    x_tmp[[model_y]] <- x[[y_var]]
  } else {
    y_var <- model_y
  }
  if (!is.null(x_var)) {
    if (length(x_var) != length(model_x)) {
      stop("'x_var' must have one column for each predictor in 'mod': ", 
        paste(model_x, collapse = ", "))
    }
    for (i in seq_along(x_var)) {
      x_tmp[[model_x[i]]] <- x[[x_var[i]]]
    }
  } else {
    x_var <- model_x
  }

  # Check input
  columnCatch(x_tmp, model_x, model_y)

  # Predict and calculate residuals on the scale of the model response
  preds <- stats::predict(mod, newdata = x_tmp)
  obs <- eval(f[[2]], x_tmp, environment(f))
  mod_resid <- as.vector(obs - preds)
  
  # Identify outliers
  is_outlier <- !is.na(mod_resid) & abs(mod_resid) >= mod_sigma * threshold

  out <- x[is_outlier, unique(c(meas_id, x_var, y_var)), drop = FALSE]
  out$mod_resid <- mod_resid[is_outlier]
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) { 
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message(sprintf("%d model outliers detected in %d rows (threshold: %s * %.3f sigma)",
      nrow(out), sum(!is.na(mod_resid)), threshold, mod_sigma))
  }

  # Return
  return(out)
}
