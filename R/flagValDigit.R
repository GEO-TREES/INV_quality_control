#' Flag groups with terminal digit preference in measurement values
#'
#' @param x `r param_x_stem()`
#' @param group_id column name(s) in 'x' defining groups to test, e.g. 
#'     `c("plot_id", "census_id")`
#' @param target `r param_target()`
#' @param digits number of decimal places of the terminal digit, e.g. 1 for 
#'     diameters recorded to the nearest mm in cm
#' @param alpha significance level of the chi-squared test
#' @param max_prop maximum proportion of any single terminal digit. Groups 
#'     are only flagged if the most common digit exceeds this proportion, 
#'     as well as the test being significant. With a uniform distribution 
#'     each digit has a proportion of 0.1. In large datasets, trivially small
#'     departures from uniformity are statistically significant
#' @param min_n minimum number of non-missing values in a group to perform 
#'     the test
#' @param comment `r param_comment()`
#'
#' @details
#' If measurements are made with equal care, the terminal digit of 
#' continuous measurements such as diameter should be approximately uniformly
#' distributed. An excess of particular digits, typically 0 and 5, suggests 
#' that values were estimated or rounded, e.g. by a particular field team. 
#' For each group, a chi-squared goodness-of-fit test compares the frequency 
#' of each terminal digit (0-9) to a uniform distribution. This test does 
#' not require an assumption about the expected proportion of rounded values.
#'
#' @return dataframe with values of `group_id` with significant terminal 
#'     digit preference, with the number of values, the proportion of 
#'     terminal digits which are 0 and 5, the most common digit and its 
#'     proportion, and the p-value
#' 
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   plot_id = rep(c("A", "B"), each = 500),
#'   dbh = c(round(rlnorm(500, 3, 0.5), 1), 
#'     round(rlnorm(500, 3, 0.5) * 2) / 2)
#' )
#' 
#' flagValDigit(df, "plot_id", "dbh")
#' 
#' @export
#' 
flagValDigit <- function(x, group_id, target, digits = 1, alpha = 0.001, 
  max_prop = 0.15, min_n = 100, comment = NULL) {

  # Check input
  columnCatch(x, group_id, target)

  x[[target]] <- classCatch(x[[target]], "numeric")

  # Extract terminal digit
  v <- x[[target]] * 10^digits
  x$..digit.. <- round(v) %% 10
  x$..digit..[!is.finite(v)] <- NA

  # Split by group
  x_split <- split(x, as.list(x[, group_id, drop = FALSE]), drop = TRUE)

  out <- do.call(rbind, lapply(x_split, function(y) {
    d <- y$..digit..[!is.na(y$..digit..)]
    if (length(d) < min_n) {
      return(NULL)
    }
    counts <- tabulate(d + 1, nbins = 10)
    res <- unique(y[, group_id, drop = FALSE])
    res$n <- length(d)
    res$prop_0 <- counts[1] / length(d)
    res$prop_5 <- counts[6] / length(d)
    res$digit_mode <- which.max(counts) - 1
    res$prop_mode <- max(counts) / length(d)
    res$p_value <- suppressWarnings(stats::chisq.test(counts)$p.value)
    res
  }))

  if (is.null(out)) {
    message("No groups with at least 'min_n' values")
    out <- x[0, group_id, drop = FALSE]
  } else {
    out <- out[out$p_value < alpha & out$prop_mode > max_prop, , drop = FALSE]
  }
  rownames(out) <- NULL

  # Generate comment
  if (!is.null(comment) && nrow(out) > 0) {
    out$comment <- comment
  }

  # Generate message
  if (nrow(out) > 0) {
    message("Terminal digit preference detected")
  }

  # Return
  return(out)
}
