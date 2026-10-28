#' Convert dates to decimal years
#'
#' @param x vector of dates. Either character dates formatted as YYYY-MM-DD, 
#'     YYYY-MM, or YYYY, objects of class `Date`, or numeric decimal years
#'
#' @details
#' Partial dates are placed at the midpoint of their month or year, matching 
#' `decDate()` in the GEO-TREES INV_processing workflow. Numeric values are 
#' assumed to already be decimal years and are returned unchanged. Values 
#' which cannot be parsed are returned as `NA`.
#'
#' @return numeric vector of decimal years
#' 
#' @examples
#' decYear(c("2024-03-15", "2024-03", "2024", "15/03/2024"))
#' decYear(as.Date("2024-07-02"))
#' 
#' @export
#' 
decYear <- function(x) {
  
  # Return immediately if already decimal years
  if (is.numeric(x)) {
    return(x)
  }

  # Format dates as YYYY-MM-DD
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- format(as.Date(x), "%Y-%m-%d")
  }

  x <- trimws(as.character(x))

  # Identify date resolution
  res <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), "YYYY-MM-DD",
    ifelse(grepl("^\\d{4}-\\d{2}$", x), "YYYY-MM",
      ifelse(grepl("^\\d{4}$", x), "YYYY", NA_character_)))

  y <- suppressWarnings(as.integer(substr(x, 1, 4)))
  m <- suppressWarnings(as.integer(substr(x, 6, 7)))

  # Start and (exclusive) end of the interval covered by each date
  start <- suppressWarnings(as.Date(ifelse(res == "YYYY-MM-DD", x,
    ifelse(res == "YYYY-MM", paste0(x, "-01"),
      ifelse(res == "YYYY", paste0(x, "-01-01"), NA_character_))), 
    optional = TRUE))
  end <- suppressWarnings(as.Date(ifelse(res == "YYYY-MM-DD", x,
    ifelse(res == "YYYY-MM", sprintf("%04d-%02d-01", y + m %/% 12, m %% 12 + 1),
      ifelse(res == "YYYY", sprintf("%04d-01-01", y + 1), NA_character_))), 
    optional = TRUE))

  # Midpoint of interval, as fraction of year elapsed
  mid <- start + (end - start) / 2
  y_mid <- as.integer(format(mid, "%Y"))
  y_start <- as.Date(ifelse(is.na(y_mid), NA_character_, paste0(y_mid, "-01-01")))
  y_len <- as.numeric(as.Date(ifelse(is.na(y_mid), NA_character_, 
    paste0(y_mid + 1, "-01-01"))) - y_start)

  # Return
  return(y_mid + as.numeric(mid - y_start) / y_len)
}
