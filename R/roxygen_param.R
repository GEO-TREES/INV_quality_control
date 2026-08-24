# Dataframe inputs

param_x_stem <- function() {
  "dataframe containing individual stem measurements"
}

# Columns in 'x'

param_id <- function(type) {
  paste("column name(s) in 'x' providing a unique ID for", type)
}

param_target <- function() {
  "column name(s) in 'x' to check"
}

param_plot_id <- function() {
  "column name in 'x' providing unique plot IDs"
}

param_status <- function() {
  "column name in `x` with logical mortality status"
}

param_pom <- function(optional = FALSE) {
  if (optional) { 
    opt <- "optional, "
  } else {
    opt <- NULL
  }
  paste0(opt, "column name in `x` with diameter Point of Measurement (POM) values")
}

param_census_id <- function(optional = FALSE) {
  if (optional) { 
    opt <- "optional, "
  } else {
    opt <- NULL
  }
  paste0(opt, "column name in 'x' providing unique census IDs. Must be sortable alpha-numerically")
}

param_census_id_vec <- function(optional = TRUE) {
  if (optional) { 
    opt1 <- "optional, "
    opt2 <- ". If 'NULL', guessed from 'x'"
  } else {
    opt1 <- NULL
    opt2 <- NULL
  }
  paste0(opt1, "vector of all possible census IDs", opt2)
}

param_census_date <- function() {
  "column name in 'x' containing census dates. Must be coercible to 'Date'"
}

param_census_date_vec <- function(optional = TRUE) {
  if (optional) { 
    opt1 <- "optional, "
    opt2 <- ". If 'NULL', guessed from 'x'. "
  } else {
    opt1 <- NULL
    opt2 <- NULL
  }
  paste0(opt1, "vector of all possible census dates", opt2, "Must be coercible to 'Date'")
}

param_diam <- function(optional = FALSE) {
  if (optional) { 
    opt <- "optional, "
  } else {
    opt <- NULL
  }
  paste0(opt, "column name in 'x' of diameter measurements.")
}

param_taxon_name <- function() {
  "column name containing taxonomic names in 'x'"
}

# Other arguments

param_comment <- function(optional = TRUE) {
  if (optional) { 
    opt <- "optional, "
  } else {
    opt <- NULL
  }
  paste0(opt, "character string to add as a comment")
}

param_cond <- function(optional = TRUE) {
  if (optional) { 
    opt <- "optional, "
  } else {
    opt <- NULL
  }
  paste0(opt, "logical expression defining a condition for each row in 'x'. Rows where 'cond' is FALSE will not be checked")
}

param_min_diam_threshold <- function() {
  "minimum diameter threshold for stem measurements in the plot(s)"
}
