#' Display help file for function as HTML or Plain Text
#'
#' This function retrieves the Rd (R documentation) help file for a given 
#' subject and renders it either as styled HTML or as plain text. 
#'
#' @param x The function or topic to look up. Can be an unquoted name 
#'   (e.g., `mean`) or a character string (e.g., `"mean"`).
#' @param pkg optional, character string specifying the package to 
#'   which the function belongs. If `NULL` searches all loaded packages
#' @param html logical, if `TRUE`, returns an object formatted 
#'   for HTML output in Quarto/RMarkdown. If `FALSE` (default), prints plain text 
#'   to console
#'
#' @return If `html = TRUE`, returns a `knit_asis` object containing 
#'   preserved HTML. If `html = FALSE`, it returns `NULL` invisibly and 
#'   prints the text to the console.
#'
#' @importFrom utils help 
#' @importFrom tools Rd2HTML Rd2txt
#' @importFrom knitr asis_output
#' @importFrom htmltools htmlPreserve
#' 
#' @examples
#' # Display help for the 'mean' function as plain text
#' displayHelp(mean)
#' 
#' # Display help for 'ggplot' from the ggplot2 package as HTML
#' # displayHelp("ggplot", pkg = "ggplot2", html = TRUE)
#' 
#' @export
#'
displayHelp <- function(x, pkg = NULL, html = FALSE) {
  # Convert unquoted name to string if necessary
  # Note: substitute(x) captures the literal expression passed by the user
  x_str <- if (is.character(substitute(x))) {
    x 
  } else {
    deparse(substitute(x))
  }

  # Look up help path
  h <- utils::help(x_str, (pkg))
  
  # Stop if help isn't found
  if (length(h) == 0) {
    return(message(paste("No help found for:", x_str)))
  }
  
  # Get help file
  help_file <- utils:::.getHelpFile(h)
  
  if (html) {
    # Render as HTML
    tc <- textConnection("s", "w", local = TRUE)
    on.exit(close(tc)) 
    tools:::Rd2HTML(help_file, out = tc)
    return(knitr::asis_output(htmltools::htmlPreserve(s)))
  } else {
    # Render as Plain Text
    txt <- capture.output(tools::Rd2txt(help_file))
    cat(paste(txt, collapse = "\n"))
  }
}
