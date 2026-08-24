#' Plot a stem map for one or more plots
#'
#' @param x dataframe containing stem measurements
#' @param rel_col optional, character vector of length 2 specifying the columns for local 
#'   relative stem coordinates (e.g., \code{c("x_rel", "y_rel")}).
#' @param lonlat_col optional, character vector of length 2 specifying the columns for 
#'   longitude and latitude.
#' @param proj_col optional, character vector of length 2 specifying the columns with UTM coordinates
#' @param poly optional `sf` polygon object containing plot boundaries
#' @param plot_id column name in `x` containing plot IDs in `x` and `poly`
#'
#' @return \code{ggplot} object or list of objects one for each plot in `x`
#' 
#' @examples
#' # simple relative coordinate plot
#' plot_stem_map(stem_data, rel_col = c("x", "y"), plot_var = "plot_id")
#' 
#' @export
plotStemMap <- function(x, rel_col = NULL, lonlat_col = NULL, proj_col = NULL, 
  plot_id = NULL, poly = NULL, poly_plot_id = NULL) {
  
  # 1. Handle Geospatial Mapping (sf)
  if (!is.null(lonlat_col) || !is.null(proj_col)) {
    if (!is.null(lonlat_col)) {
      stems_sf <- sf::st_as_sf(x, coords = lonlat_col, crs = 4326)
    } else {
      stems_sf <- sf::st_as_sf(x, coords = c("X", "Y"), crs = unique(x[[proj_col]])[1])
    }
    
    p <- ggplot2::ggplot() 
    
    if (!is.null(poly)) {
      if (sf::st_crs(poly) != sf::st_crs(stems_sf)) {
        poly <- sf::st_transform(poly, sf::st_crs(stems_sf))
      }
      p <- p + ggplot2::geom_sf(data = poly, fill = NA, color = "black", linewidth = 0.8)
    }
    
    p <- p + ggplot2::geom_sf(data = stems_sf, 
                              ggplot2::aes(size = if(!is.null(size_var)) .data[[size_var]] else NULL),
                              alpha = 0.6, color = "forestgreen")
    
  } else if (!is.null(rel_col)) {
    # 2. Handle Local/Relative Cartesian Mapping
    p <- ggplot2::ggplot(x, ggplot2::aes(x = .data[[rel_col[1]]], 
                                         y = .data[[rel_col[2]]])) +
      ggplot2::geom_point(ggplot2::aes(size = if(!is.null(size_var)) .data[[size_var]] else NULL),
                          alpha = 0.6, color = "forestgreen") +
      ggplot2::coord_fixed() + 
      ggplot2::theme_bw() +
      ggplot2::labs(x = "relative easting (m)", y = "relative northing (m)")
  } else {
    stop("you must provide either 'rel_col' or 'lonlat_col' to map the stems.")
  }

  if (!is.null(plot_var) && length(unique(x[[plot_var]])) > 1) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", plot_var)), scales = "free")
  }

  return(p)
}
