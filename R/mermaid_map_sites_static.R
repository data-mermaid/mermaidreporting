#' Create a static map of MERMAID sample events sites
#'
#' Create a static map of sample events sites, zoomed in to the site locations. If \code{plot_var} is supplied, then the site points are based on \code{plot_var} - if it is a numeric variable, the sizes of the points vary with the sizes of the variable. If it is a character, factor, or logical variable, then the colours of the points vary with the different values of the variable. The map returned is a \code{ggplot2} plot, and so \code{ggplot2} layers can be built upon it for further customization - please see the examples section.
#'
#' @param .data Data frame with latitude and longitude of sample events sites.
#' @param plot_var Variable to plot by (optional).
#' @param use_fiji_crs Whether to use a coordinate reference system appropriate for mapping Fiji data. Defaults to FALSE.
#' @param jitter Amount of jittering applied to points. Defaults to 0.01. Set to 0 to remove jittering.
#' @param size Size of points (when \code{plot_var} is not a numeric variable). Defaults to 2.
#' @param colour Colour of points (when \code{plot_var} is not a character, factor, or logical variable). Defaults to red.
#' @param alpha Opacity of points. Defaults to 0.5.
#' @param label_sites Whether the sites should be labeled. Defaults to FALSE. Sites are labed by the variable \code{site_name}.
#' @param label_axes Whether the axes should be labeled. Defaults to TRUE.
#' @param scale Whether there should be a scale bar. Defaults to FALSE.
#' @param scale_position The position of the scale bar, if there is one. One of "bottomright", "bottomleft", "topright", "topleft". Defaults to "bottomright".
#' @param arrow Whether there should be a North arrow. Defaults to FALSE.
#' @param arrow_position The position of the arrow, if there is one. One of "bottomright", "bottomleft", "topright", "topleft". Defaults to "bottomright".
#' @param legend Whether there should be a plot legend (if \code{plot_var} is specified). Defaults to TRUE.
#' @param legend_position Position of the plot legend. One of "right", "left", "top", "bottom". Defaults to "right".
#' @param latitude_bounds Latitude bounds for the map. If both \code{latitude_bounds} and \code{longitude_bounds} are supplied, then they are used for the limits of the map. If neither (or only one) is supplied, then an optimal zoom is chosen automatically. Defaults to \code{NULL}.
#' @param longitude_bounds Longitude bounds for the map. If both \code{latitude_bounds} and \code{longitude_bounds} are supplied, then they are used for the limits of the map. If neither (or only one) is supplied, then an optimal zoom is chosen automatically. Defaults to \code{NULL}.
#'
#' @export
#'
#' @examples
#' library(mermaidr)
#' sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
#'   mermaid_get_project_endpoint(endpoint = "beltfishes/sampleevents", limit = 25)
#'
#' # Default map
#' mermaid_map_sites_static(sample_events)
#'
#' # Map sites by value of biomass_kgha_avg
#' mermaid_map_sites_static(sample_events, biomass_kgha_avg)
#'
#' # Map sites by value of reef_exposure
#' mermaid_map_sites_static(sample_events, reef_exposure)
#'
#' # Remove legend
#' mermaid_map_sites_static(sample_events, reef_exposure, legend = FALSE)
#'
#' # Change legend position
#' mermaid_map_sites_static(sample_events, reef_exposure, legend_position = "bottom")
#'
#' # Add labels for sites
#' mermaid_map_sites_static(sample_events, label_sites = TRUE)
#'
#' # Add arrow and scale bar
#' mermaid_map_sites_static(sample_events, scale = TRUE, arrow = TRUE)
#'
#' # Change position of arrow
#' mermaid_map_sites_static(sample_events, scale = TRUE, arrow = TRUE, arrow_position = "topleft")
#'
#' # Remove axes labels
#' mermaid_map_sites_static(sample_events, label_axes = FALSE)
#'
#' # Map sites with user-specified latitude and longitude bounds
#' mermaid_map_sites_static(sample_events,
#'   latitude_bounds = c(-6, -5),
#'   longitude_bounds = c(132, 133)
#' )
#'
#' # Since the map returned is a ggplot2 object, further customization can
#' # be done with ggplot2 code. For example, adding a title to the map
#' library(ggplot2)
#' mermaid_map_sites_static(sample_events, biomass_kgha_avg) +
#'   labs(title = "Sites by mean total biomass")
mermaid_map_sites_static <- function(.data, plot_var = NULL, use_fiji_crs = FALSE, jitter = 0.01, size = 2, colour = "red", alpha = 0.5,
                             label_sites = FALSE, label_axes = TRUE,
                             scale = FALSE, scale_position = c("bottomright", "bottomleft", "topright", "topleft"),
                             arrow = FALSE, arrow_position = c("bottomright", "bottomleft", "topright", "topleft"),
                             legend = TRUE, legend_position = c("right", "left", "top", "bottom"),
                             latitude_bounds = NULL, longitude_bounds = NULL) {

  # Check inputs

  ## Latitude and longitude
  if (!is.null(latitude_bounds) && !is.null(longitude_bounds)) {
    check_lat_long_bounds(latitude_bounds, longitude_bounds, .data[, c("longitude", "latitude")])
  } else {
    zoom_bounds <- optimal_zoom(.data[, c("longitude", "latitude")])
    longitude_bounds <- zoom_bounds[["longitude_bounds"]]
    latitude_bounds <- zoom_bounds[["latitude_bounds"]]
  }

  # Scale, arrow, legend positions
  scale_position <- rlang::arg_match(scale_position)
  arrow_position <- rlang::arg_match(arrow_position)
  legend_position <- rlang::arg_match(legend_position)

  .data <- as.data.frame(.data)
  data_sf <- sf::st_as_sf(.data, coords = c("longitude", "latitude"), crs = 4326)

  if (use_fiji_crs) {
    data_sf <- sf::st_transform(data_sf, crs = 3460)
  }

  # Initial plot
  worldmap <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  worldmap <- suppressMessages(suppressWarnings(sf::st_crop(worldmap, xmin = longitude_bounds[[1]], xmax = longitude_bounds[[2]], ymin = latitude_bounds[[1]], ymax = latitude_bounds[[2]], crs = ifelse(use_fiji_crs, 3460, 4326))))

  p <- ggplot2::ggplot(data = worldmap) +
    ggplot2::geom_sf(fill = "antiquewhite1") +
    ggplot2::theme_minimal()

  # Size/colour of points based on variable
  if (!missing(plot_var)) {
    plot_var <- rlang::enquo(plot_var)
    variable_name <- rlang::as_name(plot_var)
    variable_class <- class(.data[[variable_name]])
  }

  if (missing(plot_var)) {
    p <- p +
      ggplot2::geom_sf(data = sf::st_jitter(data_sf, jitter), col = colour, size = size, alpha = alpha)
  } else if (variable_class == "numeric") {
    p <- p +
      ggplot2::geom_sf(data = sf::st_jitter(data_sf, jitter), ggplot2::aes(size = !!plot_var), col = colour, alpha = alpha, show.legend = "point") +
      ggplot2::guides(size = ggplot2::guide_legend(title = to_title_case(variable_name)))
  } else if (variable_class %in% c("factor", "character", "logical")) {
    p <- p +
      ggplot2::geom_sf(data = sf::st_jitter(data_sf, jitter), ggplot2::aes(colour = !!plot_var), size = size, alpha = alpha, show.legend = "point") +
      ggplot2::guides(colour = ggplot2::guide_legend(title = to_title_case(variable_name)))
  }

  # Site labels
  if (label_sites) {
    p <- p +
      ggrepel::geom_text_repel(data = .data, ggplot2::aes_string(x = "longitude", y = "latitude", label = "site_name"))
  }

  # Scale bar
  if (scale) {
    p <- p +
      ggspatial::annotation_scale(location = recode_position(scale_position), width_hint = 0.2)
  }

  # North arrow
  if (arrow) {
    p <- p +
      ggspatial::annotation_north_arrow(
        location = recode_position(arrow_position), which_north = "true",
        pad_y = ggplot2::unit(ifelse(arrow_position == scale_position & scale, 0.3, ifelse(arrow_position %in% c("topleft", "topright"), 0.1, 0)), "in"),
        style = ggspatial::north_arrow_fancy_orienteering
      )
  }

  # Axes labels
  if (!label_axes) {
    p <- p +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  }

  # Legend
  if (legend) {
    p <- p +
      ggplot2::theme(
        legend.position = legend_position,
        legend.key = ggplot2::element_rect(fill = "white", size = 0)
      )
  } else if (!legend) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }

  # Latitude and longitude bounds
  p <- p +
    ggplot2::coord_sf(xlim = longitude_bounds, ylim = latitude_bounds, expand = FALSE)

  p <- p +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "aliceblue"),
      panel.grid.major = ggplot2::element_line(colour = "transparent")
    )

  attr(p, "bounds") <- list(
    latitude_bounds = latitude_bounds,
    longitude_bounds = longitude_bounds
  )

  p
}

#' Get latitude and longitude bounds from a static map
#'
#' @param map A static map produced from \code{mermaid_map_sites_static}
#'
#' @return A list of length two containing the longitude and latitude bounds of a map.
#' @export
#' @examples
#' library(mermaidr)
#' sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
#'   mermaid_get_project_endpoint(endpoint = "beltfishes/sampleevents", limit = 25)
#' p <- mermaid_map_sites_static(sample_events)
#' mermaid_static_map_bounds(p)
mermaid_static_map_bounds <- function(map) {
  check_if_static_map(map)
  attr(map, "bounds")
}

check_if_static_map <- function(map) {
  if (!inherits(map, "ggplot") || !("bounds" %in% names(attributes(map)))) {
    stop("`map` must be a static map obtained from `mermaid_map_sites_static()`",
      call. = FALSE
    )
  }
}

print.static_map <- function(x, ...) {
  print(x[["p"]])
  invisible(x)
}

map_bounds <- function(x) {
  x[["bounds"]]
}
