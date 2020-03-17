#' Create an interactive map of MERMAID sample events sites
#'
#' Create a static map of sites, zoomed in to the sample events sites. If \code{plot_var} is supplied, then the colour of the site points are based on \code{plot_var}. The map returned is a \code{leaflet} plot, and so \code{leaflet} layers can be built upon it for further customization - please see the examples section.
#'
#' @param .data Data frame with latitude and longitude of sites.
#' @param plot_var Variable to plot by (optional).
#' @param basemap Basemap layer. See http://leaflet-extras.github.io/leaflet-providers/preview/ for options. Defaults to "Esri.WorldImagery".
#' @param jitter Amount of jittering applied to points. Defaults to 0.01. Set to 0 to remove jittering.
#' @param size Size of points (when \code{plot_var} is not a numeric variable). Defaults to 2.
#' @param colour Colour of points (when \code{plot_var} is not a character, factor, or logical variable). Defaults to red.
#' @param alpha Opacity of points. Defaults to 0.5.
#' @param scale Whether there should be a scale bar. Defaults to FALSE.
#' @param scale_position The position of the scale bar, if there is one. One of "bottomright", "bottomleft", "topright", "topleft". Defaults to "bottomright".
#' @param legend Whether there should be a plot legend (if \code{plot_var} is specified). Defaults to TRUE.
#' @param legend_position Position of the plot legend. One of "bottomright", "bottomleft", "topright", "topleft". Defaults to "bottomright".
#'
#' @export
#'
#' @examples
#' library(mermaidr)
#' sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
#'   mermaid_get_project_endpoint(endpoint = "beltfishes/sampleevents", limit = 25)
#'
#' # Default map
#' mermaid_map_sites_interactive(sample_events)
#'
#' # Map sites by value of biomass_kgha_avg
#' mermaid_map_sites_interactive(sample_events, biomass_kgha_avg)
#'
#' # Map sites by value of reef_exposure
#' mermaid_map_sites_interactive(sample_events, reef_exposure)
#'
#' # Remove legend
#' mermaid_map_sites_interactive(sample_events, reef_exposure, legend = FALSE)
#'
#' # Change legend position
#' mermaid_map_sites_interactive(sample_events, reef_exposure, legend_position = "topright")
#'
#' # Add scale bar
#' mermaid_map_sites_interactive(sample_events, scale = TRUE)
#'
#' # Change position of scale bar
#' mermaid_map_sites_interactive(sample_events, scale = TRUE, scale_position = "topright")
#'
#' # Since the map returned is a leaflet object, further customization can
#' # be done with leaflet code. For example, adding a mini-map for further navigation:
#' mermaid_map_sites_interactive(sample_events) %>%
#'   leaflet::addMiniMap()
#'
#' # You can also save a static image of the map using the mapview package:
#' \dontrun{
#' library(mapview)
#' m <- mermaid_map_sites_interactive(sample_events, reef_exposure, legend_position = "topright")
#' mapshot(m, file = "sites_by_management_rule.png")
#' }
mermaid_map_sites_interactive <- function(.data, plot_var = NULL, basemap = "Esri.WorldImagery", jitter = 0.01, size = 2, colour = "red", alpha = 0.5,
                                  scale = FALSE, scale_position = c("bottomright", "bottomleft", "topright", "topleft"),
                                  legend = TRUE, legend_position = c("bottomright", "bottomleft", "topright", "topleft")) {

  # Check inputs

  # Scale, legend positions
  if (scale) scale_position <- rlang::arg_match(scale_position)
  if (legend) legend_position <- rlang::arg_match(legend_position)

  # Change data structure if plotting by variable
  if (!missing(plot_var)) {
    plot_var <- rlang::enquo(plot_var)
    variable_name <- rlang::as_name(plot_var)
    variable_class <- class(.data[[variable_name]])

    .data <- .data %>%
      dplyr::rename("plot_variable" := !!plot_var)
  }

  # Jitter points
  .data <- .data %>%
    dplyr::mutate_at(dplyr::vars(latitude, longitude), ~ jitter(.x, factor = jitter))

  # Initial plot
  p <- leaflet::leaflet(.data) %>%
    leaflet::addProviderTiles(provider = basemap)

  # Size/colour of points based on variable
  if (missing(plot_var)) {
    p <- p %>%
      leaflet::addCircleMarkers(
        lng = ~longitude, lat = ~latitude, radius = size,
        color = colour, opacity = alpha, fillOpacity = alpha
      )
  } else {
    if (variable_class == "numeric") {
      colour_palette <- leaflet::colorNumeric(
        palette = "viridis",
        domain = .data$plot_variable
      )
    } else if (variable_class %in% c("factor", "character", "logical")) {
      colour_palette <- leaflet::colorFactor(
        palette = "viridis",
        domain = .data$plot_variable
      )
    }

    p <- p %>%
      leaflet::addCircleMarkers(
        lng = ~longitude, lat = ~latitude, radius = 2,
        color = ~ colour_palette(plot_variable), opacity = 0.5
      )

    if (legend) {
      p <- p %>%
        leaflet::addLegend(legend_position,
          pal = colour_palette, values = ~plot_variable,
          title = to_title_case(variable_name),
          opacity = 1
        )
    }
  }

  # Scale bar
  if (scale) {
    p <- p %>%
      leaflet::addScaleBar(position = scale_position)
  }

  p
}
