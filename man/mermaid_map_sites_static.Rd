% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mermaid_map_sites_static.R
\name{mermaid_map_sites_static}
\alias{mermaid_map_sites_static}
\title{Create a static map of MERMAID sample events sites}
\usage{
mermaid_map_sites_static(
  .data,
  plot_var = NULL,
  use_fiji_crs = FALSE,
  bb_ext = 1.1,
  jitter = 0.01,
  size = 2,
  colour = "red",
  alpha = 0.5,
  label_sites = FALSE,
  label_axes = TRUE,
  scale = FALSE,
  scale_position = c("bottomright", "bottomleft", "topright", "topleft"),
  arrow = FALSE,
  arrow_position = c("bottomright", "bottomleft", "topright", "topleft"),
  legend = TRUE,
  legend_position = c("right", "left", "top", "bottom"),
  latitude_bounds = NULL,
  longitude_bounds = NULL
)
}
\arguments{
\item{.data}{Data frame with latitude and longitude of sample events sites.}

\item{plot_var}{Variable to plot by (optional).}

\item{use_fiji_crs}{Whether to use a coordinate reference system appropriate for mapping Fiji data. Defaults to FALSE.}

\item{bb_ext}{Extension factor of the map's bounding box. Values smaller than 1 reduce the bounding box, and values larger than 1 enlarge the bounding box. The default is 1.1.}

\item{jitter}{Amount of jittering applied to points. Defaults to 0.01. Set to 0 to remove jittering.}

\item{size}{Size of points (when \code{plot_var} is not a numeric variable). Defaults to 2.}

\item{colour}{Colour of points (when \code{plot_var} is not a character, factor, or logical variable). Defaults to red.}

\item{alpha}{Opacity of points. Defaults to 0.5.}

\item{label_sites}{Whether the sites should be labeled. Defaults to FALSE. Sites are labeled by the variable \code{site}.}

\item{label_axes}{Whether the axes should be labeled. Defaults to TRUE.}

\item{scale}{Whether there should be a scale bar. Defaults to FALSE.}

\item{scale_position}{The position of the scale bar, if there is one. One of "bottomright", "bottomleft", "topright", "topleft". Defaults to "bottomright".}

\item{arrow}{Whether there should be a North arrow. Defaults to FALSE.}

\item{arrow_position}{The position of the arrow, if there is one. One of "bottomright", "bottomleft", "topright", "topleft". Defaults to "bottomright".}

\item{legend}{Whether there should be a plot legend (if \code{plot_var} is specified). Defaults to TRUE.}

\item{legend_position}{Position of the plot legend. One of "right", "left", "top", "bottom". Defaults to "right".}

\item{latitude_bounds}{Latitude bounds for the map. If both \code{latitude_bounds} and \code{longitude_bounds} are supplied, then they are used for the limits of the map. If neither (or only one) is supplied, then an optimal zoom is chosen automatically. Defaults to \code{NULL}.}

\item{longitude_bounds}{Longitude bounds for the map. If both \code{latitude_bounds} and \code{longitude_bounds} are supplied, then they are used for the limits of the map. If neither (or only one) is supplied, then an optimal zoom is chosen automatically. Defaults to \code{NULL}.}
}
\description{
Create a static map of sample events sites, zoomed in to the site locations. If \code{plot_var} is supplied, then the site points are based on \code{plot_var} - if it is a numeric variable, the sizes of the points vary with the sizes of the variable. If it is a character, factor, or logical variable, then the colours of the points vary with the different values of the variable. The map returned is a \code{ggplot2} plot, and so \code{ggplot2} layers can be built upon it for further customization - please see the examples section.
}
\examples{
\donttest{
library(mermaidr)
sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") \%>\%
  mermaid_get_project_data("fishbelt", "sampleevents", limit = 25)

# Default map
mermaid_map_sites_static(sample_events)

# Map sites by value of biomass_kgha_avg
mermaid_map_sites_static(sample_events, biomass_kgha_avg)

# Map sites by value of reef_exposure
mermaid_map_sites_static(sample_events, reef_exposure)

# Remove legend
mermaid_map_sites_static(sample_events, reef_exposure, legend = FALSE)

# Change legend position
mermaid_map_sites_static(sample_events, reef_exposure, legend_position = "bottom")

# Add labels for sites
mermaid_map_sites_static(sample_events, label_sites = TRUE)

# Add arrow and scale bar
mermaid_map_sites_static(sample_events, scale = TRUE, arrow = TRUE)

# Change position of arrow
mermaid_map_sites_static(sample_events, scale = TRUE, arrow = TRUE, arrow_position = "topleft")

# Remove axes labels
mermaid_map_sites_static(sample_events, label_axes = FALSE)

# Map sites with user-specified latitude and longitude bounds
mermaid_map_sites_static(sample_events,
  latitude_bounds = c(-6, -5),
  longitude_bounds = c(132, 133)
)

# Since the map returned is a ggplot2 object, further customization can
# be done with ggplot2 code. For example, adding a title to the map
library(ggplot2)
mermaid_map_sites_static(sample_events, biomass_kgha_avg) +
  labs(title = "Sites by mean total biomass")
}
}
