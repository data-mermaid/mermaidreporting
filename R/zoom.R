zoom_to_centre <- function(long_lat_points, zoom_level) {
  long_lat_points <- as.matrix(long_lat_points)
  longitude <- long_lat_points[, 1]
  latitude <- long_lat_points[, 2]

  longitude_range <- range(longitude)
  longitude_middle <- longitude_range[[1]] + abs(longitude_range[[1]] - longitude_range[[2]]) / 2
  latitude_range <- range(latitude)
  latitude_middle <- latitude_range[[1]] + abs(latitude_range[[1]] - latitude_range[[2]]) / 2

  zoom_to <- c(longitude_middle, latitude_middle)

  longitude_span <- 360 / 2^zoom_level
  latitude_span <- 180 / 2^zoom_level

  longitude_bounds <- c(zoom_to[1] - longitude_span / 2, zoom_to[1] + longitude_span / 2)
  latitude_bounds <- c(max(-90, zoom_to[2] - latitude_span / 2), min(90, zoom_to[2] + latitude_span / 2))

  return(list(
    longitude_bounds = longitude_bounds,
    latitude_bounds = latitude_bounds
  ))
}

zoom_fits <- function(long_lat_points, zoom_perimeter) {
  long_lat_points <- as.matrix(long_lat_points)
  longitude <- long_lat_points[, 1]
  latitude <- long_lat_points[, 2]

  longitude_bounds <- zoom_perimeter[["longitude_bounds"]]
  latitude_bounds <- zoom_perimeter[["latitude_bounds"]]

  min(longitude) > longitude_bounds[[1]] && max(longitude) < longitude_bounds[[2]] &&
    min(latitude) > latitude_bounds[[1]] && max(latitude) < latitude_bounds[[2]]
}

optimal_zoom <- function(long_lat_points) {
  zoom_levels <- 0:15
  z <- NA

  for (i in zoom_levels) {
    zoom_perimeter <- zoom_to_centre(long_lat_points, i)
    fits <- zoom_fits(long_lat_points, zoom_perimeter)
    if (fits) {
      z <- zoom_perimeter
    }
    else {
      return(z)
    }
  }
}
