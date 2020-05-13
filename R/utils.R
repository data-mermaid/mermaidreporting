check_data <- function(data, ...) {
  cols <- list(...)

  for (i in seq_along(cols)) {
    col <- cols[[i]]
    if (!(col %in% names(data))) {
      stop(deparse(substitute(data)), " must contain column `", col, "`",
        call. = FALSE
      )
    }
  }
}

to_title_case <- function(x) {
  x <- stringr::str_replace_all(x, "_", " ")
  stringr::str_to_title(x)
}

# Map utils

recode_position <- function(position) {
  switch(position,
    "bottomright" = "br",
    "bottomleft" = "bl",
    "topright" = "tr",
    "topleft" = "tl"
  )
}

check_lat_long_bounds <- function(latitude_bounds, longitude_bounds, points = NULL) {
  check_bounds(latitude_bounds, "latitude")
  check_bounds(longitude_bounds, "longitude")
  check_bounds_contain_points(points, latitude_bounds, longitude_bounds)
}

check_bounds <- function(x, type = c("latitude", "longitude")) {
  vector_check <- methods::is(x, "vector")
  length_check <- length(x) == 2
  numeric_check <- methods::is(x, "numeric")

  if (type == "latitude") {
    value_check <- x[1] >= -90 && x[1] <= 90 && x[2] >= -90 && x[2] <= 90
  } else if (type == "longitude") {
    value_check <- x[1] >= -180 && x[1] <= 180 && x[2] >= -180 && x[2] <= 180
  }

  if (!vector_check || !length_check || !numeric_check || !value_check) {
    if (type == "latitude") {
      stop("`latitude_bounds` must be a length 2 numeric vector with values between -90 and 90",
        call. = FALSE
      )
    } else if (type == "longitude") {
      stop("`longitude_bounds` must be a length 2 numeric vector with values between -180 and 180",
        call. = FALSE
      )
    }
  }
}

check_bounds_contain_points <- function(points, latitude_bounds, longitude_bounds) {
  latitude_range <- range(points[["latitude"]])
  longitude_range <- range(points[["longitude"]])

  contains_points <- latitude_bounds[[1]] <= latitude_range[[1]] && latitude_bounds[[2]] >= latitude_range[[2]] && longitude_bounds[[1]] <= longitude_range[[1]] && longitude_bounds[[2]] >= longitude_range[[2]]

  if (!contains_points) {
    warning("Not all sites are within the specified `latitude_bounds` and `longitude_bounds`.
Therefore, some sites are not included in the map.
The sites' latitude range is ", latitude_range[1], " to ", latitude_range[2], " (with `latitude_bounds` ", latitude_bounds[1], " to ", latitude_bounds[2], ").
The sites' longitude range is ", longitude_range[1], " to ", longitude_range[2], " (with `longitude_bounds` ", longitude_bounds[1], " to ", longitude_bounds[2], ").",
      call. = FALSE
    )
  }
}
