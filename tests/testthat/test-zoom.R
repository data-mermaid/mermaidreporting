test_that("zoom_to_centre works", {
  long_lat <- tibble::tribble (
    ~longitude, ~latitude,
    134.4131, -2.02302,
    134.8243, -2.90545,
    134.8653, -3.05547,
    135.1594, -3.10206
  )

  z1 <- zoom_to_centre(long_lat, 1)
  expect_equal(names(z1), c("longitude_bounds", "latitude_bounds"))
  expect_equal(z1[["longitude_bounds"]], c(44.78625, 224.78625))
  expect_equal(z1[["latitude_bounds"]], c(-47.56254, 42.43746))
})

test_that("zoom_fits returns TRUE when points are within perimeter, and FALSE when they're not", {
  long_lat <- tibble::tribble(
    ~longitude, ~latitude,
    178.7700, 17.61400,
    178.5435, 17.59500,
    178.5398, 17.72200,
    178.8300, 17.74600
  )

  expect_true(zoom_fits(long_lat, zoom_to_centre(long_lat, 1)))
  expect_false(zoom_fits(long_lat, zoom_to_centre(long_lat, 15)))
})

test_that("optimal_zoom returns one set of longitude/latitude bounds", {
  long_lat <- tibble::tribble(
    ~longitude, ~latitude,
    178.7700, 17.61400,
    178.5435, 17.59500,
    178.5398, 17.72200,
    178.8300, 17.74600
  )
  oz <- optimal_zoom(long_lat)
  expect_equal(names(oz), c("longitude_bounds", "latitude_bounds"))
  expect_true(zoom_fits(long_lat, oz))
})
