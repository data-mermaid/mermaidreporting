test_that("bounds returns an error when the map does not have class static_map", {
  m <- ggplot2::ggplot()
  expect_error(
    bounds(m)
  )
})
