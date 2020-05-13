test_that("check_data returns an error when the indicated columns aren't present in data", {
  df <- dplyr::tibble(a = 1, b = 2)
  expect_error(check_data(df, "c"))
  expect_error(check_data(df, "a", "c"))
  expect_silent(check_data(df, "a", "b"))
})

test_that("check_bounds works", {
  expect_error(check_bounds(list(1, 2), "longitude"))
  expect_error(check_bounds(c(1), "longitude"))
  expect_error(check_bounds(c("a", "b"), "longitude"))
  expect_error(check_bounds(c(-300, 300), "longitude"))
  expect_error(check_bounds(c(-100, 50), "latitude"))
  expect_silent(check_bounds(c(-180, -175), "longitude"))
  expect_silent(check_bounds(c(10, 15), "latitude"))
})

test_that("check_lat_long_bounds works", {
  expect_error(check_lat_long_bounds(c(10, 14), c(-190, 190)))
  expect_error(check_lat_long_bounds(c(-100, -75), c(-150, -120)))
  expect_error(check_lat_long_bounds(c(-100, -75), c(-190, 190)))

  df <- dplyr::tibble(longitude = c(179, 178), latitude = c(-17.8, -14))
  expect_silent(check_lat_long_bounds(points = df, c(-18, -13), c(175, 180)))
  expect_warning(
    check_lat_long_bounds(
      df,
      latitude_bounds = c(-19, -17),
      longitude_bounds = c(177, 180)
    )
  )
})

test_that("to_title_case replacws underscores with spaces and converts to title case", {
  expect_equal(to_title_case("management_rules"), "Management Rules")
})

test_that("recode_position recodes properly", {
  expect_equal(recode_position("bottomright"), "br")
  expect_equal(recode_position("bottomleft"), "bl")
  expect_equal(recode_position("topright"), "tr")
  expect_equal(recode_position("topleft"), "tl")
})
