library(proto)
library(ggplot2)
library(mermaidr)
library(dplyr)

test_that("mermaid_map_sites_static returns a map containing all the points", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
    mermaid_get_project_data("fishbelt", "sampleevents", limit = 25)
  p <- mermaid_map_sites_static(sample_events)
  expect_equal(nrow(ggplot_build(p)$data[[2]]), 25) # TODO: better way to extract this, so next test passes
})

# test_that("mermaid_map_sites_static map does not contain points if bounds exclude it", {
#   p <- mermaid_map_sites_static(sample_events, latitude_bounds = c(5, 6), longitude_bounds = c(120, 135))
#   expect_equal(nrow(ggplot_build(p)$data[[2]]), 0)
# })

test_that("mermaid_map_sites_static can plot by variable, adjusting size when variable is numeric and colour when it's categorical", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
    mermaid_get_project_data("fishbelt", "sampleevents", limit = 25)
  p <- mermaid_map_sites_static(sample_events, biomass_kgha_avg)
  expect_equal(rlang::as_name(p$layers[[2]]$mapping$size), "biomass_kgha_avg")

  p <- mermaid_map_sites_static(sample_events, reef_exposure)
  expect_equal(rlang::as_name(p$layers[[2]]$mapping$colour), "reef_exposure")
})

test_that("mermaid_map_sites_static visual options work", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
    mermaid_get_project_data("fishbelt", "sampleevents", limit = 25)

  p <- mermaid_map_sites_static(sample_events, label_sites = TRUE, scale = TRUE, arrow = TRUE, label_axes = FALSE, legend = FALSE)
  gb <- ggplot_build(p)
  expect_true("label" %in% names(gb$data[[3]]))
  expect_equal(p$labels$label, "site")
  expect_true(gb$data[[4]][["style"]] == "bar")
  expect_true(gb$data[[5]][["which_north"]] == "true")
  expect_true(gb$plot$theme$legend.position == "none")
  expect_true("element_blank" %in% class(gb$plot$theme$axis.text))
  expect_true("element_blank" %in% class(gb$plot$theme$axis.ticks))
  expect_true("element_blank" %in% class(gb$plot$theme$axis.title))
})

test_that("mermaid_static_map_bounds returns an error when the map doesn't have a bounds attribute", {
  m <- ggplot2::ggplot()
  expect_error(
    mermaid_static_map_bounds(m)
  )
})

test_that("mermaid_static_map_bounds returns bounds for a static map", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
    mermaid_get_project_data("fishbelt", "sampleevents", limit = 25)
  p <- mermaid_map_sites_static(sample_events)
  b <- mermaid_static_map_bounds(p)
  expect_named(b, c("latitude_bounds", "longitude_bounds"))
  expect_equal(round(b[[1]]), c(-6, -5))
  expect_equal(round(b[[2]]), c(132, 133))
})

test_that("mermaid_static_map_bounds has correct CRS for input data", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
    mermaid_get_project_data("fishbelt", "sampleevents", limit = 25)
  p <- mermaid_map_sites_static(sample_events)
  gb <- ggplot_build(p)
  expect_equal(sf::st_crs(p$layers[[2]]$data)$epsg, 4326)

  fiji_sites <- mermaid_get_sites() %>%
    filter(country == "Fiji") %>%
    head(10)
  p <- mermaid_map_sites_static(fiji_sites, use_fiji_crs = TRUE)
  gb <- ggplot_build(p)
  expect_equal(sf::st_crs(p$layers[[2]]$data)$epsg, 3460)
})

test_that("set lat/long bounds are used, and warn if they exclude points", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
    mermaid_get_project_data("fishbelt", "sampleevents", limit = 25)
  expect_warning(mermaid_map_sites_static(sample_events, latitude_bounds = c(-6, -5.8), longitude_bounds = c(132, 133)), "Not all sites are within")
  expect_equal(mermaid_static_map_bounds(mermaid_map_sites_static(sample_events, latitude_bounds = c(-6, -5.8), longitude_bounds = c(132, 133))), list(latitude_bounds = c(-6, -5.8), longitude_bounds = c(132, 133)))
})
