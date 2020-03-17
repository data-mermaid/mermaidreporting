library(proto)
library(mermaidr)

test_that("Plot layers and labels are as expected", {
  skip_if_offline()
  events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
    mermaid_get_project_endpoint(endpoint = "beltfishes/sampleevents", limit = 100)
  p <- events %>%
    mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure)
  expect_is(p$layers[[1]]$geom, "GeomBoxplot")
  expect_equal(p$labels$x, "reef_exposure")
  expect_equal(p$labels$y, ".data$value")
})
