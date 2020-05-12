library(proto)
library(mermaidr)

test_that("mermaid_plot_fish_belt_biomass plot layers and labels are as expected", {
  events <- tibble::tibble(
    biomass_kgha_by_trophic_group_avg = tibble::tibble(`piscivore` = c(10, 30), `invertivore-mobile` = c(100, 120)),
    reef_exposure = c("exposed", "semi-exposed")
  )
  p <- events %>%
    mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure)
  expect_is(p$layers[[1]]$geom, "GeomBoxplot")
  expect_equal(p$labels$x, "reef_exposure")
  expect_equal(p$labels$y, "value")
})

test_that("mermaid_plot_fish_belt_biomass values cleaning works as expected", {
  events <- tibble::tibble(
    biomass_kgha_by_trophic_group_avg = tibble::tibble(`piscivore` = c(10, 30), `invertivore-mobile` = c(100, 120)),
    reef_exposure = c("exposed", "semi-exposed")
  )

  p <- events %>%
    mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure)
  expect_true(all(unique(p$data[["reef_exposure"]]) == c("Exposed", "Semi Exposed")))
  expect_true(all(unique(p$data[["group"]]) == c("Piscivore", "Invertivore Mobile")))

  p <- events %>%
    mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure, clean_values = FALSE)
  expect_true(all(unique(p$data[["reef_exposure"]]) == c("exposed", "semi-exposed")))
  expect_true(all(unique(p$data[["group"]]) == c("piscivore", "invertivore-mobile")))

  p <- events %>%
    mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure, clean_values_case = "sentence")
  expect_true(all(unique(p$data[["reef_exposure"]]) == c("Exposed", "Semi exposed")))
  expect_true(all(unique(p$data[["group"]]) == c("Piscivore", "Invertivore mobile")))

  p <- events %>%
    mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure, clean_values_case = "sentence", replace_dashes = FALSE)
  expect_true(all(unique(p$data[["reef_exposure"]]) == c("Exposed", "Semi-exposed")))
  expect_true(all(unique(p$data[["group"]]) == c("Piscivore", "Invertivore-mobile")))

  p <- events %>%
    mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure, replace_dashes = FALSE)
  expect_true(all(unique(p$data[["reef_exposure"]]) == c("Exposed", "Semi-Exposed")))
  expect_true(all(unique(p$data[["group"]]) == c("Piscivore", "Invertivore-Mobile")))
})

test_that("mermaid_plot_fish_belt_biomass errors if group_var is not a df-col", {
  events_clean <- tibble::tibble(
    biomass_kgha_by_trophic_group_avg = c(10, 100),
    trophic_group = c("piscivore", "invertivore-mobile"),
    reef_exposure = c("exposed", "semi-exposed"))
  expect_error(mermaid_plot_fish_belt_biomass(events_clean, biomass_kgha_by_trophic_group_avg, reef_exposure), "the raw output from a ")
})
