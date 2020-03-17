test_that("summary_table output is as expected", {
  benthicpit_df <- tibble::tribble(
    ~country, ~site, ~year, ~latitude, ~longitude, ~exposure, ~reef_type, ~reef_zone, ~management_name, ~management_rules, ~estimated_compliance, ~benthic_category, ~mean_depth, ~mean_percent_cover, ~mean_hard_coral_percent_cover,
    "Fiji", "Test1", 2019, -17.8, 179.0, "sheltered", "fringing", "back reef", "Mgmt", "Periodic Closure", NA, "Bare substrate", 3.3, .10, 0.50,
    "Fiji", "Test1", 2019, -17.8, 179.0, "sheltered", "fringing", "back reef", "Mgmt", "Periodic Closure", NA, "Crustose coralline algae", 3.5, .20, 0.50,
  )
  beltfish_df <- tibble::tribble(
    ~country, ~site, ~year, ~latitude, ~longitude, ~exposure, ~reef_type, ~reef_zone, ~management_name, ~management_rules, ~estimated_compliance, ~mean_total_biomass, ~fish_family, ~mean_depth, ~mean_total_biomass_fish_family,
    "Fiji", "Test1", 2019, -17.8, 179.0, "sheltered", "fringing", "back reef", "Mgmt", "Periodic Closure", NA, 400, "Acanthuridae", 3.5, 50,
    "Fiji", "Test1", 2019, -17.8, 179.0, "sheltered", "fringing", "back reef", "Mgmt", "Periodic Closure", NA, 400, "Carangidae", 3.3, 100
  )
  output <- summary_table(benthicpit_df, beltfish_df)
  expect_equal(colnames(output), c("country", "site", "year", "latitude", "longitude", "exposure", "reef_type", "reef_zone", "management_name", "management_rules", "estimated_compliance", "mean_hard_coral_percent_cover", "percent_cover_bare_substrate", "percent_cover_crustose_coralline_algae", "mean_total_biomass", "mean_total_biomass_acanthuridae", "mean_total_biomass_carangidae"))
  expect_equal(nrow(output), 1)
})
