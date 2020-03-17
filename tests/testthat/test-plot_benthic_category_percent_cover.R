library(proto)

test_that("Plot layers and labels are as expected", {
  p <- plot_benthic_category_percent_cover(fiji_benthicpit_summary, management_rules)
  expect_is(p$layers[[1]]$geom, "GeomBoxplot")
  expect_equal(p$labels$x, "management_rules")
  expect_equal(p$labels$y, ".data$mean_percent_cover")
})
