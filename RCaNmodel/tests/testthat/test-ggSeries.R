test_that("ggSeries works", {
  expect_true(inherits(ggSeries(sampleCaNmod, "F01"),
                       "ggplot"))
})


test_that("plot_series works in ggSeries", {
  expect_no_error(ggSeries(sampleCaNmod,
                           "HerbZooplankton", 
                           plot_series = FALSE))
})


test_that("facet works in ggSeries", {
  expect_no_error(ggSeries(sampleCaNmod,
                           c("F01", "HerbZooplankton"), 
                           facet = FALSE))
})
