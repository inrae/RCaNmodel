test_that("ggSeries works", {
  expect_true(inherits(ggSeries(sampleCaNmod, "F01"),
                       "ggplot"))
})


test_that("frac works in ggSeries", {
  expect_no_error(ggTrophicRelation(ggSeries,"HerbZooplankton", frac = .5))
})
