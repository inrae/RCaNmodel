test_that("ggPairsBiomass works", {
  expect_true(inherits(ggPairsBiomass(sampleCaNmod),
                       "ggplot"))
})


test_that("frac works in ggPairsBiomass", {
  expect_no_error(ggPairsBiomass(sampleCaNmod,"HerbZooplankton", frac = .5))
})

test_that("years works in ggPairsBiomass", {
  expect_no_error(ggPairsBiomass(sampleCaNmod,
                                 "HerbZooplankton",
                                 years = 1989:1990))
})

test_that("logscale works in ggPairsBiomass", {
  expect_no_error(ggPairsBiomass(sampleCaNmod,
                                 "HerbZooplankton",
                                 logscale = TRUE))
})
