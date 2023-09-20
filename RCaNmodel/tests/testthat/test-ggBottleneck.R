test_that("ggBottleneck works", {
  expect_true(inherits(ggBottleneck(sampleCaNmod),
                       "ggplot"))
})


test_that("frac works in ggBottleneck", {
  expect_no_error(ggBottleneck(sampleCaNmod,"HerbZooplankton", frac = .5))
})

test_that("years works in ggBottleneck", {
  expect_no_error(ggBottleneck(sampleCaNmod,
                               "HerbZooplankton",
                               years = 1989:1990))
})
