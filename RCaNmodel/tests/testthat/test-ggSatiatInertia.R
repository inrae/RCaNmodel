test_that("ggSatiatInertia works", {
  expect_true(inherits(ggSatiatInertia(sampleCaNmod),
                       "ggplot"))
})


test_that("frac works in ggSatiatInertia", {
  expect_no_error(ggSatiatInertia(sampleCaNmod,"HerbZooplankton", frac = .5))
})

test_that("years works in ggSatiatInertia", {
  expect_no_error(ggSatiatInertia(sampleCaNmod,
                                  "HerbZooplankton",
                                  years = 1989:1990))
})
