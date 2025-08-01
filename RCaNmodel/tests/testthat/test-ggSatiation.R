test_that("ggSatiation works", {
  expect_true(inherits(ggSatiation(sampleCaNmod),
                       "ggplot"))
})


test_that("frac works in ggSatiation", {
  expect_no_error(ggSatiation(sampleCaNmod,
                              "HerbZooplankton",
                              frac = .5))
})

test_that("years works in ggSatiation", {
  expect_no_error(ggSatiation(sampleCaNmod,
                              "HerbZooplankton",
                              years = 1989:1990))
})
