test_that("ggDiet works", {
  expect_true(inherits(ggDiet(sampleCaNmod, "HerbZooplankton"),
                       "ggplot"))
})


test_that("years works in ggDiet", {
  expect_no_error(ggDiet(sampleCaNmod,
                         "HerbZooplankton",
                         years = 1989:1990))
})

test_that("barplot works in ggBottleneck", {
  expect_no_error(ggDiet(sampleCaNmod,
                         "HerbZooplankton",
                         barplot=FALSE))
})
