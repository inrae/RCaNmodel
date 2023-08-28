test_that("ggTopDownBottomUp works with single species", {
  expect_true(inherits(ggTopDownBottomUp(sampleCaNmod,list("HerbZooplankton" = NULL)),
              "ggplot"))
})


test_that("invalid syntax is detected in ggTopDownBottomUp", {
  expect_error(ggTopDownBottomUp(sampleCaNmod,"HerbZooplankton"))
})
