test_that("ggTopDownBottomUp works with single species", {
  expect_true(inherits(ggTopDownBottomUp(sampleCaNmod,
                                         list("HerbZooplankton" = NULL)),
              "ggplot"))
})


test_that("frac works in ggTopDownBottomUp", {
  expect_no_error(ggTopDownBottomUp(sampleCaNmod, frac = .5))
})
