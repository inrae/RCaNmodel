test_that("ggTrophicRelation works", {
  expect_true(inherits(ggTrophicRelation(sampleCaNmod),
                       "ggplot"))
})


test_that("frac works in ggTrophicRelation", {
  expect_no_error(ggTrophicRelation(sampleCaNmod,"HerbZooplankton", frac = .5))
})
