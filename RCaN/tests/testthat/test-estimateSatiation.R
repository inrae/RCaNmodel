test_that("estimateSatiation work for copepods", {
  expect_equal(estimateSatiation(2.35e-4, taxon = "invertebrate", 1, 0.65, 0.3),
               203.3352,
               tolerance = 1e-4)

})
