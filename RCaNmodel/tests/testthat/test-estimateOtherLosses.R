test_that("estimateOtherLosses work for fish", {
  expect_equal(estimateOtherLosses(240,
                                   "yodzis",
                                   taxon = "vertebrate ecotherm"),
               3.286056,
               tolerance = 1e-4)
  expect_equal(estimateOtherLosses(240, "gillooly", "fish", 13, 1),
               2.648084,
               tolerance = 1e-4)
  expect_equal(estimateOtherLosses(240, "makarieva", "fish", 13, 1),
               4.791929,
               tolerance = 1e-4)

})
