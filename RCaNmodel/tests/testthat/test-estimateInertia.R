test_that("estimateInertia work for copepods", {
  expect_equal(estimateInertia(2.35e-4,
                               method = "savage",
                               taxon = "multicellular eukaryote",
                               temperature = 280 - 273.15,
                               d = 243 / 365),
               5.491845,
               tolerance = 1e-4)
  expect_equal(estimateInertia(2.35e-4,
                               method = "yodzis",
                               taxon = "invertebrate",
                               fractional = 0.1),
               41.78504,
               tolerance = 1e-4)

})
