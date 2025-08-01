test_that("loadRCaNfile works", {
  network <- loadRCaNfile(
    system.file("extdata",
                "CaN_template_mini.xlsx", package = "RCaNmodel"),
    "test")
  expect_equal(nrow(network$components), 4)
  expect_equal(ncol(network$components), 11)
  expect_equal(ncol(network$fluxes), 7)
  expect_equal(nrow(network$fluxes), 4)
  expect_equal(length(network$dictionary), 32)
  expect_equal(dim(network$metaobs), c(12, 3))
  expect_equal(dim(network$constraints), c(3, 8))
  expect_equal(dim(network$observations), c(4, 13))
})
