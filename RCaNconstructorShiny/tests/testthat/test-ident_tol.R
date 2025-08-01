test_that("ident_tol works", {
  expect_true(ident_tol(c(0,0,0), c(0,0,0.00001)))
  expect_true(ident_tol(c(0,0,0), c(0,0,0.1), tolerance = 0.2))
  expect_false(ident_tol(c(0,0,0), c(0,0,0.31), tolerance = 0.2))
  expect_true(ident_tol(c("toto", "tata"), c("toto", "tata"), tolerance = 0.2))
})
