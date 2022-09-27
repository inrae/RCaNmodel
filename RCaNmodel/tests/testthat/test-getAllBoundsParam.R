


test_that("all bounds are well estimated", {
  n <- 4
  A1 <- -diag(n)
  b1 <- as.matrix(rep(0,n))
  A2 <- diag(n)
  b2 <- as.matrix(rep(1,n))
  A <- rbind(A1,A2)
  b <- rbind(b1,b2)
  X0 <- getAllBoundsParam(list(A = A, b = b))
  expect_equal(nrow(X0), n)
  expect_true(all(X0$lowerbound == 0))
  expect_true(all(X0$uppperbound == 1))
})
