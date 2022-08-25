test_that("bounds are well estimated", {
  n <- 4
  A1 <- -diag(n)
  b1 <- as.matrix(rep(0,n))
  A2 <- diag(n)
  b2 <- as.matrix(rep(1,n))
  A <- rbind(A1,A2)
  b <- rbind(b1,b2)
  X0 <- getBoundParam(list(A = A, b = b), 1)
  expect_true(X0[1] == 0)
  expect_true(X0[2] == 1)
})
