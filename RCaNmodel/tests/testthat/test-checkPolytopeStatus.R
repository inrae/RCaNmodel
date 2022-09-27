test_that("this polytope is ok", {
  expect_output(checkPolytopeStatus(list(A = diag(4),
                                          b = rep(1, 4))),
                 "polytope ok")

})


test_that("this polytope is unbounded", {
  expect_output(checkPolytopeStatus(list(A = diag(4)[-4, ],
                                         b = rep(1, 3))),
                "polytope not bounded")

})

test_that("this polytope is infeasible", {
  expect_output(checkPolytopeStatus(list(A = rbind(diag(4),
                                                   c(-1, 0, 0, 0)),
                                         b = c(rep(1, 4), -2))),
                "empty polytope")

})

test_that("incorrect dimensions are detected", {
  expect_error(checkPolytopeStatus(list(A = rbind(diag(4),
                                                  c(-1, 0, 0, 0)),
                                        b = rep(1, 4))))

})
