
test_that("sampleCaN objects with good names", {
  res <- sampleCaN(myCaNmod, 100)
  res2 <- sampleCaN(res, 100)
  expect_equal(all(names(res) == names(res2)) &
                all(sort(names(res)) == sort(c("CaNmod", 
                                               "mcmc", 
                                               "covMat", 
                                               "x0", 
                                               "N", 
                                               "thin", 
                                               "nchain", 
                                               "method"))),
              TRUE)
})


test_that("sampleCaN mcmc with good dim", {
  res <- sampleCaN(myCaNmod, 50, thin = 2, nchain = 2, ncore = 2)
  expect_equal(coda::nchain(res$mcmc),
               2)
  expect_equal(coda::niter(res$mcmc),
               50)
  expect_equal(coda::nvar(res$mcmc),
               nrow(myCaNmod$fluxes_def) * (nrow(myCaNmod$series) - 1) +
                 length(myCaNmod$species) * nrow(myCaNmod$series))
})


test_that("sampleCaN works when there is no C", {
  myCaNmod$C <- matrix(0, 0, ncol(myCaNmod$A))
  myCaNmod$v <- numeric(0)
  expect_no_error(sampleCaN(myCaNmod, 50, thin = 2, nchain = 2, ncore = 2))
})
