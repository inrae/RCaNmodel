file1 <- system.file("extdata",
                     "CaN_template_mini.xlsx", package = "RCaN")
file_generic <- system.file("extdata",
                            "CaN_template_mini_generic.xlsx", package = "RCaN")


myCaNmod <- buildCaN(file1)
myCaNmod_generic <- buildCaN(file_generic, generic = TRUE)

as.double_matrix <- function(x){
  x2 <- matrix(ncol = ncol(x),
               nrow = nrow(x))
  for (i in seq_along(nrow(x))){
    for (j in seq_along(ncol(x)))
      x2[i, j] <- as.numeric(x[i, j])
  }
}

test_that("classes and dims of outputs objects are correct", {
  symbo <- RCaN:::generateSymbolicObjects(myCaNmod$components_param,
                                          myCaNmod$fluxes_def,
                                          nrow(myCaNmod$series),
                                          myCaNmod$series,
                                          myCaNmod$aliases,
                                          myCaNmod$dynamics$Equation)

  expect_s4_class(symbo$IE_H, "DenseMatrix")
  expect_equal(dim(symbo$IE_H), rep(length(myCaNmod$species), 2))

  expect_type(symbo$N, "double")
  expect_equal(dim(symbo$N), c(length(myCaNmod$species),
                               nrow(myCaNmod$fluxes_def)))

})


test_that("classes and dims of outputs objects are correct with generic", {
  symbo <- RCaN:::generateSymbolicObjects(myCaNmod_generic$components_param,
                                          myCaNmod_generic$fluxes_def,
                                          nrow(myCaNmod_generic$series),
                                          myCaNmod_generic$series,
                                          myCaNmod_generic$aliases,
                                          myCaNmod_generic$dynamics$Equation)

  expect_s4_class(symbo$IE_H, "DenseMatrix")
  expect_equal(dim(symbo$IE_H), rep(length(myCaNmod_generic$species), 2))

  expect_type(symbo$N, "double")
  expect_equal(dim(symbo$N), c(length(myCaNmod_generic$species),
                               nrow(myCaNmod_generic$fluxes_def)))

})


test_that("generic and trophic are identical", {
  symbo2 <- RCaN:::generateSymbolicObjects(myCaNmod_generic$components_param,
                                          myCaNmod_generic$fluxes_def,
                                          nrow(myCaNmod_generic$series),
                                          myCaNmod_generic$series,
                                          myCaNmod_generic$aliases,
                                          myCaNmod_generic$dynamics$Equation)
  symbo <- RCaN:::generateSymbolicObjects(myCaNmod$components_param,
                                          myCaNmod$fluxes_def,
                                          nrow(myCaNmod$series),
                                          myCaNmod$series,
                                          myCaNmod$aliases,
                                          myCaNmod$dynamics$Equation)
  expect_equal(as.double_matrix(symbo$IE_H), as.double_matrix(symbo2$IE_H))
  expect_equal(symbo$N, symbo2$N)

})
