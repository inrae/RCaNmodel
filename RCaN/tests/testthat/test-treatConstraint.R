file1 <- system.file("extdata",
                     "CaN_template_mini.xlsx", package = "RCaN")
myCaNmod <- buildCaN(file1)
symbo <- RCaN:::generateSymbolicObjects(myCaNmod$components_param,
                                        myCaNmod$fluxes_def,
                                        nrow(myCaNmod$series),
                                        myCaNmod$series,
                                        myCaNmod$aliases,
                                        myCaNmod$dynamics$Equation)


test_that("refuge is ok", {
  mat <- treatConstraint("HerbZooplankton >= 1", symbo, "1989:1990", "test")
  expect_equal(nrow(mat), 2)
})


test_that("standardisation by mean is ok even with NA", {
  symbo$HerbZooplankton_Biomass[1] <- NaN
  mat <- treatConstraint(
    "HerbZooplankton/mean(HerbZooplankton) = HerbZooplankton_Biomass/mean(HerbZooplankton_Biomass)",
    symbo,
    "1988:1991",
    "test")
  expect_equal(nrow(mat), 3)
})

test_that("standardisation by sum is ok even with NA", {
  symbo$HerbZooplankton_Biomass[1] <- NaN
  mat <- treatConstraint(
    "HerbZooplankton/sum(HerbZooplankton) = HerbZooplankton_Biomass/sum(HerbZooplankton_Biomass)",
    symbo,
    "1988:1991",
    "test")
  expect_equal(nrow(mat), 3)
})


test_that("single constraint works ok", {
  mat <- treatConstraint(
    "mean(HerbZooplankton) = 120",
    symbo,
    NA,
    "test")
  expect_equal(nrow(mat), 1)
})
