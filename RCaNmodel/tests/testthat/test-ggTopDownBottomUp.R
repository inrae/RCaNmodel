myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
                                 package = "RCaNmodel"))
res <- sampleCaN(myCaNmod, 100)



test_that("ggTopDownBottomUp works with single species", {
  expect_true(inherits(ggTopDownBottomUp(res,list("HerbZooplankton" = NULL)),
              "ggplot"))
})


test_that("invalid syntax is detected in ggTopDownBottomUp", {
  expect_error(ggTopDownBottomUp(res,"HerbZooplankton"))
})
