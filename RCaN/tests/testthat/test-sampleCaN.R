myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
                                  package = "RCaN"))

test_that("can sample", {
  res <- sampleCaN(myCaNmod, 100)
  res2 <- sampleCaN(res, 100)
})
