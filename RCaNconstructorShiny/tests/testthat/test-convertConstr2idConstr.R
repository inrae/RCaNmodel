dictionary <- c(`id-test` = "new",
                `3565-54qg` = "new2")
test_that("consrt2idconstr works", {
  expect_equal(convertConstr2idConstr("Beforenew+AfterRationew2>=33.2",
                                      dictionary),
               "Before{id-test}+AfterRatio{3565-54qg}>=33.2")
})
