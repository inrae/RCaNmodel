dictionary <- c(`id-test` = "new",
                `3565-54qg` = "new2")
test_that("idconsrt2constr", {
  expect_equal(convertidConstr2Constr("Before{id-test}+After{3565-54qg}>=33.2",
                                      dictionary),
               "Beforenew+Afternew2>=33.2")
})
