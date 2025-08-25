test_that("getConstraintWord works", {
  expect_equal(
    getConstraintWord("new+new2>=new3*2.22"),
    c("new", "+", "new2", ">", "=", "new3", "*", "2.22")
    )
})
