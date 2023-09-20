test_that("generate extract file", {
  outfile <- paste0(tempfile(), ".xlsx")
  getTimerestrictedRCaNfile(templatefiles[[1]], outfile, 1989)
  expect_true(file.exists(outfile))
  expect_s3_class(buildCaN(outfile), "CaNmod")
})
