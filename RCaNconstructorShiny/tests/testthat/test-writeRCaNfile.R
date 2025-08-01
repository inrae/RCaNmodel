test_that("writeRCaNfile works", {
  network <- loadRCaNfile(
    system.file("extdata",
                "CaN_template_mini.xlsx", package = "RCaNmodel"),
    "test")
  file <- openxlsx2::wb_workbook()
  param <- read.csv(system.file("info.csv", package = "RCaNconstructor"),
                    sep = "\t",
                    header=FALSE, 
                    na.strings = "")
  
  input <- list()
  input[[param[1, 1]]] <- param[1, 1]
  input[[param[2, 1]]] <- param[2, 1]
  for (i in seq_len(nrow(param))){
    if (!is.na(param[i, 2])){
      input[[param[i, 2]]] <- param[i, 3]
    }
  }
  
  
  
  file <- writeRCaNfile(file, network, param, input, NULL)
  filepath <- paste0(tempfile(), ".xlsx")
  openxlsx2::wb_save(file, filepath)
  
  expect_equal(dim(readxl::read_excel(filepath, "Components & input parameter")),
               c(4, 10))
  expect_equal(dim(readxl::read_excel(filepath, "Fluxes")),
               c(4, 4))
  expect_equal(dim(readxl::read_excel(filepath, "Constraints")),
               c(3, 4))
  expect_equal(dim(readxl::read_excel(filepath, "Aliases")),
               c(0, 3))
  expect_equal(dim(readxl::read_excel(filepath, "Observation MetaInfo")),
               c(12, 2))
  expect_equal(dim(readxl::read_excel(filepath, "TimeLines")),
               c(0, 3))
  expect_equal(dim(readxl::read_excel(filepath, "Input time-series")),
               c(4, 13))
  expect_equal(dim(readxl::read_excel(filepath, "INFO")),
               c(34, 3))
               
})
