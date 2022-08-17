library(testthat)
library(RCaN)
Sys.setenv("R_TESTS" = "")
#Sys.unsetenv("R_TESTS") #avoid hanging when testing with parallel loop
#https://github.com/r-lib/devtools/issues/1526
test_check("RCaN")
