library(testthat)
d13File = function(filename){
  system.file("extdata", filename, package = "breathtestcore")  
}

test_check("breathtestcore", filter = "breathtest_read_function")
#test_check("breathtestcore")
