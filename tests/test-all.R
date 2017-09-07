library(testthat)
d13File = function(filename){
  system.file("extdata", filename, package = "breathtestcore")  
}

test_check("breathtestcore", filter = "cleanup_data")
#test_check("breathtestcore")
