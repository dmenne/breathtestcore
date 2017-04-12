library(testthat)
options(warn = 2)
d13File = function(filename){
  system.file("extdata", filename, package = "breathtestcore")  
}

test_check("breathtestcore")
