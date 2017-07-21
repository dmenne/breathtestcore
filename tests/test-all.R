library(testthat)
d13File = function(filename){
  system.file("extdata", filename, package = "breathtestcore")  
}

#test_check("breathtestcore", filter= "coef")
test_check("breathtestcore")
