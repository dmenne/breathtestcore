library(testthat)

options(Ncpus = parallel::detectCores(logical = TRUE))
test_check("breathtestcore")
