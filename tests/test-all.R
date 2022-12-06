library(testthat)

options(Ncpus = parallelly::availableCores(omit = 1))
test_check("breathtestcore")
