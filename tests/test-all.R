library(testthat)

#options(Ncpus = parallelly::availableCores(omit = 1))
#options(Ncpus = 1)
options(lifecycle_verbosity = "warning")
test_check("breathtestcore")
