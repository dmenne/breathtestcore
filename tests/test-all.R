library(testthat)

#options(Ncpus = parallelly::availableCores(omit = 1))
#options(Ncpus = 1)
options(lifecycle_verbosity = "warning")
options(warn = 0) # Try to silence problems with Fedora build July 2026
test_check("breathtestcore")
