context("Exponential beta function")


test_that( "exp_beta returns values and gradient", {
  start = list(m = 20, k = 1/100, beta = 2)
  pdr =  exp_beta(seq(1, 100, by = 10), 100, start$m, start$k, start$beta)
  expect_lt(abs(max(pdr) - 10), 0.01)
  expect_is(attr(pdr, "gradient"), "matrix")
  ce = cum_exp_beta(c(0.00001,10000), 100, start)
  expect_lt(ce[1], 0.001)
  expect_gt(ce[2], 99.999)
})

