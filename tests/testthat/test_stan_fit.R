library(assertthat)
library(testthat)
library(ggplot2)
library(dplyr)
library(rstan)
library(stringr)
context("Bayesian fit with Stan")

test_that("stanmodels exist", {
  expect_true(exists("stanmodels"), "No stanmodels found")
})

test_that("stanmodels$test1 exists", {
  expect_silent(stanmodels$test1)
})



test_that("Data that cannot be fitted with nls_list/nlme work with stan_fit", {
  # with this seed, cf[10] does not fit with nls_list
  chains = 4
  student_df = 10
  dose = 100
  iter = 1000
  stanmodels[["breath_test_1.stan"]]
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  fit = stan_fit(data)
  expect_is(fit, "breathtestfit")
})

