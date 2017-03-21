context("Simulated breath test data")
library(testthat)
test_that("Invalid parameters throw", {
  expect_error(simulate_breathtest_data(m_std = -1))
  expect_error(simulate_breathtest_data(k_std = -1))
  expect_error(simulate_breathtest_data(beta_std = -1))

  expect_error(simulate_breathtest_data(m_std = 20))
  expect_error(simulate_breathtest_data(k_std = 0.1))
  expect_error(simulate_breathtest_data(beta_std = 1))

  expect_error(simulate_breathtest_data(step_minute = 100))
})  

test_that("Dubious parameter give warning", {
  expect_warning(simulate_breathtest_data(noise = 0))
  expect_warning(simulate_breathtest_data(student_t_df = 1.5))
})  


test_that("Valid parameters return valid data", {
  d = simulate_breathtest_data(seed = 4711)
  expect_is(d, "list")
  expect_equal(names(d), c("record", "data"))
  expect_equal(nrow(d$record), 10)
  expect_equal(nrow(d$data), 110)
  expect_match(comment(d$data), "Gaussian")
})

test_that("Fewer data with missing values", {
  d = simulate_breathtest_data(missing = 0.1, seed = 4711)
  expect_equal(nrow(d$data), 99)
  expect_match(comment(d$data), "10%")
})

test_that("Valid student_t", {
  d = suppressWarnings(
    simulate_breathtest_data(student_t_df = 1.2, seed = 4711))
  expect_match(comment(d$data),"Student-t 2")
})

