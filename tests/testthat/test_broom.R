library(testthat)
context("broom functions")

test_that("broom/tidy returns a simplified data set with only maes_ghoos t50", {
  data = cleanup_data(simulate_breathtest_data(seed=10)$data)
  fit = nls_fit(data)
  td = tidy(fit)
  expect_is(td, "tbl")
  expect_equal(names(td), c("patient_id", "group", "m", "k", "beta", "t50"))
  expect_equal(nrow(td), 10)
})


test_that("broom/augment returns predictions", {
  data = cleanup_data(simulate_breathtest_data()$data)
  fit = nls_fit(data)
  td = augment(fit)

  expect_equal(names(td), c("patient_id", "group", "minute","pdr","fitted"))
  expect_equal(nrow(td), 110)

  # Use spacing
  td = augment(fit, by = 5)
  expect_equal(names(td), c("patient_id", "group", "minute", "fitted"))
  expect_equal(nrow(td), 310)

  # Use vector of time values
  td = augment(fit, minute = c(0:9, seq(10, 150, by = 5)))
  expect_equal(names(td), c("patient_id", "group", "minute", "fitted"))
  expect_equal(nrow(td), 390)
})
