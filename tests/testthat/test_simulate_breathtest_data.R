context("Simulated breath test data")

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


test_that("Valid parameters without std return valid data with cov-matrix", {
  d = simulate_breathtest_data(seed = 4711)
  expect_is(d, "simulated_breathtest_data")
  expect_is(d, "list")
  expect_equal(names(d), c("record", "data"))
  expect_equal(nrow(d$record), 10)
  expect_equal(names(d$record), c("patient_id","m","k","beta","t50_maes_ghoos"))
  expect_equal(nrow(d$data), 110)
  expect_equal(names(d$data), c("patient_id","minute","pdr"))
  expect_match(comment(d$data), "Gaussian")
  expect_match(comment(d$data), "cov-matrix")
  cov = attr(d$record, "cov")
  expect_is(cov, "matrix")
  expect_equal(rownames(cov), c("m","k", "beta"))
})


test_that("Cov matrix not used when n_records<= 3 ", {
  d = simulate_breathtest_data(n_records = 1, seed = 4711)
  expect_equal(nrow(d$record), 1)
  expect_is(d, "simulated_breathtest_data")
  expect_is(d, "list")
  expect_equal(names(d), c("record", "data"))
  cov = attr(d$record, "cov")
  expect_null(cov)
  d = simulate_breathtest_data(n_records = 3, seed = 4711)
  expect_equal(nrow(d$record), 3)
  cov = attr(d$record, "cov")
  expect_null(cov)
})  

test_that("Valid parameters with one std return valid data without", {
  d = simulate_breathtest_data(m_std = 0., seed = 4711)
  expect_match(comment(d$data), ", 0%")
  expect_null(attr(d$record, "cov"))
})

test_that("Fewer data with missing values", {
  d = simulate_breathtest_data(missing = 0.1, seed = 4711)
  expect_equal(nrow(d$data), 99)
  expect_match(comment(d$data), "10%")
  expect_match(comment(d$data), "cov-matrix")
})

test_that("Warning when requesting too many missing", {
  expect_warning(simulate_breathtest_data(missing = 0.8, seed = 4711), "Fraction of")
})

test_that("Valid student_t", {
  d = suppressWarnings(
    simulate_breathtest_data(student_t_df = 1.2, seed = 4711))
  expect_match(comment(d$data),"Student-t 2")
})


