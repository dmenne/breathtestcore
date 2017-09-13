context("Individual curve fitting with nls")

test_that("Nice data return nice result", {
  d = simulate_breathtest_data(seed = 4711)   # default 10 records
  # This job is normally done by cleanup_data
  data = d$data %>% 
    mutate(
      group = rep(c("A", "B"), each = 55 ),
      minute = pmax(minute, 0.001)
    ) %>% 
    select(patient_id, group, minute, pdr)
  comment(data) = "comment"
  fit = nls_fit(data)
  expect_is(fit, "breathtestfit")
  expect_is(fit, "breathtestnlsfit")
  expect_identical(names(fit), c("coef", "data", "nls_fit"))
  expect_equal(comment(fit$data), "comment")
  expect_gt(sigma(fit), 0.5) # about 0.87
  cf = coef(fit)
  expect_is(cf, "data.frame")
  expect_equal(names(cf), c("patient_id", "group", "parameter", "method", "value"))
  expect_equal(nrow(cf), 90)
})

test_that("Nasty data return results with na", {
  # with this seed, cf[10] does not fit
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  fit = nls_fit(data)
  cf = coef(fit)
  expect_equal(nrow(cf), 81) # Last dropped
})  

rel_diff = function(d, cf, parameter){
  d1 = d$record[[parameter]]
  d2 = as.numeric(cf[cf["parameter"] == parameter,"value"])  
  abs(d2 - d1)/abs(d1)
}

test_that("Single record give valid result after passing through cleanup_data", {
  d = simulate_breathtest_data(n_records = 1, noise = 0.2, seed = 4711)
  data = cleanup_data(d$data)
  cf = coef(nls_fit(data))
  expect_is(cf, "data.frame")
  expect_equal(names(cf), c("patient_id", "group", "parameter", "method", "value"))
  expect_equal(nrow(cf), 9)
  expect_lt(rel_diff(d, cf, "m"), 0.02)
  expect_lt(rel_diff(d, cf, "beta"), 0.014)
  expect_lt(rel_diff(d, cf, "k"), 0.01)
})

test_that("Nonsense record", {
  d = simulate_breathtest_data(n_records = 2)
  d$data$pdr = 0
  data = cleanup_data(d$data)
  expect_error(nls_fit(data), "No valid fit")
})
