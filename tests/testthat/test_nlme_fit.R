context("Population fit with nlme")

test_that("Data that cannot be fitted with nls_list also fail with nlme", {
  # with this seed, cf[10] does not fit with nls_list
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  fit = expect_error(
    nlme_fit(data)
    , "PNLS step")
})  

