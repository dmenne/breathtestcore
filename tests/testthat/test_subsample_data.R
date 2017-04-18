context("Subsampling of data")

test_that("Subsample uses denser steps early and wider steps later",{
  d = simulate_breathtest_data(n_records = 1, first_minute = 1, step_minute = 1)$data
  d1 = subsample_data(cleanup_data(d), 15)
  tt = as.integer(table(diff(d1$minute)))
  expect_equal(tt, c(6,3,1,4))
})