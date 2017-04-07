context("Plot functions for breathtestfit class")

nlayers <- function(x) length(ggplot_build(x)$data)

test_that("Plot layers match expectations",{
  # With seed = 100, rec_10 fails nls-fit
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  x = nls_fit(data)
  p = plot(x)
  expect_gt(max(layer_data(p)$y), 25)
  expect_equal(nlayers(p), 2)   
  expect_equal(length(p), 9)   
  expect_equal(nlevels(layer_data(p)$PANEL), 10)
})

test_that("Plot multiple groups",{
  # With seed = 100, rec_10 fails nls-fit
  data = list(
    A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
    B = simulate_breathtest_data(n_records = 4, seed = 187)$data 
  )
  x = nls_fit(cleanup_data(data))
  p = plot(x)
  expect_equal(nlayers(p), 3)   
  expect_equal(length(p), 9)   
  expect_equal(nlevels(layer_data(p)$PANEL), 6)
})
