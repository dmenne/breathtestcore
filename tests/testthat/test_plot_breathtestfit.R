context("Plot functions for breathtestfit and breathtestdata class")

nlayers <- function(x) length(ggplot_build(x)$data)

test_that("Plot layers match expectations",{
  # With seed = 100, rec_10 fails nls-fit
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  x = nls_fit(data)
  p = plot(x)
  expect_is(p, "ggplot")
  expect_gt(max(layer_data(p)$y), 25)
  expect_equal(nlayers(p), 3)   
  expect_equal(length(p), 9)   
  expect_equal(nlevels(layer_data(p)$PANEL), 10)
})

test_that("Failed nlme fit plots data only", {
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  fit = nlme_fit(data)
  expect_null(fit$coef)
  p = plot(fit)
  expect_is(p, "ggplot")
  expect_equal(nlayers(p), 1)   
})

test_that("Successful nlme fit plots data and fit", {
  data = cleanup_data(simulate_breathtest_data()$data)
  fit = nlme_fit(data)
  p = plot(fit)
  expect_is(p, "ggplot")
  expect_equal(nlayers(p), 3)   
})

test_that("Plot multiple groups",{
  # With seed = 100, rec_10 fails nls-fit
  data = list(
    A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
    B = simulate_breathtest_data(n_records = 4, seed = 187)$data 
  )
  d = cleanup_data(data)
  x = nls_fit(d)
  p = plot(x)
  expect_is(p, "ggplot")
  expect_equal(nlayers(p), 3)   
  expect_equal(length(p), 9)   
  expect_equal(nlevels(layer_data(p)$PANEL), 6)
})

test_that("Plot multiple groups data only (no fit)",{
  data = list(
    A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
    B = simulate_breathtest_data(n_records = 4, seed = 187)$data 
  )
  d = cleanup_data(data)
  x = null_fit(d) # mainly converts to class breathtestnullfit/breathtestfit
  expect_equal(class(x), c("breathtestnullfit", "breathtestfit"))
  p = plot(x) # Plots raw data only
  expect_is(p, "ggplot")
  expect_equal(nlayers(p), 1)   
  expect_equal(length(p), 9)   
  expect_equal(nlevels(layer_data(p)$PANEL), 6)
})


test_that("Can plot a breathtestdata class",{
  data = list(
    A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
    B = simulate_breathtest_data(n_records = 4, seed = 187)$data 
  )
  d = null_fit(cleanup_data(data))
  p = plot(d)
  expect_is(p, "ggplot")
})