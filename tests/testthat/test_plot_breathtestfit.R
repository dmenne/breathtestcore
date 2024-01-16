nlayers <- function(x) length(ggplot_build(x)$data)

test_that("Plot layers match expectations",{
  # With seed = 100, rec_10 fails nls-fit
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  x = nls_fit(data)
  p = plot(x, point_size = 1) # Also tests case of given point size
  expect_s3_class(p, "ggplot")
  expect_gt(max(layer_data(p)$y), 25)
  expect_equal(nlayers(p), 4)   
  expect_equal(length(p), length(ggplot()))   
  expect_equal(nlevels(layer_data(p)$PANEL), 10)
})

test_that("Failed nlme fit plots data only", {
  data = cleanup_data(simulate_breathtest_data(seed = 100)$data)
  fit = nlme_fit(data)
  expect_null(fit$coef)
  p = plot(fit)
  expect_s3_class(p, "ggplot")
  expect_equal(nlayers(p), 1)   
})

test_that("Successful nlme fit plots data and fit", {
  data = cleanup_data(simulate_breathtest_data()$data)
  fit = nlme_fit(data)
  p = plot(fit)
  expect_s3_class(p, "ggplot")
  expect_equal(nlayers(p), 4)   
})

test_that("Plot multiple groups with repeats",{
  # With seed = 100, rec_10 fails nls-fit
  data = list(
    A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
    B = simulate_breathtest_data(n_records = 4, seed = 187)$data 
  )
  d = cleanup_data(data)
  x = nls_fit(d)
  p = plot(x)
  expect_s3_class(p, "ggplot")
  expect_equal(nlayers(p), 4)   
  expect_equal(length(p), length(ggplot()))   
  expect_equal(nlevels(layer_data(p)$PANEL), 6)
})

test_that("Plot multiple groups without repeats",{
  # With seed = 100, rec_10 fails nls-fit
  skip_on_cran()
  data = list(
    A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
    B = simulate_breathtest_data(n_records = 4, seed = 187)$data,
    C = simulate_breathtest_data(n_records = 3, seed = 100)$data,
    D = simulate_breathtest_data(n_records = 2, seed = 187)$data
  )
  # Make it a randomized design without repeats
  data$B$patient_id = paste0("b",data$B$patient_id)
  data$C$patient_id = paste0("c",data$C$patient_id)
  data$D$patient_id = paste0("d",data$D$patient_id)
  d = cleanup_data(data)
  x = nls_fit(d)
  p = plot(x)
  expect_s3_class(p, "ggplot")
  expect_equal(nlayers(p), 4)   
  expect_equal(length(p), length(ggplot()))   
  expect_equal(nlevels(layer_data(p)$PANEL), 15)
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
  expect_s3_class(p, "ggplot")
  expect_equal(nlayers(p), 1)   
  expect_equal(length(p), length(ggplot()))   
  expect_equal(nlevels(layer_data(p)$PANEL), 6)
})


test_that("Can plot a breathtestdata class",{
  data = list(
    A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
    B = simulate_breathtest_data(n_records = 4, seed = 187)$data 
  )
  d = null_fit(cleanup_data(data))
  p = plot(d)
  expect_s3_class(p, "ggplot")
})
