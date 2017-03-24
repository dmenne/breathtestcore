context("Cleanup data")

test_that("Two correctly names columns are returned unchanged", {
  data = simulate_breathtest_data(1,)$data[,c("minute", "pdr")]  
  data1 = cleanup_data(data)
  expect_equal(data, data1)  
})

test_that("Incorectly names columns are renamed", {
  data = simulate_breathtest_data(1,)$data[,c("minute", "pdr")]  
  names(data) = c("a","b")
  data1 = cleanup_data(data)
  expect_equal(names(data1), c("minute","pdr"))
})


test_that("Suspect missing patient column if multiple pdr with same minute", {
  data = simulate_breathtest_data(n_records = 2)$data[,c("minute", "pdr")]  
  expect_error(cleanup_data(data), "multiple")
})


test_that("When there are three columns, must be named correctly", {
  data = simulate_breathtest_data(n_records = 2)$data[,c("patient_id", "minute", "pdr")]
  names(data)[1] = "pat_id"
  expect_error(cleanup_data(data), "must be named")
})

test_that("When there are four columns, must be named correctly", {
  data = simulate_breathtest_data(n_records = 2)$data[,c("patient_id", "minute", "pdr")]
  data$group = "A"
  data = data[,c("patient_id", "group", "minute", "pdr")]
  expect_silent(cleanup_data(data))
  names(data)[2]  = "grp"
  expect_error(cleanup_data(data), "must be named patient_id, group")
})

test_that("Columns must be numeric", {
  data = simulate_breathtest_data(n_records = 2)$data[,c("patient_id", "minute", "pdr")]
  data$minute = "A"
  expect_error(cleanup_data(data), "numeric")
})  

test_that("A list of data frames is concatenated", {
  data = simulate_breathtest_data(n_records = 2)$data
  data$group = "A"
  data = data[,c("patient_id", "group", "minute", "pdr")]
  data1 = data
  data1$group = "B"
  
  data = list(data1 = data, data2 = data1)
  d = cleanup_data(data)
  expect_is(d, "tbl")
  expect_equal(nrow(d), 44)
  expect_equal(ncol(d), 4)
})  





