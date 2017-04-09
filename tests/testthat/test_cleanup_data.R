context("Cleanup data")

test_that("pdr is made numeric, remove gradient", {
  minute = seq(0,100, by = 10)
  # pdr has a gradient
  data = data.frame(minute, 
         pdr = exp_beta(minute, dose = 100, m = 30,  k = 0.01, beta = 2))
  expect_false(is.vector(data$pdr))
  data1 = cleanup_data(data)
  expect_true(is.vector(data1$pdr))
  expect_equal(names(data1), c("patient_id", "group", "minute","pdr"))
})


test_that("Two correctly named columns are are dummy filled and value at t=0 is corrected", {
  data = simulate_breathtest_data(1,)$data[,c("minute", "pdr")]  
  data1 = cleanup_data(data)
  # First row is changes
  expect_equal(data[-1,], data1[-1,3:4])  
  expect_equal(data1$minute[1], 0.01) # Slightly shifted
  expect_equal(names(data1), c("patient_id", "group", "minute","pdr"))
})

test_that("Incorectly names columns are renamed", {
  data = simulate_breathtest_data(1,)$data[,c("minute", "pdr")]  
  names(data) = c("a","b")
  data1 = cleanup_data(data)
  expect_equal(names(data1), c("patient_id", "group", "minute","pdr"))
})

test_that("Columns without names are renamed", {
  data = simulate_breathtest_data(1,)$data[,c("minute", "pdr")]  
  names(data) = NULL
  data1 = cleanup_data(data)
  expect_equal(names(data1), c("patient_id", "group", "minute","pdr"))
  expect_is(data1, "tbl_df")
})

test_that("Matrix is converted to data frame", {
  data = simulate_breathtest_data(1,)$data[,c("minute", "pdr")]  
  data1 = cleanup_data(as.matrix(data))
  expect_equal(names(data1), c("patient_id", "group", "minute","pdr"))
  expect_is(data1, "tbl_df")
})

test_that("Matrix with more than 2 columns not accepted", {
  data = matrix(rnorm(30), ncol = 3)
  expect_error(cleanup_data(data), "Use a data frame")
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
  d = cleanup_data(data)
  expect_equal(nrow(d), 22)
  names(data)[2]  = "grp"
  expect_error(cleanup_data(data), "must be named patient_id, group")
})

test_that("Columns must be numeric", {
  data = simulate_breathtest_data(n_records = 2)$data[,c("patient_id", "minute", "pdr")]
  data$minute = "A"
  expect_error(cleanup_data(data), "numeric")
})  

test_that("A list of data frames is concatenated, comments are concatenated", {
  data = simulate_breathtest_data(n_records = 2)$data
  data$group = "A"
  data = data[,c("patient_id", "group", "minute", "pdr")]
  data1 = data
  data1$group = "B"
  # Add comment  
  comment(data) = "A"
  comment(data1) = "B"
  data = list(data1 = data, data2 = data1)
  d = cleanup_data(data)
  expect_identical(comment(d), "A\nB\n")
  expect_is(d, "tbl")
  expect_equal(nrow(d), 44)
  expect_equal(ncol(d), 4)
})  



test_that("Same data used twice in list raises error", {
  data = simulate_breathtest_data(n_records = 2)$data
  data$group = "A"
  data = data[,c("patient_id", "group", "minute", "pdr")]
  expect_error(cleanup_data(list(data, data)), "twice")
})  

test_that("Same data used twice in data frame raises error", {
  data = simulate_breathtest_data(n_records = 2)$data
  data$group = "A"
  data = data[,c("patient_id", "group", "minute", "pdr")]
  data = rbind(data, data)
  expect_error(cleanup_data(data), "twice")
})  


test_that("data from BreathId device is accepted as input", {
  filename = system.file("extdata", "350_20043_0_GER.txt", 
                         package = "breathtestcore")
  data = read_breathid(filename)
  expect_silent(cleanup_data(data))
})

test_that("Composite data from Iris device is accepted as input", {
  filename = system.file("extdata", "IrisMulti.TXT", package = "breathtestcore")
  data = read_iris(filename)
  expect_silent(cleanup_data(data))
})

test_that("CSV data from Iris device is accepted as input", {
  filename = system.file("extdata", "IrisCSV.TXT", package = "breathtestcore")
  data = read_iris_csv(filename)
  expect_silent(cleanup_data(data))
})


test_that("list of breathtest_data from a common format is accepted as input", {
  f1 = system.file("extdata", "350_20043_0_GER.txt", 
                   package = "breathtestcore")
  f2 = system.file("extdata", "350_20023_0_GERWithNan.txt", 
                   package = "breathtestcore")
  data = list(read_breathid(f1), read_breathid(f2)) 
  d = cleanup_data(data)
  # expect dummy group when passing unnamed list
  expect_equal(unique(d$group), "A")
  expect_equal(nrow(d), 136)
  expect_equal(unique(d$patient_id), c("350_20043_0_GER", "350_20023_0_GERWithNan"))
})

test_that("list of breathtest_data of different formats is accepted as input", {
  f1 = system.file("extdata", "350_20043_0_GER.txt", package = "breathtestcore")
  f2 = system.file("extdata", "IrisMulti.TXT", package = "breathtestcore")
  f3 = system.file("extdata", "IrisCSV.TXT", package = "breathtestcore")
  data = list(A = read_breathid(f1), B = read_iris(f2), C = read_iris_csv(f3)) 
  d = cleanup_data(data)
  # When no name is given, letters are given to group
  expect_equal(unique(d$group), c("A", "B", "C"))
  expect_equal(nrow(d), 115)
  expect_equal(unique(d$patient_id), c("350_20043_0_GER", "1871960", "123456"))
})


