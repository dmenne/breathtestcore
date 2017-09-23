context("Cleanup data")
expected_columns = c("patient_id", "group", "minute","pdr")

test_that("pdr is made numeric, remove gradient", {
  minute = seq(0,100, by = 10)
  # pdr has a gradient
  data = data.frame(minute, 
         pdr = exp_beta(minute, dose = 100, m = 30,  k = 0.01, beta = 2))
  expect_false(is.vector(data$pdr))
  data1 = cleanup_data(data)
  expect_true(is.vector(data1$pdr))
  expect_equal(names(data1), expected_columns)
})

test_that("Duplicates are removed", {
  data = simulate_breathtest_data()$data
  data = rbind(data,data)
  data1 = cleanup_data(data)
  expect_identical(2L*nrow(data1), nrow(data))
})



test_that("Two correctly named columns are are dummy filled and value at t=0 is corrected", {
  # Here we pass $data; see next test for alternative
  data = simulate_breathtest_data(1, first_minute = 0)$data[,c("minute", "pdr")]  
  data1 = cleanup_data(data)
  # First row is changes
  expect_equal(data[-1,], data1[-1,3:4])  
  expect_equal(data1$minute[1], 0.01) # Slightly shifted
  expect_equal(names(data1), expected_columns)
})

test_that("Can pass simulate_breathtest_data() to cleanup_data() without $data", {
  data = simulate_breathtest_data() # no $data here
  data1 = cleanup_data(data)
  expect_equal(names(data1), expected_columns)
})

test_that("Can pass list of simulate_breathtest_data() to cleanup_data() without $data", {
  data = list(anton = simulate_breathtest_data(3), 
              bertha = simulate_breathtest_data(4))
  data1 = cleanup_data(data)
  expect_equal(names(data1), c("patient_id", "group", "minute","pdr"))
  expect_equal(unique(data1$group), c("anton","bertha"))
})

test_that("Spaces in patient_id and group are removed", {

  data = list(anton = simulate_breathtest_data(3)$data,
               bertha = simulate_breathtest_data(4)$data)
  data = cleanup_data(data)
  data$group = paste0(data$group, " dd xx ")
  data$patient_id = paste0(data$patient_id, " dd ")
  
  data1 = cleanup_data(data)
  expect_equal(unique(data1$group), c("anton_dd_xx","bertha_dd_xx"))
  expect_equal(unique(data1$patient_id), 
               c("rec_01_dd", "rec_02_dd", "rec_03_dd", "rec_04_dd"))
})

test_that("Incorectly named columns are renamed", {
  data = simulate_breathtest_data(1)$data[,c("minute", "pdr")]  
  names(data) = c("a","b")
  data1 = cleanup_data(data)
  expect_equal(names(data1), expected_columns)
})

test_that("Columns without names are renamed", {
  data = simulate_breathtest_data(1)$data[,c("minute", "pdr")]  
  names(data) = NULL
  data1 = cleanup_data(data)
  expect_equal(names(data1), expected_columns)
  expect_is(data1, "tbl_df")
})

test_that("Matrix is converted to data frame", {
  data = simulate_breathtest_data(1)$data[,c("minute", "pdr")]  
  data1 = cleanup_data(as.matrix(data))
  expect_equal(names(data1), expected_columns)
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

test_that("Group can be missing when there are no colliding events", {
  data = simulate_breathtest_data(n_records = 2)$data[,c("patient_id", "minute", "pdr")]  
  data1 = cleanup_data(data)
  expect_is(data1, "tbl_df")
  # Jitter minutes a bit, so that table can report zeroes
  data$minute = data$minute + base::sample(seq(-0.2,0.2, by = 0.1), nrow(data), replace = TRUE)
  data1 = cleanup_data(data)
  expect_is(data1, "tbl_df")
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
  data0 = simulate_breathtest_data(n_records = 2)$data
  data0$group = "A"
  data0 = data0[,c("patient_id", "group", "minute", "pdr")]
  data1 = data0
  data1$group = "B"
  

  # Without comments
  data = list(data0 = data0, data1 = data1)
  d = cleanup_data(data)
  expect_null(comment(d))
  expect_is(d, "tbl")
  expect_equal(nrow(d), 44)
  expect_equal(ncol(d), 4)
  
  # Add one comment  
  comment(data0) = "A"
  data = list(data0 = data0, data1 = data1)
  d = cleanup_data(data)
  expect_identical(comment(d), "A")

  # Add two different comments
  comment(data0) = "A"
  comment(data1) = "B"
  data = list(data0 = data0, data1 = data1)
  d = cleanup_data(data)
  expect_identical(comment(d), "A\nB")

  # Add three comments with repeats
  data2 = data0
  data2$group = "C"
  comment(data0) = "A"
  comment(data1) = "B"
  comment(data2) = "A"
  data = list(data0 = data0, data1 = data1, data2 = data2)
  d = cleanup_data(data)
  expect_identical(comment(d), "A\nB")
})  



test_that("Same data used twice in list raises error", {
  data = simulate_breathtest_data(n_records = 2)$data
  data$group = "A"
  data = data[,c("patient_id", "group", "minute", "pdr")]
  expect_warning(cleanup_data(list(data, data)), "twice")
})  

test_that("data from BreathId device is accepted as input", {
  filename = btcore_file("350_20043_0_GER.txt") 
  data = read_breathid(filename)
  expect_silent(cleanup_data(data))
})

test_that("Composite data from Iris device is accepted as input", {
  filename = btcore_file("IrisMulti.TXT")
  data = read_iris(filename)
  expect_silent(cleanup_data(data))
})

test_that("CSV data from Iris device is accepted as input", {
  filename = btcore_file("IrisCSV.TXT")
  data = read_iris_csv(filename)
  expect_silent(cleanup_data(data))
})

test_that("BreathID XML format is accepted as input", {
  filename = btcore_file("NewBreathID_multiple.xml")
  data = read_breathid_xml(filename)
  expect_silent(cleanup_data(data))
})


test_that("list of breathtest_data from a common format is accepted as input", {
  f1 = btcore_file("350_20043_0_GER.txt") 
  f2 = btcore_file("350_20023_0_GERWithNan.txt") 
  data = list(read_breathid(f1), read_breathid(f2)) 
  d = cleanup_data(data)
  # expect dummy group when passing unnamed list
  expect_equal(unique(d$group), c("A", "B")) # Default names
  expect_equal(nrow(d), 136)
  expect_equal(unique(d$patient_id), c("350_20043_0_GER", "350_20023_0_GERWithNan"))
})

test_that("list of breathtest_data of different formats is accepted as input", {
  f1 = btcore_file("350_20043_0_GER.txt")
  f2 = btcore_file("IrisMulti.TXT")
  f3 = btcore_file("IrisCSV.TXT")
  data = list(anton = read_breathid(f1), #
              bertha = read_iris(f2), 
              caesar = read_iris_csv(f3)) 
  d = cleanup_data(data)
  # When no name is given, letters are given to group
  expect_equal(unique(d$group), c("anton", "bertha", "caesar"))
  expect_equal(nrow(d), 115)
  expect_equal(unique(d$patient_id), c("350_20043_0_GER", "1871960", "123456"))

  # Force use of filename for patient_id
  d = cleanup_data(data, use_filename_as_patient_id = TRUE)
  # When no name is given, letters are given to group
  expect_equal(unique(d$group), c("anton", "bertha", "caesar"))
  expect_equal(nrow(d), 115)
  expect_equal(unique(d$patient_id), c("350_20043_0_GER", "IrisMulti", "IrisCSV"))
})

test_that("Single XML is accepted as input", {
  f1 = btcore_file("NewBreathID_multiple.xml")
  data = read_breathid_xml(f1)
  d = cleanup_data(data)
})
  
test_that("list of breathtest_data with XML is accepted as input", {
  f1 = btcore_file("350_20043_0_GER.txt")
  f2 = btcore_file("NewBreathID_multiple.xml")
  data = list(anton = read_breathid(f1), bertha = read_breathid_xml(f2)) 
  d = cleanup_data(data)
  # When no name is given, letters are given to group
  ## TODO, not so good behaviour, think of getting bertha_A instead
  expect_equal(unique(d$group), c("anton", "A", "B", "C"))
  expect_equal(nrow(d), 152)
  expect_equal(unique(d$patient_id), 
               c("350_20043_0_GER", "07951400", "10727002",  "10650692" ))
})

