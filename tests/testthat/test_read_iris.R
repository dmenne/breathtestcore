context("13C read IRIS composite files test")
d13File = function(filename){
  system.file("extdata", filename, package = "breathtestcore")  
}

test_that("read_iris returns valid data set",{
  filename = d13File("IrisMulti.TXT")
  f = read_iris(filename)
  expect_is(f,"breathtest_data")
  expect_equal(f$file_name, basename(filename))
  expect_equal(f$name,"V")
  expect_equal(f$first_name,"S")
  expect_equal(f$initials,"VS")
  expect_equal(f$patient_id,"1871960")
  expect_equal(nrow(f$data),14)
  expect_equal(ncol(f$data),3)
})

test_that("read_iris returns valid data set when values are negative",{
  filename = d13File("IrisNegativeValues.TXT")
  f = read_iris(filename)
  expect_is(f,"breathtest_data")
  expect_equal(nrow(f$data),12)
  expect_equal(ncol(f$data),3)
  expect_true(all(f$data$dob >= -10))
})

test_that("read_iris returns valid data set when weight/height is zero",{
  filename = d13File("IrisZeroWeight.TXT")
  f = read_iris(filename)
  expect_is(f,"breathtest_data")
  expect_true(is.na(f$weight))
  expect_true(is.na(f$height))
  expect_equal(nrow(f$data),14)
  expect_equal(ncol(f$data),3)
})


test_that("read_iris of CSV file throws",{
  filename = d13File("IrisCSV.TXT")
  expect_error( read_iris(filename),"valid IRIS")
})

