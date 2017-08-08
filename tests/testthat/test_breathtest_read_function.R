context("recognize breathtest file format")
d13file = function(filename) {
  system.file("extdata", filename, package = "breathtestcore")
}

check_and_read = function(filename, read_function){
  file = d13file(filename)
  expect_true(file.exists(file))
  f = breathtest_read_function(file)
  expect_equal(f, read_function)
  bt = f(file)
  expect_is(bt, "breathtest_data")
  expect_gt(nrow(bt$data), 1)
}

test_that("Correct file format returned and files correctly read" , {
  check_and_read("350_20043_1_GER.txt", read_breathid)
  check_and_read("IrisCSV.TXT", read_iris_csv)
  check_and_read("IrisMulti.TXT", read_iris)
})
  
test_that("Wrong formats throw or return null" , {
  expect_error(breathtest_read_function(d13file("a.TXT")),"exist")
  expect_null(breathtest_read_function(text = "This is not a breathtest file"))
})

read_any_file = function(filename){
  file = d13file(filename)
  expect_true(file.exists(file))
  f = breathtest_read_function(file)(file)
}


test_that("Valid files with datea errors throw exception" , {
  expect_error(read_any_file("IrisCSV_MissingColumn.TXT"), "unexpected 12 columns")
  expect_error(read_any_file("IrisCSV_invalidValues.TXT"), "Invalid PDR/DOB data")
})
