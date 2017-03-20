context("recognize breathtest file format")
d13file = function(filename) {
  system.file("extdata", filename, package = "breathtestcore")
}
test_that("Correct file format returned" , {
  expect_equal(breathtest_read_function(d13file("350_20043_0_GER.txt")), read_breathid)
  expect_equal(breathtest_read_function(d13file("IrisCSV.TXT")), read_iris_csv)
  expect_equal(breathtest_read_function(d13file("IrisMulti.TXT")), read_iris)
})
  
test_that("Error cases throw or return null" , {
  expect_error(breathtest_read_function(d13file("a.TXT")),"exist")
  expect_null(breathtest_read_function(text = "This is not a breathtest file"))
})
