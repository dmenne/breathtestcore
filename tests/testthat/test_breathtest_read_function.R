library(purrr)
filename = "NewBreathID_multiple.xml"
read_function = read_breathid_xml

check_and_read = function(filename, read_function){
  file = btcore_file(filename)
  expect_true(file.exists(file))
  f = breathtest_read_function(file)
  expect_equal(f, read_function)
  bt = f(file)
  if (inherits(bt, "breathtest_data_list")) {
    expect_true(all(map_lgl(bt, function(x) {class(x) == "breathtest_data"})))
    bt = bt[[1]]  
  } else if (inherits(bt, "breathtest_data")) {
    expect_s3_class(bt, "breathtest_data")
    expect_gt(nrow(bt$data), 1)
  }
}

test_that("btcore_file without arguments returns all files in sample directory",{
  file = btcore_file()  
  expect_type(file, "character")
  expect_gt(length(file), 21L)
})

test_that("Correct file format returned and files correctly read" , {
  check_and_read("NewBreathID_multiple.xml", read_breathid_xml)
  check_and_read("350_20043_1_GER.txt", read_breathid)
  check_and_read("IrisCSV.TXT", read_iris_csv)
  check_and_read("IrisMulti.TXT", read_iris)
})
  
test_that("Wrong formats throws exception or return null" , {
  expect_error(breathtest_read_function(btcore_file("a.TXT")),"exist")
  expect_null(breathtest_read_function(text = "This is not a breathtest file"))
})

read_any_file = function(filename){
  file = btcore_file(filename)
  expect_true(file.exists(file))
  breathtest_read_function(file)(file)
}


test_that("Valid files with datea errors throw exception" , {
  expect_error(read_any_file("IrisCSV_MissingColumn.TXT"), "unexpected 12 columns")
  expect_error(read_any_file("IrisCSV_invalidValues.TXT"), "Invalid or missing PDR/DOB data")
})
