context("13C read IRIS composite files test")

test_that("read_iris returns valid data set",{
  filename = btcore_file("IrisMulti.TXT")
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

test_that("read_iris from text and from file give almost the same results",{
  filename = btcore_file("IrisMulti.TXT")
  f = read_iris(filename)
  f1 = read_iris(text = readLines(filename)) 
  expect_equal(f1$file_name, "from text")
  expect_equal(f$file_name, basename(filename))
  f$file_name = "from text"  
  expect_equal(f, f1)
})


test_that("read_iris returns valid data set when values are negative",{
  filename = btcore_file("IrisNegativeValues.TXT")
  f = read_iris(filename)
  expect_is(f,"breathtest_data")
  expect_equal(nrow(f$data),12)
  expect_equal(ncol(f$data),3)
  expect_true(all(f$data$dob >= -10))
})

test_that("read_iris returns valid data set when weight/height is zero",{
  filename = btcore_file("IrisZeroWeight.TXT")
  f = read_iris(filename)
  expect_is(f,"breathtest_data")
  expect_equal(f$weight, 75)
  expect_equal(f$height, 180)
  expect_equal(nrow(f$data),14)
  expect_equal(ncol(f$data),3)
})

test_that("read_iris correctly ready names with umlaut",{
  filename = btcore_file("Iris_Umlaut.TXT")
  f = read_iris(filename)
  expect_is(f,"breathtest_data")
  expect_equal(f$name, "Rüdigör")
  expect_equal(f$first_name, "Özgur")
  expect_equal(f$initials, "RÖ")
})



test_that("read_iris of CSV file throws",{
  filename = btcore_file("IrisCSV.TXT")
  expect_error( read_iris(filename),"valid IRIS")
})

test_that("read_iris throws on non-existing file",{
  expect_error( read_iris("a.txt"),"not exist")
})  

