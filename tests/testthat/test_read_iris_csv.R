test_that("extract_id returns valid id" , {
  expect_equal(extract_id("123456"), "123456")
  expect_equal(extract_id("123-456"), "123_456")
  expect_equal(extract_id("KEK-ZH-Nr.2013-1234"), "2013_1234")
  expect_equal(extract_id("Las4Dd5 .f lkj"), "las4dd5_f_lkj")
})

test_that("read_iris_csv returns valid data set", {
  filename = btcore_file("IrisCSV.TXT")
  f = read_iris_csv(filename)
  expect_s3_class(f, "breathtest_data")
  expect_equal(f$file_name, basename(filename))
  expect_equal(f$name, "Einstein")
  expect_equal(f$first_name, "Albert")
  expect_equal(f$initials, "EA")
  expect_equal(f$patient_id, "123456")
  expect_equal(nrow(f$data), 14)
  expect_equal(ncol(f$data), 3)
  expect_equal(f$study, "GE FEST")
})

test_that("read_iris_csv from text and from file give almost the same results", {
  filename = btcore_file("IrisCSV.TXT")
  f = read_iris_csv(filename)
  text = readLines(filename)
  f1 = read_iris_csv(text = readLines(filename))
  expect_equal(f1$file_name, "from text")
  expect_equal(f$file_name, basename(filename))
  f$file_name = "from text"
  expect_equal(f, f1)
})


test_that("read_iris of composite file throws",{
  filename = btcore_file("IrisMulti.TXT")
  expect_error( read_iris_csv(filename),"valid IRIS")
})

test_that("read_iris throws when column is missing",{
  filename = btcore_file("IrisCSV_MissingColumn.TXT")
  expect_error( read_iris_csv(filename),"unexpected")
})  


test_that("read_iris_csv cleans up special characters", {
  filename = btcore_file("IrisCSV_with_KEK.TXT")
  f = read_iris_csv(filename)
  expect_equal(f$patient_id, "2013_1234")
  skip_on_cran()  
  expect_equal(f$study, "GE-flüssig")
})

test_that("read_iris_csv raises error on short file", {
  filename = btcore_file("IrisCSVShort.TXT")
  expect_error(read_iris_csv(filename), "has only")
})

test_that("read_iris_csv raises error on invalid entries", {
  filename = btcore_file("IrisCSV_invalidValues.TXT")
  expect_error(read_iris_csv(filename), "Invalid")
})
