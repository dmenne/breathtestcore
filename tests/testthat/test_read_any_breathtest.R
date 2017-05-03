context("Read any breath test file")

test_that("Reading a single file returs a list",{
  file = system.file("extdata", "IrisCSV.TXT", package = "breathtestcore")
  bt = read_any_breathtest(file)
  expect_is(bt, "list")
  expect_equal(length(bt), 1)  
  expect_is(bt[[1]], "breathtest_data")
  expect_equal(bt[[1]]$patient_id, "123456")
})

test_that("Reading multiple files returns a list with multiple items",{
  files = c(
    system.file("extdata", "IrisCSV.TXT", package = "breathtestcore"),
    system.file("extdata", "350_20043_0_GER.txt", package = "breathtestcore"),
    system.file("extdata", "IrisMulti.TXT", package = "breathtestcore")
  )
  bt = read_any_breathtest(files)
  expect_is(bt, "list")
  expect_equal(length(bt), 3)  
  expect_equal(bt[[1]]$patient_id, "123456")
})

#'  bt = read_any_breathtest(files)
#'  str(bt, 1)
