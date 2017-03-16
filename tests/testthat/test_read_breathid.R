context("13c read BreathID test")
d13file = function(filename) {
  system.file("extdata", filename, package = "breathtestcore")
}

test_that("read_breathid returns valid data set", {
  filename = d13file("350_20043_0_GER.txt")
  f = read_breathid(filename)
  expect_is(f, "breathtest_data")
  expect_equal(f$file_name, basename(filename))
  expect_equal(f$test_no, 20043)
  expect_equal(f$t50, 71.23)
  expect_equal(f$patient_id, "0")
  expect_equal(f$gender, "m")
  expect_equal(nrow(f$data), 87)
  expect_equal(ncol(f$data), 6)
  expect_true("cpdrfit" %in% names(f$data))
})

test_that("read_breathid from text and from file give almost the same results",{
  filename = d13file("350_20043_0_GER.txt")
  f = read_breathid(filename)
  f1 = read_breathid(text = readLines(filename)) 
  expect_equal(f1$file_name, "from text")
  expect_equal(f$file_name, basename(filename))
  f$file_name = "from text"  
  expect_equal(f, f1)
})


test_that("read_breathid on bad data file throws", {
  filename = d13file("350_20043_0_GERBadHeader.txt")
  expect_error(read_breathid(filename), "not a valid BreathID")
  filename = d13file("350_20043_0_GERNoData.txt")
  expect_error(read_breathid(filename), "does not contain PDR")
  filename = d13file("350_20043_0_GERNoT50.txt")
  expect_error(read_breathid(filename), "no <<T 1/2>> found")
})

test_that("read_breathid with NA returns valid data, without NA columns",{
  filename = d13file("350_20023_0_GERWithNan.txt")
  f = read_breathid(filename)
  expect_is(f, "breathtest_data")
  expect_true(!("cpdfit" %in% names(f$data)))
})

test_that("dob_to_pdr is not too far from what breathid says", {
  filename = d13file("350_20049_0_GERWithWeight.txt")
  bid = read_breathid(filename)
  bid$data$pdr1 = dob_to_pdr(bid$data$dob,
                             weight = bid$weight,
                             height = bid$height)
  expect_true(sqrt(var(bid$data$pdr1 - bid$data$pdr)) < 0.032)
})
