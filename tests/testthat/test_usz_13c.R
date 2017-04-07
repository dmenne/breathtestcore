context("Data")

test_that("USZ data have the correct columns",{
  data("usz_13c")  
  expect_equal(names(usz_13c), c("patient_id","group","minute","pdr"))
  expect_equal(nrow(usz_13c), 15574)
  expect_match(comment(usz_13c), "Zurich")
  nls_bad = attr(usz_13c, "nls_bad")
  # check for one known case
  expect_match(nls_bad, "pat_012")
})