context("Data")

test_that("USZ data have the correct columns",{
  data("usz_13c")  
  expect_equal(names(usz_13c), c("patient_id","group","minute","pdr"))
  expect_identical(nrow(usz_13c), 15561L)
  expect_match(comment(usz_13c), "Zurich")
  # Check that there are no duplicates
  expect_identical(nrow(usz_13c[duplicated(usz_13c[,-4]),]),0L)
  nls_bad = attr(usz_13c, "nls_bad")
  # check for one known case
  expect_match(nls_bad, "pat_013")
})