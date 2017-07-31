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


test_that("USZ 13c a data have the correct columns",{
  data("usz_13c_a")  
  expect_equal(names(usz_13c_a), c("patient_id","group","minute","pdr"))
  # Check that there are no duplicates
  expect_identical(nrow(usz_13c_a[duplicated(usz_13c_a[,-4]),]),0L)
  library(dplyr)
  d = usz_13c_a %>% 
       cleanup_data() %>% 
       nlme_fit()
  expect_match(comment(d$data),"no successful fit with nlme")
})

test_that("USZ 13c d data have the correct columns",{
  data("usz_13c_d")  
  expect_equal(names(usz_13c_d), c("patient_id","group","minute","pdr"))
  # Check that there are no duplicates
  expect_identical(nrow(usz_13c_d[duplicated(usz_13c_d[,-4]),]),0L)
})
