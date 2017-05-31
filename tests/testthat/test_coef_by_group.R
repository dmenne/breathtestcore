context("Coeffients by group")

library(dplyr)
data("usz_13c")
data = usz_13c %>%
  dplyr::filter( patient_id %in%
  c("norm_001", "norm_002", "norm_003", "norm_004", "pat_001", "pat_002","pat_003")) %>%
  cleanup_data()
fit = nls_fit(data)

test_that("Result with default parameters is tbl_df with required columns",{
  cf = coef_by_group(fit)
  expect_is(cf, "tbl_df")
  expect_identical(ncol(cf), 7L)
  expect_equal(names(cf), c("parameter", "method", "group", "estimate", "conf.low", 
                 "conf.high", "diff_group"))
  expect_identical(nrow(cf), 27L)
  expect_identical(unique(cf$diff_group), c("a", "ab", "b", "c"))
  expect_equal(unique(cf$group),
     c("liquid_normal", "patient", "solid_normal"))
})

test_that("Options digits is served",{
  options(digits = 4)
  cf = coef_by_group(fit)
  expect_is(cf, "tbl_df")
  expect_identical(max(nchar(paste(cf[1,4:6]))),5L)
})


test_that("Fit must be of class breathtestfit",{
  expect_error(coef_by_group(NULL))
})

test_that("nlme_fit can be used to compute coefficients",{
  skip_on_cran()
  fit = nlme_fit(data)
  cf = coef_by_group(fit)
  expect_is(cf, "tbl_df")
  expect_identical(ncol(cf), 7L)
  expect_equal(names(cf), c("parameter", "method", "group", "estimate", "conf.low", 
                            "conf.high", "diff_group"))
})

test_that("Fit of single curve returns valid data", {
  data = usz_13c %>%
    dplyr::filter( patient_id == "pat_001") %>% 
    cleanup_data()
  fit = nls_fit(data)
  cf = coef_by_group(fit)
  expect_identical(ncol(cf), 7L)
  expect_identical(nrow(cf), 9L)
  expect_equal(cf$conf.low, rep(NA, 9L))
  expect_equal(cf$conf.high, rep(NA, 9L))
  
})