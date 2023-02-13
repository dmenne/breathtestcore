suppressPackageStartupMessages(library(dplyr))
data("usz_13c")
data = usz_13c %>%
  dplyr::filter( patient_id %in%
  c("norm_001", "norm_002", "norm_004", "norm_007", "pat_004", "pat_012", "pat_023")) %>%
  cleanup_data()
fit = nls_fit(data)
cm = comment(fit$data)

test_that("Result with default parameters is tbl_df with required columns",{
  cf = coef_by_group(fit)
  expect_s3_class(cf, "tbl_df")
  expect_s3_class(cf, "coef_by_group")
  expect_identical(ncol(cf), 7L)
  expect_equal(names(cf), c("parameter", "method", "group", "estimate", "conf.low", 
                 "conf.high", "diff_group"))
  expect_identical(nrow(cf), 36L)
  # The following test fails for multcomp 1.4-22
  #expect_identical(unique(cf$diff_group), c("a", "c", "b", "bc", "ab"))
  expect_equal(unique(cf$group),
     c("liquid_normal", "liquid_patient", "solid_normal", "solid_patient"))
  expect_equal(comment(cf), cm)
  
})

digs = function(x){
  nchar(stringr::str_replace_all(paste(abs(x)), "[0\\.]",""))
}

test_that("Options digits is served",{
  options(digits = 4)
  cf = coef_by_group(fit)
  expect_s3_class(cf, "tbl_df")
  expect_lte(digs(cf[[1,"estimate"]]) ,4L)
})


test_that("Fit must be of class breathtestfit",{
  expect_error(coef_by_group(NULL))
})

test_that("nlme_fit can be used to compute coefficients for multiple records in one group",{
  data = cleanup_data(simulate_breathtest_data(4))
  fit = nlme_fit(data)
  cf = coef_by_group(fit)
  expect_s3_class(cf, "tbl_df")
  expect_identical(ncol(cf), 7L)
  expect_equal(unique(cf$group), "A")
  expect_equal(unique(cf$diff_group), "a")
  expect_equal(names(cf), c("parameter", "method", "group", "estimate", "conf.low", 
                            "conf.high", "diff_group"))
})

test_that("nlme_fit can be used to compute coefficients for multiple groups",{
  skip_on_cran() # Does no converge on some CRAN settings
  fit = nlme_fit(data)
  cf = coef_by_group(fit)
  expect_s3_class(cf, "tbl_df")
  expect_identical(ncol(cf), 7L)
  expect_equal(names(cf), c("parameter", "method", "group", "estimate", "conf.low", 
                            "conf.high", "diff_group"))
})

test_that("Fit of a single curve returns valid data", {
  data = usz_13c %>%
    dplyr::filter( patient_id == "pat_001") %>% 
    cleanup_data()
  comment(data) = "comment"
  options(digits = 4)
  fit = nls_fit(data)
  cf = coef_by_group(fit)
  expect_identical(ncol(cf), 7L)
  expect_identical(nrow(cf), 9L)
  expect_equal(cf$conf.low, rep(NA, 9L))
  expect_equal(cf$conf.high, rep(NA, 9L))
  expect_equal(comment(cf), "comment")
  expect_lte(digs(cf[[1,"estimate"]]) ,4L)
  
})

